# Preview layer: immutable approved compact counts + explicit public context.
# Published counts stay untouched; D13 permits an explicitly labelled small-rate assumption.
.participation_archive_sha256 <- "b1409d95e8d8b9ebd7d29797a3d4433fe4b5d77c89e159cc1509a0b1c7153fc9"

.participation_profile <- function(refresh=FALSE) {
  if(!refresh && !is.null(.reactextract_env$participation)) return(.reactextract_env$participation)
  archive <- system.file("extdata","participation-profile.tar.gz",package="reactextract")
  if(!nzchar(archive) || !identical(.sha256_file(archive),.participation_archive_sha256))
    stop("Participation profile archive checksum mismatch.",call.=FALSE)
  path <- file.path(tempdir(),paste0("reactextract-participation-",.participation_archive_sha256))
  if(!file.exists(file.path(path,"checksums.csv"))) {
    dir.create(path,recursive=TRUE,showWarnings=FALSE)
    utils::untar(archive,exdir=path)
  }
  hashes <- .read_literal_csv(file.path(path,"checksums.csv"))
  for(i in seq_len(nrow(hashes))) {
    f <- hashes$file[i]
    if(grepl("(^/|[.][.]|\\\\)",f) || !identical(.sha256_file(file.path(path,f)),hashes$sha256[i]))
      stop("Participation profile file checksum mismatch.",call.=FALSE)
  }
  read <- function(f) .read_literal_csv(file.path(path,f))
  a <- read("approval.csv"); a <- stats::setNames(a$value,a$key)
  m <- read("candidate/manifest.csv"); m <- stats::setNames(m$value,m$key)
  if(a[["status"]]!="formally_approved" ||
     !identical(a[["candidate_manifest_sha256"]],.sha256_file(file.path(path,"candidate/manifest.csv"))) ||
     !identical(a[["candidate_checksums_sha256"]],.sha256_file(file.path(path,"candidate/checksums.csv"))) ||
     !identical(m[["dictionary_manifest_sha256"]],react_dictionary_version()$manifest_sha256[[1]]))
    stop("Participation approval/dictionary mismatch.",call.=FALSE)
  p <- list(counts=read("candidate/counts.csv"),domains=read("candidate/domains.csv"),
    plan=read("candidate/plan.csv"),availability=read("candidate/availability.csv"),
    occurrences=read("contract/occurrence_participation.csv"),
    age_bins=read("contract/age_context.csv"),approval=a,metadata=m)
  p$counts$count <- as.numeric(p$counts$count)
  p$counts$suppressed <- as.logical(p$counts$suppressed)
  if(any(is.na(p$counts$count)!=p$counts$suppressed) ||
     any(!is.na(p$counts$count)&(p$counts$count<10|p$counts$count%%5!=0)))
    stop("Invalid participation protected counts.",call.=FALSE)
  p$count_index <- split(seq_len(nrow(p$counts)),p$counts$field)
  p$domain_index <- split(seq_len(nrow(p$domains)),p$domains$occurrence_id)
  .reactextract_env$participation <- p
  p
}

.has_participation <- function(source) !is.null(source$profile$participation)

.participation_overlay <- function(profile,refresh=FALSE) {
  profile$participation <- .participation_profile(refresh)
  profile$eligibility <- .eligibility_context_contract(refresh)
  profile$response_options <- .response_options_contract(refresh)
  profile$dependency_models <- .read_dependency_models()
  replace <- c("profile_release","profile_version","participation_status","participation_sha256", "eligibility_sha256", "response_options_sha256", "participation_rate_policy")
  profile$metadata <- profile$metadata[!profile$metadata$key %in% replace,,drop=FALSE]
  profile$metadata <- rbind(profile$metadata,data.frame(key=replace,value=c(
    "react-synthetic-profile-v6-preview","participation-v6-preview-6-dependency-models-v1",
    "approved_aggregates_preview_model",.participation_archive_sha256,.synthetic_rules_sha256,.synthetic_rules_sha256,
    .participation_rate_policy)))
  profile$metadata <- rbind(profile$metadata,data.frame(key="dependency_models_sha256",value=.dependency_models_sha256))
  profile$metadata <- profile$metadata[profile$metadata$key!="profile_sha256",,drop=FALSE]
  profile$metadata <- rbind(profile$metadata,data.frame(key="profile_sha256",value=.profile_object_sha256(profile)))
  profile
}

.participation_age_band <- function(age) {
  as.character(cut(age,c(0,5,12,13,16,18,55,121),right=FALSE,
    labels=c("under_5","5_11","12","13_15","16_17","18_54","55_plus")))
}

.participation_context <- function(source,round,n,dictionary) {
  p <- source$profile$participation
  o <- dictionary$occurrences[dictionary$occurrences$round_id==round & dictionary$occurrences$variable=="U_AGE",,drop=FALSE]
  age <- if(nrow(o)) .participation_value(source,o,n,dictionary)$value else rep(NA_real_,n)
  rows <- p$counts[p$count_index[["individual_participation"]],,drop=FALSE]
  rows <- rows[rows$round_id==round,,drop=FALSE]
  rate <- .participation_rate(source, round)
  nonresponse <- rep(FALSE,n)
  if(rate$status != "unestimated") nonresponse <- .with_stream_seed(source$seed,round,"participation::individual",{
    # Retain the existing order/probabilities for released REACT-1 rates.
    weights <- if (rate$status == "estimated") rows$count/sum(rows$count) else
      ifelse(rows$state == "shared_nonresponse", rate$probability, 1-rate$probability)
    .sample_dependency_values(rows$state,n,weights)=="shared_nonresponse"
  })
  # FALSE above means no overwrite is imposed, NOT an estimated zero rate.
  list(age=age,band=.participation_age_band(age),nonresponse=nonresponse,
    status=rate$status, rate=rate)
}

.participation_bins <- function(p,source,occurrence,dictionary) {
  if(occurrence$variable %in% c("U_AGE","AGE")) {
    b <- p$age_bins
    return(data.frame(bin_id=b$bin_id,lower=b$lower,upper=b$upper_exclusive,
      boundary_rules="lower_inclusive_upper_exclusive",stringsAsFactors=FALSE))
  }
  s <- source$profile$profile_specs
  spec <- s[match(occurrence$occurrence_id,s$occurrence_id),,drop=FALSE]
  source$profile$safe_bins[source$profile$safe_bins$bin_spec_id==spec$bin_spec_id,,drop=FALSE]
}

.participation_value <- function(source,occurrence,n,dictionary,context=NULL,eligible=rep(TRUE,n)) {
  if (length(eligible) != n || anyNA(eligible)) stop("Invalid option eligibility mask.", call. = FALSE)
  p <- source$profile$participation; id <- occurrence$occurrence_id[[1]]
  di <- p$domain_index[[id]]; ci <- p$count_index[[id]]
  reason <- rep("",n); issue <- .empty_issues()
  if(is.null(di)||is.null(ci)) {
    # Excluded/identifier fields retain the existing privacy-aware generator.
    z <- .generate_synthetic_value(source,occurrence,sum(eligible))
    value <- z$value[rep(NA_integer_, n)]; value[eligible] <- z$value
    z$value <- value; reason[!eligible] <- "structural_skip_option"
    return(c(z,list(reason=reason)))
  }
  dom <- p$domains[di,,drop=FALSE]; cc <- p$counts[ci,,drop=FALSE]
  bins <- .participation_bins(p,source,occurrence,dictionary)
  age_field <- occurrence$variable[[1]]=="AGE"
  groups <- if(age_field && !is.null(context)) ifelse(is.na(context$band),"missing_or_invalid",context$band) else rep("all",n)
  states <- rep("database_missing",n)
  fallback <- 0L
  .with_stream_seed(source$seed,occurrence$round_id[[1]],paste0(id,"::participation_values"),{
    for(g in sort(unique(groups))) {
      positions <- which(groups==g & eligible)
      if (!length(positions)) next
      rows <- cc[cc$reference_state==g,,drop=FALSE]
      counts <- rows$count[match(dom$state,rows$state)]
      permitted <- rep(TRUE,nrow(dom))
      public <- dom$substantive=="TRUE"
      if(occurrence$variable[[1]] %in% c("AGE","U_AGE")) {
        # Do not introduce out-of-study ages through the public prior.
        lower <- as.numeric(p$age_bins$lower[match(sub("^bin:","",dom$state),p$age_bins$bin_id)])
        permitted[public] <- lower[public]>=if(startsWith(occurrence$round_id[[1]],"react1.")) 5 else 18
        if(age_field) permitted[public] <- permitted[public] &
          p$age_bins$band[match(sub("^bin:","",dom$state[public]),p$age_bins$bin_id)]==g
      }
      if(occurrence$variable[[1]]=="SFREPORTFIG") permitted[dom$state=="coded:-77"]<-FALSE
      permitted[is.na(permitted)]<-FALSE
      prior <- permitted & public
      released <- permitted & !is.na(counts) & counts>0
      # No empirical estimate exists for hidden missing codes. The prior only
      # populates valid ANSWERS, never independently invents shared -77 events.
      if(any(released)) {
        prob <- numeric(length(counts));prob[released]<-counts[released]/sum(counts[released])
        if(any(prior)) prob <- (1-source$safe_prior_fraction)*prob + source$safe_prior_fraction*prior/sum(prior)
      } else if(any(prior) && !age_field) {
        prob <- prior/sum(prior);fallback<-fallback+length(positions)
      } else {fallback<-fallback+length(positions);next}
      states[positions] <- .sample_dependency_values(dom$state,length(positions),prob)
    }
  })
  value <- rep(NA_character_,n)
  code <- startsWith(states,"code:");value[code]<-sub("^code:","",states[code])
  coded <- startsWith(states,"coded:");value[coded]<-sub("^coded:","",states[coded])
  value[states=="text_present"]<-"Synthetic response";value[states=="empty_text"]<-""
  bin_states <- unique(states[startsWith(states,"bin:")])
  if(length(bin_states)) .with_stream_seed(source$seed,occurrence$round_id[[1]],paste0(id,"::within_bin"),{
    spec <- source$profile$profile_specs
    kind <- spec$profile_kind[match(id,spec$occurrence_id)]
    date <- identical(kind,"date") || occurrence$data_type[[1]]=="DATE" || startsWith(occurrence$data_type[[1]],"TIMESTAMP")
    for(state in sort(bin_states)) {
      b <- match(sub("^bin:","",state),bins$bin_id);positions<-which(states==state)
      if(is.na(b)) next
      lo <- if(date) as.numeric(as.Date(bins$lower[b])) else as.numeric(bins$lower[b])
      hi <- if(date) as.numeric(as.Date(bins$upper[b])) else as.numeric(bins$upper[b])
      if(!is.finite(lo)||!is.finite(hi)) next
      boundary <- .safe_bin_boundary_rule(bins,b)
      lx<-boundary %in% c("lower_exclusive_upper_inclusive","exclusive")
      ux<-boundary %in% c("lower_inclusive_upper_exclusive","exclusive")
      if(kind=="continuous" && !occurrence$variable[[1]] %in% c("AGE","U_AGE") && !date) {
        v<-stats::runif(length(positions),lo,hi)
        if(lx)v[v<=lo]<-lo+(hi-lo)/2
      } else {
        low<-if(lx)floor(lo)+1 else ceiling(lo); high<-if(ux)ceiling(hi)-1 else floor(hi)
        if(low>high) next
        v<-.sample_dependency_values(seq.int(low,high),length(positions))
      }
      value[positions]<-if(date && occurrence$data_type[[1]]!="NUMBER") as.character(as.Date(v,origin="1970-01-01")) else as.character(v)
    }
  })
  reason[states=="coded:-77"]<-"unresolved_source_code_77"
  reason[states=="coded:-91"]<-"not_applicable"
  reason[states=="coded:-92"]<-"item_nonresponse"
  reason[states=="outside_public_support"]<-"outside_public_support"
  reason[!eligible] <- "structural_skip_option"
  if(fallback) issue<-.issue("warning","synthetic","participation_public_fallback",
    "Conditional counts unavailable: public answer support or missing values used; no hidden count estimated.",occurrence$round_id[[1]],"synthetic",occurrence$variable[[1]],fallback)
  if(occurrence$data_type[[1]]=="DATE" || startsWith(occurrence$data_type[[1]],"TIMESTAMP"))value[coded]<-NA_character_
  value <- .coerce_synthetic_value(value,occurrence$data_type[[1]])
  list(value=value,issues=issue,reason=reason)
}

.participation_enforce <- function(data,occurrences,source,context,reasons) {
  p<-source$profile$participation$occurrences
  p<-p[match(occurrences$occurrence_id,p$occurrence_id),,drop=FALSE]
  for(i in seq_len(nrow(occurrences))) {
    variable<-occurrences$variable[i]
    if(!variable %in% names(data)||is.na(p$occurrence_id[i])) next
    if(is.null(reasons[[variable]]))reasons[[variable]]<-rep("",nrow(data))
    if(nzchar(p$minimum_age[i])) {
      ineligible<-is.na(context$age)|context$age<as.numeric(p$minimum_age[i])
      # No verified universal skip code: preserve a missing value + reason.
      data[[variable]][ineligible]<-NA
      reasons[[variable]][ineligible]<-"structural_skip_age"
    }
    shared <- (p$stage[i]=="individual" && p$governed[i]=="TRUE") || variable=="SFREPORTFIG"
    if(shared) {
      hit<-context$nonresponse
      code<-if(inherits(data[[variable]],c("Date","POSIXt"))) NA else if(is.numeric(data[[variable]])) -77 else "-77"
      data[[variable]][hit]<-code
      reasons[[variable]][hit]<-"survey_nonresponse"
    }
  }
  list(data=data,reasons=reasons)
}

.read_participation_round <- function(source,registry_row,occurrences,dictionary) {
  round<-registry_row$round_id[[1]]; n<-source$n_per_round[[round]]
  context<-.participation_context(source,round,n,dictionary)
  tag<-toupper(gsub("[.]","-",round));key<-registry_row$observation_key[[1]]
  data<-data.frame(x=sprintf("SYN-%s-%07d",tag,seq_len(n)),SUBJECT_ID=sprintf("SYN-SUBJECT-%s-%07d",tag,seq_len(n)))
  names(data)[1]<-key;issues<-list();reasons<-list()
  option_contract <- source$profile$response_options
  restricted <- if (is.null(option_contract)) rep(FALSE, nrow(occurrences)) else
    occurrences$occurrence_id %in% option_contract$targets$target_occurrence_id
  # Generate context parents first. Each occurrence keeps its own random stream.
  for(i in order(restricted)) {
    o<-occurrences[i,,drop=FALSE];v<-o$variable[[1]]
    if(v %in% c(key,"SUBJECT_ID")) next
    eligible <- if (restricted[i]) .response_option_eligible(data, o$occurrence_id, occurrences, context, option_contract) else rep(TRUE,n)
    z<-.participation_value(source,o,n,dictionary,context,eligible)
    data[[v]]<-if(v=="U_AGE")context$age else z$value
    reasons[[v]]<-z$reason;issues[[length(issues)+1L]]<-z$issues
  }
  # Preserve v5 relationships only for independently sourced fields. Restoring
  # survey columns prevents stale unconditional writers changing new profiles.
  base<-data
  if(!.has_dependency_models(source)) data<-.apply_synthetic_dependencies(data,occurrences,source,dictionary)
  p<-source$profile$participation$occurrences
  independent<-p$variable[p$round_id==round & p$source_kind %in% c("sampling_or_linked","laboratory_or_processing")]
  restore<-setdiff(names(base),setdiff(independent,c("U_AGE","AGE")))
  data[restore]<-base[restore]
  locked<-.participation_enforce(data,occurrences,source,context,reasons)
  if (!is.null(source$profile$eligibility)) {
    routed <- .apply_eligibility_context(locked$data, occurrences, dictionary, context,
      source$profile$eligibility, return_skips = TRUE, options = option_contract)
    data <- routed$data
    for (v in names(routed$skipped)) {
      if (is.null(locked$reasons[[v]])) locked$reasons[[v]] <- rep("", n)
      locked$reasons[[v]][routed$skipped[[v]]] <- "structural_skip_questionnaire"
      locked$reasons[[v]][routed$option_skipped[[v]]] <- "structural_skip_option"
    }
  } else {
    data<-.apply_synthetic_routing(locked$data,occurrences,dictionary,strict_missing=TRUE)
    for(v in intersect(names(data),names(locked$reasons))) {
      changed<-!is.na(locked$data[[v]]) & (is.na(data[[v]])|as.character(data[[v]])!=as.character(locked$data[[v]]))
      changed[is.na(changed)]<-FALSE
      locked$reasons[[v]][changed]<-"structural_skip_questionnaire"
    }
  }
  if (is.null(source$profile$eligibility) && !is.null(option_contract)) {
    option_locked <- .apply_response_options(data, occurrences, context, option_contract, locked$reasons)
    data <- option_locked$data; locked$reasons <- option_locked$reasons
  }
  locked<-.participation_enforce(data,occurrences,source,context,locked$reasons)
  data<-.synchronise_synthetic_age_group(locked$data,occurrences,dictionary)
  model_summary<-NULL
  if(.has_dependency_models(source)) {
    model<-.apply_model_generation(data,occurrences,source,context,dictionary,locked$reasons)
    data<-model$data;locked$reasons<-model$reasons;model_summary<-model$summary
    issues[[length(issues)+1L]]<-.issue("warning","synthetic","dependency_model_limits",
      paste0("Approved outcome models and conditional tables applied. Suppressed distributions retain baseline fallbacks; age/shared participation remain anchored to the prior approved context. ",model_summary),round)
  }
  if(context$status=="unestimated")issues[[length(issues)+1L]]<-.issue("warning","synthetic","participation_unestimated",
    "Shared participation rate unavailable: no shared overwrite inferred; this is not a zero non-response estimate.",round)
  if(context$status=="assumed_below_10")issues[[length(issues)+1L]]<-.issue("warning","synthetic","participation_rate_assumed",
    paste0("Shared nonresponse is explicitly below 10: using an assumed count of 5 divided by the approved rounded round total (",
      context$rate$denominator, "). This is a modelling assumption, not a recovered count or survey-completion rate; see manifest."),round)
  issues[[length(issues)+1L]]<-.issue("warning","synthetic","participation_preview_limits",
    if(.has_dependency_models(source)) "Preview: approved dependency models are active with recorded fallbacks and questionnaire guards; held context/option cases and general multivariate correlations remain outside this release." else "Preview: approved question context and 109 option restrictions are applied; held cases, other options and participation-conditioned outcome relationships remain incomplete.",round)
  list(data=data,source_object=paste0("synthetic:",round),issues=.bind_rows(issues,.empty_issues()),missing_reasons=locked$reasons,model_summary=model_summary,
    participation_summary=paste0(round,":",context$status,":shared=",
      if(context$status!="unestimated")sum(context$nonresponse) else "unestimated",
      ":age_unavailable=",sum(is.na(context$band))))
}

.participation_harmonised_reasons <- function(h,observations,reasons) {
  if(is.null(reasons)||!nrow(h))return(h)
  groups<-split(seq_len(nrow(h)),h$source_raw_variable)
  for(v in names(groups)) {
    vars<-strsplit(v,"|",fixed=TRUE)[[1]]
    if(!all(vars %in% names(reasons)))next
    idx<-groups[[v]]; rows<-match(h$observation_id[idx],observations$observation_id)
    rr<-lapply(reasons[vars],function(x)x[rows])
    same<-Reduce(`&`,lapply(rr,function(x)x==rr[[1]])) & nzchar(rr[[1]])
    # Do not replace a valid grouped answer merely because one input is missing.
    same<-same & nzchar(h$missing_reason[idx]);same[is.na(same)]<-FALSE
    h$missing_reason[idx[same]]<-rr[[1]][same]
  }
  h
}
