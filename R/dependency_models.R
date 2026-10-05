# Approved dependency-model inputs. Kept separate from v5 and the participation
# preview until raw-field encoding and end-to-end equivalence tests are complete.
.dependency_models_sha256 <- "37c83acbfab4a4bf86a09755f6339648636b7f55357cd0ca7a8e0e2d892ad698"

.validate_dependency_models <- function(x) {
  expected <- c(sprintf("react1.r%02d",1:19),sprintf("react2.r%02d",1:6))
  if (!identical(x$schema,"dependency-release-selection-v1") ||
      anyDuplicated(names(x$rounds)) || !setequal(names(x$rounds),expected))
    stop("Dependency model schema or round coverage mismatch.",call.=FALSE)
  for (r in expected) {
    z <- x$rounds[[r]]; m <- z$model
    if (!identical(m$kind,"ridge_logistic_v1") || !is.list(m$levels) ||
        anyDuplicated(names(m$levels)) || is.null(names(m$levels))) stop("Invalid dependency model levels.")
    if (any(!vapply(m$levels,function(v) is.character(v) && !anyNA(v) && !anyDuplicated(v) && length(v)>=2L,logical(1))))
      stop("Invalid factor support.")
    columns <- c("(Intercept)",unlist(lapply(names(m$levels),function(f) paste(f,m$levels[[f]][-1],sep="=")),use.names=FALSE))
    if (!identical(names(m$coefficients),columns) || !is.numeric(m$coefficients) ||
        any(!is.finite(m$coefficients)) || !identical(m$outcome,"positive_among_evaluable")) stop("Invalid model coefficients.")
    for (spec in c(z$context_counts,z$symptom_counts,z$ct_counts)) {
      t <- spec$table
      if (!identical(names(t),c(spec$parents,spec$target,"count","suppressed")) ||
          !is.numeric(t$count) || !is.logical(t$suppressed) || anyNA(t$suppressed) ||
          any(is.na(t$count)!=t$suppressed) ||
          any(!is.na(t$count) & (!is.finite(t$count) | t$count<10 | t$count%%5!=0)))
        stop("Invalid protected dependency counts.")
      if (anyNA(t[c(spec$parents,spec$target)]) || anyDuplicated(t[c(spec$parents,spec$target)]))
        stop("Invalid count support.")
    }
  }
  invisible(TRUE)
}

.read_dependency_models <- function(path = system.file("extdata","dependency-models-v1.rds",package="reactextract")) {
  if (!nzchar(path) || !file.exists(path) || !identical(.sha256_file(path),.dependency_models_sha256))
    stop("Approved dependency-model checksum mismatch.",call.=FALSE)
  approval_path <- system.file("extdata","dependency-models-v1-approval.csv",package="reactextract")
  approval <- .read_literal_csv(approval_path)
  if(nrow(approval)!=1L || !identical(approval$sha256,.dependency_models_sha256) ||
      !identical(approval$approval_status,"formally_approved")) stop("Dependency-model approval record mismatch.",call.=FALSE)
  x <- readRDS(path)
  .validate_dependency_models(x)
  x
}

.dependency_model_probability <- function(model, states) {
  n <- nrow(states); eta <- rep(unname(model$coefficients[["(Intercept)"]]),n)
  unknown <- integer(n)
  for (field in names(model$levels)) {
    if (!field %in% names(states)) stop("Missing model context: ",field,call.=FALSE)
    value <- as.character(states[[field]])
    bad <- is.na(value) | !value %in% model$levels[[field]]
    if (any(bad) && !"model_unknown" %in% model$levels[[field]]) stop("No unknown-state support.")
    value[bad] <- "model_unknown"; unknown <- unknown+as.integer(bad)
    b <- c(0,unname(model$coefficients[paste(field,model$levels[[field]][-1],sep="=")]))
    eta <- eta+b[match(value,model$levels[[field]])]
  }
  list(probability=stats::plogis(eta),unknown_predictors=unknown)
}

# Only a complete, released conditional distribution may be used. The caller
# supplies a safe existing fallback per row. Suppressed cells are never assigned
# zero or reconstructed from other released counts. No RNG state escapes.
.sample_model_counts <- function(spec, states, fallback, public_domain, seed, round, stream) {
  n <- nrow(states)
  if (length(fallback)!=n || any(!fallback %in% public_domain) || anyNA(fallback)) stop("Invalid safe fallback.")
  if (!all(spec$parents %in% names(states))) stop("Missing conditional context.")
  result <- as.character(fallback); used <- rep(FALSE,n)
  t <- spec$table
  key <- function(d) if (!length(spec$parents)) rep("all",nrow(d)) else
    do.call(paste,c(lapply(d[spec$parents],as.character),sep="\r"))
  have <- key(t); wanted <- key(states)
  .with_stream_seed(seed,round,stream,{
    for (g in unique(wanted)) {
      j <- which(have==g); i <- which(wanted==g)
      if (!length(j) || any(t$suppressed[j]) || anyNA(t$count[j]) ||
          any(!as.character(t[[spec$target]][j]) %in% public_domain)) next
      probability <- t$count[j]/sum(t$count[j])
      # Public-domain prior does not use hidden counts or hidden support.
      p <- rep(.01/length(public_domain),length(public_domain))
      p[match(as.character(t[[spec$target]][j]),public_domain)] <-
        p[match(as.character(t[[spec$target]][j]),public_domain)]+.99*probability
      result[i] <- sample(public_domain,length(i),replace=TRUE,prob=p)
      used[i] <- TRUE
    }
  })
  list(value=result,used_released_distribution=used)
}

.has_dependency_models <- function(source) !is.null(source$profile$dependency_models)

.model_lock <- function(data, occurrences, source, context, dictionary, reasons=list()) {
  # Parent answers may have changed since the previous pass. Recompute skip
  # reasons; an old closed branch must not be labelled structurally closed after
  # it becomes eligible (its missing answer may remain missing).
  for(v in intersect(names(reasons),occurrences$variable)) reasons[[v]][startsWith(reasons[[v]],"structural_skip")]<-""
  z <- .participation_enforce(data,occurrences,source,context,reasons)
  routed <- .apply_eligibility_context(z$data,occurrences,dictionary,context,
    source$profile$eligibility,return_skips=TRUE,options=source$profile$response_options)
  for(v in names(routed$skipped)) {
    if(is.null(z$reasons[[v]])) z$reasons[[v]]<-rep("",nrow(data))
    z$reasons[[v]][routed$skipped[[v]]]<-"structural_skip_questionnaire"
    z$reasons[[v]][routed$option_skipped[[v]]]<-"structural_skip_option"
  }
  .participation_enforce(routed$data,occurrences,source,context,z$reasons)
}

.model_vaccine_fields <- function(data) {
  suffix <- if("VACCINE3SYM" %in% names(data)) "SYM" else ""
  c(status=paste0("VACCINE3",suffix),dose=paste0("VACCDOSE",suffix))
}
.model_dose_map <- function(field, occurrences, dictionary) {
  ids<-occurrences$occurrence_id[occurrences$variable==field]
  opt<-unique(dictionary$response_options[dictionary$response_options$occurrence_id %in% ids,c("return_value","display_value")])
  labels<-tolower(trimws(opt$display_value))
  map<-c(one="one",two="two",three="three",`more than two`="three_plus",`more than three`="four_plus")
  value<-unname(map[labels]);value[is.na(value)]<-"unreported"
  stats::setNames(value,opt$return_value)
}
.model_states <- function(data, occurrences, source, context, dictionary, reasons) {
  round<-unique(occurrences$round_id);n<-nrow(data)
  pp<-source$profile$participation$occurrences
  pp<-pp[pp$round_id==round,,drop=FALSE]
  answer<-function(stage) {
    fields<-intersect(pp$variable[pp$stage==stage],names(data))
    out<-rep(FALSE,n)
    for(v in fields) out<-out | (!is.na(data[[v]]) & !.dependency_admin_value(data[[v]]) & as.character(data[[v]])!="")
    out
  }
  individual<-answer("individual")
  detail<-ifelse(individual,"response_evidenced","undetermined")
  if(startsWith(round,"react1") && "SFREPORTFIG" %in% names(data)) {
    flag<-as.character(data$SFREPORTFIG)
    detail[which(flag=="1")]<-"recorded_complete"
    detail[which(flag=="0")]<-"recorded_breakoff"
    detail[which(flag=="-555" & !individual)]<-"source_555"
    detail[which(flag=="-77" & individual)]<-"discordant_flag_and_answer"
  }
  detail[context$nonresponse]<-"shared_nonresponse"
  states<-data.frame(participation=detail,registration=ifelse(answer("registration"),"response_evidenced","undetermined"))
  mask<-function(value,fields) {
    po<-pp[pp$variable %in% fields,,drop=FALSE]
    if(nrow(po) && all(po$stage=="individual" & po$governed=="TRUE")) value[context$nonresponse]<-"survey_nonresponse"
    if(length(fields) && all(fields %in% names(reasons))) {
      skip<-Reduce(`&`,lapply(reasons[fields],function(x) startsWith(x,"structural_skip")))
      value[skip & value!="survey_nonresponse"]<-"structural_skip"
    }
    value[is.na(value)]<-"missing";value
  }
  specs<-.dependency_specs(dictionary,round)
  for(i in seq_len(nrow(specs))) {
    s<-specs[i,,drop=FALSE]
    if(s$predictor_id=="classic_symptom_status") next
    if(s$predictor_id=="vaccination_status") {
      field<-.model_vaccine_fields(data)[["status"]]
      if(field %in% names(data)) s$source_fields<-field
    }
    fields<-.dependency_occurrences(s,dictionary)$variable
    states[[s$predictor_id]]<-mask(.dependency_predictor_state(data,s,dictionary),fields)
  }
  vf<-.model_vaccine_fields(data)
  if(vf[["status"]] %in% names(data) && "vaccination_status" %in% names(states)) {
    dose<-rep("unreported",n)
    if(vf[["dose"]] %in% names(data)) {
      map<-.model_dose_map(vf[["dose"]],occurrences,dictionary)
      dose<-unname(map[as.character(data[[vf[["dose"]]]])]);dose[is.na(dose)]<-"unreported"
    }
    dose[states$vaccination_status=="no" & dose=="unreported"]<-"none"
    dose[states$vaccination_status=="no" & dose %in% c("one","two","three","three_plus","four_plus")]<-"discordant"
    states$vaccine_dose<-mask(dose,vf[["dose"]])
  }
  states
}

.apply_model_generation <- function(data,occurrences,source,context,dictionary,reasons) {
  round<-unique(occurrences$round_id);n<-nrow(data)
  bundle<-source$profile$dependency_models$rounds[[round]]
  locked<-.model_lock(data,occurrences,source,context,dictionary,reasons)
  data<-locked$data;reasons<-locked$reasons
  stats<-c(context_fallback=0L,context_encoding_mismatch=0L,outcome_availability_fallback=0L,
    symptom_fallback=0L,ct_fallback=0L,ct_consistency_fallback=0L,outcome_guarded=0L)
  specs<-.dependency_specs(dictionary,round)
  # Age and shared participation are anchored by the existing approved context.
  model_fields<-unique(unlist(lapply(seq_len(nrow(specs)),function(i)
    .dependency_occurrences(specs[i,,drop=FALSE],dictionary)$variable),use.names=FALSE))
  model_fields<-union(model_fields,unname(.model_vaccine_fields(data)))
  model_occ<-.eligibility_required_occurrences(dictionary,
    occurrences[occurrences$variable %in% model_fields,,drop=FALSE],
    source$profile$eligibility,source$profile$response_options)
  # Their conditional count tables do not override that separately reviewed model.
  for(target in setdiff(names(bundle$context_counts),c("age_band","participation","registration","outcome_availability"))) {
    states<-.model_states(data,occurrences,source,context,dictionary,reasons)
    if(!target %in% names(states)) next
    s<-specs[specs$predictor_id==target,,drop=FALSE]
    if(target=="vaccine_dose") {
      field<-.model_vaccine_fields(data)[["dose"]]
      if(!field %in% names(data)) next
      fields<-field;map<-.model_dose_map(field,occurrences,dictionary)
      domain<-unique(c(unname(map),"none","discordant","missing","survey_nonresponse","structural_skip"))
    } else {
      if(!nrow(s)) next
      if(target=="vaccination_status") {
        field<-.model_vaccine_fields(data)[["status"]]
        if(field %in% names(data)) s$source_fields<-field
      }
      fields<-.dependency_occurrences(s,dictionary)$variable
      domain<-unique(c(.dependency_predictor_levels(s,dictionary),"missing","survey_nonresponse","structural_skip"))
    }
    domain<-union(domain,unique(states[[target]]))
    sample<-.sample_model_counts(bundle$context_counts[[target]],states,states[[target]],domain,
      source$seed,round,paste0("model::context::",target))
    stats["context_fallback"]<-stats["context_fallback"]+sum(!sample$used_released_distribution)
    before<-data
    if(target=="vaccine_dose") {
      value<-rep(NA_real_,n)
      for(level in unique(sample$value)) {
        support<-names(map)[map==level & map!="unreported"]
        if(length(support)) value[sample$value==level]<-.with_stream_seed(source$seed,round,paste0("model::dose::",level),
          as.numeric(.sample_dependency_values(support,sum(sample$value==level))))
      }
      data[[field]]<-value
    } else data<-.set_dependency_predictor(data,sample$value,s,dictionary,source)
    # Retain exact original missing codes on unchanged states, including fallback
    # rows. This also avoids converting ordinary item-missingness into zeros.
    unchanged<-sample$value==states[[target]] | !sample$used_released_distribution
    for(v in intersect(fields,names(data))) {
      data[[v]][unchanged]<-before[[v]][unchanged]
      if(!is.null(reasons[[v]])) reasons[[v]][!unchanged]<-""
    }
    locked<-.model_lock(data,model_occ,source,context,dictionary,reasons)
    data<-locked$data;reasons<-locked$reasons
    actual<-.model_states(data,occurrences,source,context,dictionary,reasons)[[target]]
    mismatch<-actual!=sample$value
    stats["context_encoding_mismatch"]<-stats["context_encoding_mismatch"]+sum(mismatch)
    if(any(mismatch)) {
      for(v in intersect(fields,names(data))) data[[v]][mismatch]<-before[[v]][mismatch]
      locked<-.model_lock(data,model_occ,source,context,dictionary,reasons)
      data<-locked$data;reasons<-locked$reasons
    }
  }
  locked<-.model_lock(data,occurrences,source,context,dictionary,reasons)
  data<-locked$data;reasons<-locked$reasons
  states<-.model_states(data,occurrences,source,context,dictionary,reasons)
  oid<-unique(specs$outcome_id)
  fallback<-.sample_dependency_outcome(source,round,oid,n)
  availability<-ifelse(fallback %in% c("positive","negative"),"evaluable",fallback)
  av<-.sample_model_counts(bundle$context_counts$outcome_availability,states,availability,
    if(startsWith(round,"react1")) c("evaluable","missing") else c("evaluable","non_evaluable","missing"),
    source$seed,round,"model::outcome_availability")
  stats["outcome_availability_fallback"]<-sum(!av$used_released_distribution)
  pr<-.dependency_model_probability(bundle$model,states)
  outcome<-av$value
  hit<-outcome=="evaluable"
  draws<-.with_stream_seed(source$seed,round,"model::outcome",stats::runif(n))
  outcome[hit]<-ifelse(draws[hit]<pr$probability[hit],"positive","negative")
  # Generate the REACT-2 result and test-completion answers jointly. These are
  # fictional candidate answers, not observed responses to preserve. Mandatory
  # questionnaire guards are reapplied below and can veto this branch.
  if(startsWith(round,"react2")) {
    can_test<-!context$nonresponse
    stats["outcome_guarded"]<-sum(!can_test & outcome!="missing")
    outcome[!can_test]<-"missing"
  }
  data<-.set_dependency_outcome(data,outcome,round,oid,source,dictionary)
  for(v in intersect(c("RESULT","FINALRESULT","NEWRESULT","NEWRESULT_2","ABATTEMPT","ABCOMP"),names(data)))
    if(!is.null(reasons[[v]])) reasons[[v]][!.dependency_admin_value(data[[v]])]<-""
  safe_outcome<-data
  # Conditional Ct bins retain zero as a point mass. Marginal draws that violate
  # the reviewed PCR definition fall back to the outcome-consistent encoder.
  for(v in intersect(names(bundle$ct_counts),names(data))) {
    x<-as.numeric(data[[v]]);state<-rep("missing_or_outside_support",n)
    state[!is.na(x)&x==0]<-"zero"
    bins<-cut(x,c(0,10,20,30,40,50,60),labels=c("over0_to10","over10_to20","over20_to30","over30_to40","over40_to50","over50_to60"))
    state[!is.na(bins)]<-as.character(bins[!is.na(bins)])
    domain<-c("zero","over0_to10","over10_to20","over20_to30","over30_to40","over40_to50","over50_to60","missing_or_outside_support")
    cs<-.sample_model_counts(bundle$ct_counts[[v]],data.frame(outcome=outcome),state,domain,source$seed,round,paste0("model::ct::",v))
    stats["ct_fallback"]<-stats["ct_fallback"]+sum(!cs$used_released_distribution)
    value<-rep(NA_real_,n);value[cs$value=="zero"]<-0
    u<-.with_stream_seed(source$seed,round,paste0("model::ct_value::",v),stats::runif(n,min=.001,max=1))
    for(k in 1:6) {ix<-cs$value==domain[k+1L];value[ix]<-round((k-1)*10+u[ix]*10,3)}
    value[!cs$used_released_distribution]<-data[[v]][!cs$used_released_distribution]
    data[[v]]<-value
  }
  actual<-.dependency_outcome_state(data,round,oid,dictionary)
  bad<-actual!=outcome;bad[is.na(bad)]<-TRUE
  stats["ct_consistency_fallback"]<-sum(bad)
  for(v in intersect(names(bundle$ct_counts),names(data))) data[[v]][bad]<-safe_outcome[[v]][bad]
  states$outcome<-.dependency_outcome_state(data,round,oid,dictionary)
  for(id in names(bundle$symptom_counts)) {
    v<-occurrences$variable[match(id,occurrences$occurrence_id)]
    if(is.na(v)||!v %in% names(data)) next
    original<-data[[v]]
    state<-ifelse(!is.na(original)&original==1,"selected",ifelse(!is.na(original)&original==0,"not_selected","item_missing"))
    if(!is.null(reasons[[v]])) {
      state[startsWith(reasons[[v]],"structural_skip")]<-"structural_skip"
      state[reasons[[v]]=="survey_nonresponse"]<-"survey_nonresponse"
    }
    ss<-.sample_model_counts(bundle$symptom_counts[[id]],states,state,
      c("selected","not_selected","item_missing","structural_skip","survey_nonresponse"),source$seed,round,paste0("model::symptom::",id))
    stats["symptom_fallback"]<-stats["symptom_fallback"]+sum(!ss$used_released_distribution)
    change<-ss$used_released_distribution & ss$value!=state
    data[[v]][change]<-ifelse(ss$value[change]=="selected",1,ifelse(ss$value[change]=="not_selected",0,NA_real_))
    if(!is.null(reasons[[v]])) reasons[[v]][change]<-""
  }
  locked<-.model_lock(data,occurrences,source,context,dictionary,reasons)
  locked$data<-.synchronise_synthetic_age_group(locked$data,occurrences,dictionary)
  final_outcome<-.dependency_outcome_state(locked$data,round,oid,dictionary)
  stats["outcome_guarded"]<-stats["outcome_guarded"]+sum(final_outcome!=outcome)
  locked$summary<-paste0(round,":",paste(names(stats),stats,sep="=",collapse=","),
    ",unknown_predictor_rows=",sum(pr$unknown_predictors>0))
  locked
}
