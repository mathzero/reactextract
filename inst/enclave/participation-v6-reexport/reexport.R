# Aggregate-only v6 re-export. No database calls, respondent rows or new packages.
pr6_schema <- "participation-conditional-compact-v2"
pr6_source_schema <- "participation-conditional-capture-v1"
pr6_count_columns <- c("round_id","table","field","reference","state","reference_state","count","field_kind","reference_kind")
pr6_meta <- function(x, key) pf6_meta(x, key)
pr6_seal <- function(x) {
  attr(x,"validated_payload_sha256") <- NULL
  attr(x,"validated_payload_sha256") <- digest::digest(x,algo="sha256")
  x
}
pr6_frame <- function(round, table, field, reference, state, count, context="all") {
  data.frame(round_id=round, table=table, field=field, reference=reference,
    state=state, reference_state=context, count=as.numeric(count),
    field_kind="public_value_state", reference_kind=if(reference=="none") "none" else "participation_context",
    stringsAsFactors=FALSE)
}
pr6_plan <- function(p) {
  governed <- p$stage == "individual" & p$governed == "TRUE"
  if (any(!p$minimum_age %in% c("","18"))) stop("A new age threshold needs an explicit compact-export rule.")
  scope <- ifelse(governed,"individual_not_shared_nonresponse","all_observations")
  scope[nzchar(p$minimum_age)] <- paste0(scope[nzchar(p$minimum_age)],"_age18plus")
  age <- p$variable == "AGE"
  scope[age] <- paste0(scope[age],"_by_NHS_age")
  data.frame(occurrence_id=p$occurrence_id, round_id=p$round_id, variable=p$variable,
    role=ifelse(p$variable=="U_AGE","NHS_age",ifelse(age,"questionnaire_age","answer_distribution")),
    source_stage=p$stage, shared_nonresponse_removed=governed,
    minimum_age=p$minimum_age, condition=scope,
    age_conditioning=ifelse(age,"NHS_age_bands",ifelse(nzchar(p$minimum_age),"adult_only_pooled","pooled")),
    stringsAsFactors=FALSE)
}
pr6_verify <- function(x, kit=".", progress=TRUE) {
  say <- function(...) if(progress) message("[reactextract re-export] ",...)
  if (!is.list(x) || !setequal(names(x),c("counts","domains","issues","timings","manifest")))
    stop("Use conditional-profile-INTERNAL.rds from the original capture, not an exported CSV or another RDS file.")
  if (!identical(pr6_meta(x,"schema"),pr6_source_schema) ||
      !identical(pr6_meta(x,"status"),pc6_internal) || "suppressed" %in% names(x$counts))
    stop("Only the original unsuppressed INTERNAL aggregate checkpoint can be re-exported.")
  if(!grepl("^0[.]5[.][0-9]+$",pr6_meta(x,"package_version"))) stop("Unexpected source package version.")
  c6 <- pc6_contract(file.path(kit,"contract"))
  if (!identical(pr6_meta(x,"dictionary_manifest_sha256"),pf6_dictionary_hash) ||
      !identical(pr6_meta(x,"capture_contract_sha256"),c6$hash) ||
      !identical(pr6_meta(x,"implementation_sha256"),pc6_hash(file.path(kit,"profile.R"))))
    stop("The checkpoint does not match this kit's dictionary, capture code or contract. Do not mix runs.")
  d <- reactextract::react_dictionary()
  rounds <- d$rounds$round_id
  if (!setequal(strsplit(pr6_meta(x,"rounds"),"|",fixed=TRUE)[[1]],rounds) ||
      !setequal(unique(x$counts$round_id),rounds)) stop("Expected the complete 25-round capture.")
  if (!identical(names(x$counts),pr6_count_columns)) stop("Unexpected count columns.")
  pf6_validate_counts(x$counts)
  if (!identical(names(x$domains),c("occurrence_id","state","label","substantive")) ||
      anyNA(x$domains) || anyDuplicated(x$domains[c("occurrence_id","state")])) stop("Invalid public domains.")
  if (!identical(names(x$issues),c("round_id","variable","issue")) || anyNA(x$issues) ||
      any(x$issues$issue != "field_unavailable_no_nonresponse_inference_if_stage_incomplete")) stop("Unexpected issue records.")
  p <- c6$occurrence_participation
  s <- pc6_ns(".approved_profile_specs")(d)
  s <- s[match(p$occurrence_id,s$occurrence_id),,drop=FALSE]
  safe <- !is.na(s$profile_kind) & !s$profile_kind %in% "identifier" &
    !s$generation_action %in% c("excluded","synthetic_identifier") & !p$variable %in% c("DOB","DATEOFBIRTH")
  issue_keys <- paste(x$issues$round_id,x$issues$variable)
  if(anyDuplicated(issue_keys) || !all(issue_keys %in% paste(p$round_id[safe],p$variable[safe]))) stop("Unknown unavailable fields.")
  p <- p[safe & !paste(p$round_id,p$variable) %in% issue_keys,,drop=FALSE]
  if (!setequal(p$occurrence_id,x$domains$occurrence_id)) stop("Incomplete public field coverage.")
  dom_index <- split(seq_len(nrow(x$domains)),x$domains$occurrence_id)
  d$participation_age_bins <- c6$age_context
  say("Checking public response domains for ",nrow(p)," fields; no database queries")
  for (i in seq_len(nrow(p))) {
    id <- p$occurrence_id[i]
    o <- d$occurrences[match(id,d$occurrences$occurrence_id),,drop=FALSE]
    dd <- pc6_domain(o,s[match(id,s$occurrence_id),,drop=FALSE],d)
    a <- x$domains[dom_index[[id]],,drop=FALSE]
    if (!identical(as.character(a$state),dd$states) || !identical(as.character(a$label),dd$labels) ||
        !identical(as.character(a$substantive),as.character(dd$substantive))) stop("Public domain mismatch: ",id)
  }
  list(contract=c6, occurrences=p, rounds=rounds, domains=dom_index)
}
pr6_check_round <- function(z, p, domains, age_levels=pc6_age_levels) {
  part <- z[z$table=="participation_by_age",,drop=FALSE]
  refs <- as.vector(outer(c("response_evidenced","undetermined"),age_levels,paste,sep="|"))
  if (nrow(part)!=112L || !all(part$field=="individual_stage_detail") ||
      !all(part$reference=="registration_and_NHS_age") ||
      !setequal(part$state,pc6_detail_levels) || !setequal(part$reference_state,refs)) stop("Incomplete participation matrix.")
  if (any(!z$table %in% c("participation_by_age","field_by_participation_age")) ||
      any(z$field_kind!="public_value_state" | z$reference_kind!="participation_context")) stop("Unexpected capture table.")
  fields <- z[z$table=="field_by_participation_age",,drop=FALSE]
  if (!setequal(fields$field,p$occurrence_id)) stop("Round field coverage mismatch.")
  total <- sum(part$count)
  idx <- split(seq_len(nrow(fields)),fields$field)
  context_totals <- list()
  for (i in seq_len(nrow(p))) {
    a <- fields[idx[[p$occurrence_id[i]]],,drop=FALSE]
    scope <- p$stage[i]
    use_age <- scope!="unassigned" || p$variable[i] %in% c("AGE","U_AGE") || nzchar(p$minimum_age[i])
    lev <- if(scope=="individual") pc6_stage_levels else if(scope=="registration") c("response_evidenced","undetermined") else "not_assigned"
    expect_ref <- if(use_age) as.vector(outer(lev,age_levels,paste,sep="|")) else lev
    state <- domains$state[domains$occurrence_id==p$occurrence_id[i]]
    if (nrow(a)!=length(state)*length(expect_ref) || !setequal(a$state,state) ||
        !setequal(a$reference_state,expect_ref) ||
        !all(a$reference==paste0(scope,if(use_age) "_and_NHS_age" else "")) || sum(a$count)!=total)
      stop("Incomplete or inconsistent field matrix: ",p$occurrence_id[i])
    margins <- tapply(a$count,a$reference_state,sum)
    group <- a$reference[1L]
    if (!is.null(context_totals[[group]]) && !identical(margins,context_totals[[group]]))
      stop("Inconsistent participation/age denominators within a round.")
    context_totals[[group]] <- margins
  }
  invisible(TRUE)
}
pr6_collapse_round <- function(z, p, domains) {
  # A fixed public plan, never chosen according to which counts are small.
  pr6_check_round(z,p,domains)
  part <- z[z$table=="participation_by_age",,drop=FALSE]
  out <- list(pr6_frame(z$round_id[1L],"round_participation","individual_participation","none",
    c("shared_nonresponse","not_shared_nonresponse"),
    c(sum(part$count[part$state=="shared_nonresponse"]),sum(part$count[part$state!="shared_nonresponse"]))))
  plan <- pr6_plan(p)
  fields <- z[z$table=="field_by_participation_age",,drop=FALSE]
  index <- split(seq_len(nrow(fields)),fields$field)
  didx <- split(seq_len(nrow(domains)),domains$occurrence_id)
  for (i in seq_len(nrow(p))) {
    id <- p$occurrence_id[i]
    a <- fields[index[[id]],,drop=FALSE]
    if(plan$shared_nonresponse_removed[i]) a <- a[!startsWith(a$reference_state,"shared_nonresponse|"),,drop=FALSE]
    age <- sub("^.*[|]","",a$reference_state)
    if(nzchar(plan$minimum_age[i])) {keep <- age %in% c("18_54","55_plus");a <- a[keep,,drop=FALSE];age <- age[keep]}
    states <- domains$state[didx[[id]]]
    if(p$variable[i]=="AGE") {
      # AGE remains questionnaire age, conditional on NHS age, not a copy of U_AGE.
      cells <- expand.grid(state=states,reference_state=pc6_age_levels,stringsAsFactors=FALSE)
      sums <- tapply(a$count,paste(a$state,age,sep="\r"),sum)
      value <- unname(sums[paste(cells$state,cells$reference_state,sep="\r")])
      if(anyNA(value)) stop("Missing questionnaire-age cells.")
      out[[length(out)+1L]] <- pr6_frame(z$round_id[1L],"questionnaire_age_by_NHS_age",id,plan$condition[i],cells$state,value,cells$reference_state)
    } else {
      sums <- tapply(a$count,a$state,sum)
      value <- unname(sums[states])
      if(anyNA(value)) stop("Missing compact field states.")
      out[[length(out)+1L]] <- pr6_frame(z$round_id[1L],if(p$variable[i]=="U_AGE") "NHS_age" else "field_distribution",
        id,plan$condition[i],states,value)
    }
  }
  list(counts=do.call(rbind,out),plan=plan)
}
pr6_availability <- function(d, domains, plan, rounds) {
  # Only protected data are used: no hidden count, magnitude or private total.
  index <- split(seq_len(nrow(d)),paste(d$round_id,d$table,d$field,sep="\r"))
  substantive <- paste(domains$occurrence_id[as.character(domains$substantive)=="TRUE"],
    domains$state[as.character(domains$substantive)=="TRUE"],sep="\r")
  out <- lapply(index,function(idx) {
    a <- d[idx,,drop=FALSE]
    role <- if(a$table[1L]=="round_participation") "shared_participation" else plan$role[match(a$field[1L],plan$occurrence_id)]
    substantive_state <- if(role=="shared_participation") rep(TRUE,nrow(a)) else paste(a$field,a$state,sep="\r") %in% substantive
    available <- !a$suppressed
    valid <- if(role=="shared_participation") all(available) else sum(available & substantive_state)>=if(role=="NHS_age") 2L else 1L
    data.frame(round_id=a$round_id[1L],field=a$field[1L],role=role,
      visible_cells=sum(available),hidden_cells=sum(!available),
      visible_answer_cells=sum(available & substantive_state),
      availability=if(!any(available)) "unavailable" else if(!valid) "insufficient_answer_support" else if(any(!available)) "partial_public_prior_needed" else "available",
      core_requirement=role %in% c("shared_participation","NHS_age"),core_usable=role %in% c("shared_participation","NHS_age") & valid,
      stringsAsFactors=FALSE)
  })
  ans <- do.call(rbind,out); rownames(ans)<-NULL
  # A missing U_AGE occurrence is a visible blocker, not a silent success.
  for(r in setdiff(rounds,ans$round_id[ans$role=="NHS_age"])) ans <- rbind(ans,data.frame(round_id=r,
    field="U_AGE_unavailable",role="NHS_age",visible_cells=0L,hidden_cells=0L,visible_answer_cells=0L,
    availability="unavailable",core_requirement=TRUE,core_usable=FALSE))
  ans
}
pr6_reexport <- function(x, kit=".", progress=TRUE) {
  checked <- pr6_verify(x,kit,progress)
  counts <- plans <- list()
  say <- function(...) if(progress) message("[reactextract re-export] ",...)
  for (i in seq_along(checked$rounds)) {
    r <- checked$rounds[i]
    say("Round ",i,"/",length(checked$rounds)," ",r," | combining saved counts and applying protection")
    p <- checked$occurrences[checked$occurrences$round_id==r,,drop=FALSE]
    dom <- x$domains[x$domains$occurrence_id %in% p$occurrence_id,,drop=FALSE]
    compact <- pr6_collapse_round(x$counts[x$counts$round_id==r,,drop=FALSE],p,dom)
    # Exact unchanged v3 disclosure policy; one round at a time bounds memory.
    z <- list(counts=compact$counts,manifest=data.frame(key=c("schema","status","dictionary_manifest_sha256"),
      value=c("participation-targeted-v3",pf6_internal,pf6_dictionary_hash)))
    protected <- pf6_prepare_export(z)
    counts[[r]] <- protected$counts; plans[[r]] <- compact$plan
  }
  out <- list(counts=do.call(rbind,counts),domains=x$domains,plan=do.call(rbind,plans),issues=x$issues)
  out$availability <- pr6_availability(out$counts,out$domains,out$plan,checked$rounds)
  core <- out$availability[out$availability$core_requirement,,drop=FALSE]
  keys <- c("schema","status","dictionary_manifest_sha256","capture_contract_sha256","source_implementation_sha256",
    "reexport_implementation_sha256","disclosure_implementation_sha256","source_schema","source_package_version","rounds",
    "minimum_cell","round_counts_to","suppression_policy","cross_release_review","database_queries",
    "participation_definition","participation_age_relationship","registration_status","eligibility_status",
    "core_distributions_available","generator_ready")
  values <- c(pr6_schema,pc6_pending,pf6_dictionary_hash,checked$contract$hash,pr6_meta(x,"implementation_sha256"),
    pc6_hash(file.path(kit,"reexport.R")),pc6_hash(file.path(kit,"followup-policy.R")),pr6_source_schema,pr6_meta(x,"package_version"),
    paste(checked$rounds,collapse="|"),"10","5",pf6_meta(protected,"suppression_policy"),
    "required_against_conditional_v1_and_all_previous_profiles","0",
    "shared_nonresponse_vs_all_other_states_not_completion","not_released_no_empirical_age_participation_link_claimed",
    "response_evidence_only_no_shared_nonresponse_inference","adult_smoking_only_other_compound_rules_pending",
    as.character(nrow(core)==50L && all(core$core_usable)),"FALSE")
  out$manifest <- data.frame(key=keys,value=values,stringsAsFactors=FALSE)
  say("Protection complete. Core distributions usable: ",sum(core$core_usable),"/",nrow(core),
      ". This is still a candidate requiring disclosure review.")
  pr6_seal(out)
}
pr6_write <- function(x, path) {
  # Accidental edits after protection must not enter an export. This checksum is
  # an integrity guard, not an authentication scheme or disclosure approval.
  seal <- attr(x,"validated_payload_sha256")
  attr(x,"validated_payload_sha256") <- NULL
  if(is.null(seal)||!identical(seal,digest::digest(x,algo="sha256"))) stop("Protected candidate changed or was not validated. Re-run pr6_reexport().")
  if(!identical(pr6_meta(x,"schema"),pr6_schema) || !identical(pr6_meta(x,"status"),pc6_pending)) stop("Use a protected compact candidate.")
  expected <- c("counts","domains","plan","issues","availability","manifest")
  if(!setequal(names(x),expected)) stop("Unexpected candidate components; refusing export.")
  d <- x$counts
  if(!identical(names(d),c(pr6_count_columns,"protection","suppressed")) || !is.logical(d$suppressed) || anyNA(d$suppressed) ||
    any(is.na(d$count)!=d$suppressed) || any(!is.na(d$count)&(!is.finite(d$count)|d$count<10|d$count%%5!=0))) stop("Invalid protected counts.")
  fresh <- pr6_availability(d,x$domains,x$plan,strsplit(pr6_meta(x,"rounds"),"|",fixed=TRUE)[[1]])
  if(!isTRUE(all.equal(x$availability,fresh,check.attributes=FALSE))) stop("Availability report differs from protected data.")
  if(file.exists(path)||dir.exists(path)) stop("Output already exists. Choose a fresh output folder; nothing was overwritten.")
  dir.create(path,recursive=TRUE)
  for(name in expected) utils::write.csv(x[[name]],file.path(path,paste0(name,".csv")),row.names=FALSE,na="")
  core <- x$availability[x$availability$core_requirement,,drop=FALSE]
  writeLines(c("Compact participation candidate: normal enclave disclosure approval required.",
    "Only saved aggregate counts were used. No database queries or respondent records.",
    "Not a generator-ready v6 profile. Do not install or publish this folder as a profile.",
    "See AVAILABILITY.md and availability.csv before requesting release review.",
    "Keep the original INTERNAL checkpoint and this script's R workspace inside the enclave."),file.path(path,"README.txt"))
  missing <- core[!core$core_usable,,drop=FALSE]
  report <- c("# Availability of the protected compact export","",
    paste0("Core distributions with usable counts: **",sum(core$core_usable)," of ",nrow(core),"**."),"",
    "These are counts of exported distributions, not counts of people. Hidden entries are unknown, not zero.","",
    "- Participation is usable only when both binary state counts are visible.",
    "- NHS age is usable for a partial profile when at least two public age-bin counts are visible. Hidden bins still require an explicitly documented public prior; this is not complete distribution recovery.",
    "- Other field profiles can remain partly or wholly unavailable. availability.csv lists them without revealing hidden counts.","",
    "## Action","",
    if(nrow(missing)) "Some required distributions are unavailable. Return this protected report with the candidate for review; do not repeat the database pull or fill hidden cells with zeros." else
      "The core counts are available. Obtain normal disclosure review for this complete candidate and all overlapping earlier releases before it is used outside the enclave.","",
    "Technical checks do not grant disclosure approval. Generator integration, remaining questionnaire rules and profile approval are still required.","",
    "## Required distributions needing attention","",
    if(nrow(missing)) paste0("- ",missing$round_id," / ",missing$role,": ",missing$availability) else "None.")
  writeLines(report,file.path(path,"AVAILABILITY.md"))
  files <- sort(list.files(path))
  utils::write.csv(data.frame(file=files,sha256=vapply(file.path(path,files),pc6_hash,character(1))),file.path(path,"checksums.csv"),row.names=FALSE)
  invisible(path)
}
