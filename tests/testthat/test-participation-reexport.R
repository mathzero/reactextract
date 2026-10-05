reexport_path <- Sys.getenv("REACT_PARTICIPATION_REEXPORT_DIR",unset=system.file("enclave","participation-v6-reexport",package="reactextract"))
source(file.path(reexport_path,"followup-policy.R"),local=TRUE)
source(file.path(reexport_path,"profile.R"),local=TRUE)
source(file.path(reexport_path,"reexport.R"),local=TRUE)

# All counts in these fixtures are invented and independent of enclave returns.
reexport_fixture <- function(round="react2.r01") {
  p <- data.frame(occurrence_id=c("age","smoke","answer","lab","exception","qage"),round_id=round,
    variable=c("U_AGE","SMOKENOW","FEELUN","RESULT","SMOKECIG","AGE"),
    stage=c("unassigned","registration","individual","unassigned","individual","registration"),
    governed=c("FALSE","TRUE","TRUE","FALSE","FALSE","TRUE"),minimum_age=c("","18","","","18",""))
  p$minimum_age[6] <- ""
  ages <- c("0_4","5_11","12","13_15","16_17","18_24","25_34","35_44","45_54","55_64","65_74","75_84","85_120")
  band <- c("under_5","5_11","12","13_15","16_17",rep("18_54",4),rep("55_plus",4))
  groups <- c(0,12000,6000,9000,9000,120000,90000,0)
  if(startsWith(round,"react2")) groups[1:5] <- 0
  names(groups) <- pc6_age_levels
  domains <- do.call(rbind,lapply(seq_len(nrow(p)),function(i) {
    states <- if(p$variable[i] %in% c("U_AGE","AGE")) c("database_missing","coded:-77",paste0("bin:",ages),"outside_public_support") else
      c("database_missing","coded:-77","coded:-91","code:1","code:2","outside_public_support")
    data.frame(occurrence_id=p$occurrence_id[i],state=states,label=states,substantive=grepl("^(code:|bin:)",states))
  }))
  contexts <- as.vector(outer(c("response_evidenced","undetermined"),pc6_age_levels,paste,sep="|"))
  part <- expand.grid(state=pc6_detail_levels,reference_state=contexts,stringsAsFactors=FALSE)
  a <- sub("^.*[|]","",part$reference_state)
  part$count <- ifelse(startsWith(part$reference_state,"response_evidenced|"),
    ifelse(part$state=="shared_nonresponse",groups[a]*.1,
      ifelse(part$state==if(startsWith(round,"react2")) "response_evidenced" else "recorded_complete",groups[a]*.9,0)),0)
  cc <- list(pr6_frame(round,"participation_by_age","individual_stage_detail","registration_and_NHS_age",part$state,part$count,part$reference_state))
  for(i in seq_len(nrow(p))) {
    scope <- p$stage[i]
    use_age <- scope!="unassigned"||p$variable[i]%in%c("U_AGE","AGE")||nzchar(p$minimum_age[i])
    levels <- if(scope=="individual") pc6_stage_levels else if(scope=="registration") c("response_evidenced","undetermined") else "not_assigned"
    refs <- if(use_age) as.vector(outer(levels,pc6_age_levels,paste,sep="|")) else levels
    states <- domains$state[domains$occurrence_id==p$occurrence_id[i]]
    cells <- expand.grid(state=states,reference_state=refs,stringsAsFactors=FALSE); cells$count<-0
    for(ref in refs) {
      age <- sub("^.*[|]","",ref)
      n <- if(use_age) groups[age] else sum(groups)
      if(startsWith(ref,"undetermined")) n<-0
      if(scope=="individual"&&!startsWith(ref,"undetermined")) n<-n*if(startsWith(ref,"shared_nonresponse")) .1 else .9
      ids<-which(cells$reference_state==ref)
      if(p$variable[i]%in%c("U_AGE","AGE")) {
        bins <- paste0("bin:",ages[band==age])
        cells$count[ids[cells$state[ids]%in%bins]]<-if(length(bins)) n/length(bins) else 0
      } else if(startsWith(ref,"shared_nonresponse")) cells$count[ids[cells$state[ids]=="coded:-77"]]<-n
      else {
        cells$count[ids[cells$state[ids]=="code:1"]]<-n*.6
        cells$count[ids[cells$state[ids]=="code:2"]]<-n*.3
        cells$count[ids[cells$state[ids]=="database_missing"]]<-n*.1
      }
    }
    cc[[length(cc)+1L]]<-pr6_frame(round,"field_by_participation_age",p$occurrence_id[i],paste0(scope,if(use_age) "_and_NHS_age" else ""),cells$state,cells$count,cells$reference_state)
  }
  list(counts=do.call(rbind,cc),p=p,domains=domains)
}
protect_compact <- function(counts) pf6_prepare_export(list(counts=counts,
  manifest=data.frame(key=c("schema","status","dictionary_manifest_sha256"),value=c("participation-targeted-v3",pf6_internal,pf6_dictionary_hash))))$counts

testthat::test_that("sparse original matrices become usable without changing disclosure policy", {
  x<-reexport_fixture(); pf6_validate_counts(x$counts)
  old<-protect_compact(x$counts)
  testthat::expect_true(all(old$suppressed[old$table=="participation_by_age"]))
  testthat::expect_true(all(old$suppressed[old$field=="age"]))
  new<-pr6_collapse_round(x$counts,x$p,x$domains)
  safe<-protect_compact(new$counts)
  testthat::expect_false(any(safe$suppressed[safe$table=="round_participation"]))
  testthat::expect_gte(sum(!safe$suppressed[safe$table=="NHS_age"]),2L)
  testthat::expect_lt(nrow(safe),nrow(old)/3)
  testthat::expect_true(all(is.na(safe$count)==safe$suppressed))
  testthat::expect_true(all(safe$count[!safe$suppressed]>=10 & safe$count[!safe$suppressed]%%5==0))
})
testthat::test_that("participation, independent fields, adult restrictions and historical exceptions survive", {
  x<-reexport_fixture("react1.r09"); z<-pr6_collapse_round(x$counts,x$p,x$domains)
  count<-function(field) sum(z$counts$count[z$counts$field==field])
  total<-sum(x$counts$count[x$counts$table=="participation_by_age"])
  testthat::expect_equal(count("individual_participation"),total)
  testthat::expect_equal(count("lab"),total)
  testthat::expect_equal(count("answer"),total*.9)
  testthat::expect_equal(count("smoke"),210000)
  testthat::expect_equal(count("exception"),210000)
  testthat::expect_equal(z$counts$count[z$counts$field=="exception"&z$counts$state=="coded:-77"],21000)
  testthat::expect_equal(z$counts$count[z$counts$field=="answer"&z$counts$state=="coded:-77"],0)
  testthat::expect_true(all(z$counts$reference_state[z$counts$field=="age"]=="all"))
  testthat::expect_setequal(z$counts$reference_state[z$counts$field=="qage"],pc6_age_levels)
  testthat::expect_equal(count("qage"),total)
  # Changing raw count magnitudes never changes the public aggregation plan.
  testthat::expect_identical(z$plan,pr6_plan(x$p))
  # Residual source -77 among respondents is not erased or relabelled as a skip.
  row<-which(x$counts$field=="answer" & x$counts$reference_state=="response_evidenced|18_54" & x$counts$state=="code:1")
  other<-which(x$counts$field=="answer" & x$counts$reference_state=="response_evidenced|18_54" & x$counts$state=="coded:-77")
  x$counts$count[row]<-x$counts$count[row]-100;x$counts$count[other]<-100
  residual<-pr6_collapse_round(x$counts,x$p,x$domains)
  testthat::expect_equal(residual$counts$count[residual$counts$field=="answer"&residual$counts$state=="coded:-77"],100)
})
testthat::test_that("small binary counts stay hidden and no zero is filled back in", {
  z<-pr6_frame("react1.r01","round_participation","individual_participation","none",c("shared_nonresponse","not_shared_nonresponse"),c(3,5000))
  safe<-protect_compact(z)
  testthat::expect_true(all(safe$suppressed));testthat::expect_true(all(is.na(safe$count)))
  x<-reexport_fixture(); c<-pr6_collapse_round(x$counts,x$p,x$domains);d<-protect_compact(c$counts)
  report<-pr6_availability(d,x$domains,c$plan,"react2.r01")
  testthat::expect_true(all(report$core_usable[report$core_requirement]))
  testthat::expect_false(any(c("count","source_label","elapsed_seconds")%in%names(report)))
  d$count[d$table=="round_participation"]<-NA_real_;d$suppressed[d$table=="round_participation"]<-TRUE
  report<-pr6_availability(d,x$domains,c$plan,"react2.r01")
  testthat::expect_false(report$core_usable[report$role=="shared_participation"])
})
testthat::test_that("incomplete and contradictory source summaries fail closed", {
  x<-reexport_fixture()
  testthat::expect_error(pr6_collapse_round(x$counts[-1,],x$p,x$domains),"participation matrix")
  b<-x$counts;b$count[which(b$field=="lab")[1]]<-1
  testthat::expect_error(pr6_collapse_round(b,x$p,x$domains),"inconsistent field")
  b<-x$p;b$minimum_age[2]<-"16"
  testthat::expect_error(pr6_plan(b),"explicit compact-export rule")
  testthat::expect_error(pr6_verify(list(counts=x$counts)),"original capture")
  testthat::expect_error(pr6_write(list(),tempfile()),"not validated")
})
testthat::test_that("writer exports only protected whitelisted components and never overwrites", {
  f<-reexport_fixture();c<-pr6_collapse_round(f$counts,f$p,f$domains)
  x<-list(counts=protect_compact(c$counts),domains=f$domains,plan=c$plan,
    issues=data.frame(round_id=character(),variable=character(),issue=character()))
  x$availability<-pr6_availability(x$counts,x$domains,x$plan,"react2.r01")
  x$manifest<-data.frame(key=c("schema","status","rounds"),value=c(pr6_schema,pc6_pending,"react2.r01"))
  x<-pr6_seal(x);out<-tempfile("compact-export-")
  pr6_write(x,out)
  testthat::expect_setequal(list.files(out),c("counts.csv","domains.csv","plan.csv","issues.csv","availability.csv","manifest.csv","README.txt","AVAILABILITY.md","checksums.csv"))
  h<-pc6_read(file.path(out,"checksums.csv"))
  testthat::expect_identical(unname(vapply(file.path(out,h$file),pc6_hash,character(1))),h$sha256)
  testthat::expect_error(pr6_write(x,out),"already exists")
  b<-x;b$counts$count[1]<-3
  testthat::expect_error(pr6_write(b,tempfile()),"changed")
  b<-x;b$domains$label[1]<-"PRIVATE TEXT"
  testthat::expect_error(pr6_write(b,tempfile()),"changed")
})
