test_that("approved participation inputs remain immutable and separate from v5", {
  p<-react_synthetic_profile(refresh=TRUE)
  expect_true(is.list(p$participation))
  expect_identical(p$participation$approval[["status"]],"formally_approved")
  expect_identical(p$participation$metadata[["generator_ready"]],"FALSE")
  expect_equal(nrow(p$participation$plan),14900L)
  expect_true(all(is.na(p$participation$counts$count)==p$participation$counts$suppressed))
  old<-react_synthetic_profile(version="v5")
  expect_null(old$participation)
  expect_s3_class(react_synthetic(old, n_per_round=1L), "react_synthetic_source")
  expect_s3_class(react_synthetic(react_synthetic_profile(development=TRUE), n_per_round=1L), "react_synthetic_source")
  expect_identical(old$categorical_counts,p$categorical_counts)
  expect_identical(old$dependency_counts,p$dependency_counts)
  expect_identical(.synthetic_profile_metadata(p)[["profile_release"]],"react-synthetic-profile-v6-preview")
  expect_error(react_write_profile(p,tempfile()),"composite bundled profile")
  broken<-p;broken$participation<-NULL
  expect_error(react_synthetic(broken),"Participation profile inputs are missing")
})

test_that("shared state follows the released binary rate without smoothing hidden rates", {
  s<-react_synthetic(n_per_round=10000L,seed=177L);d<-react_dictionary()
  x<-.participation_context(s,"react1.r02",10000L,d)
  q<-s$profile$participation$counts
  q<-q[q$table=="round_participation"&q$round_id=="react1.r02",]
  expected<-q$count[q$state=="shared_nonresponse"]/sum(q$count)
  expect_lt(abs(mean(x$nonresponse)-expected),0.025)
  expect_identical(x$status,"estimated")
  y<-.participation_context(s,"react2.r01",100L,d)
  expect_identical(y$status,"assumed_below_10")
  expect_equal(y$rate$assumed_count,5)
  expect_equal(y$rate$probability,5/109075)
  expect_true(all(y$age[!is.na(y$age)]>=18))
  expect_identical(x,.participation_context(s,"react1.r02",10000L,d))
  expect_true(all(x$age[!is.na(x$age)]>=5))
})

test_that("mandatory conditions preserve independent fields and date types", {
  s<-react_synthetic(n_per_round=4L);d<-react_dictionary()
  p<-s$profile$participation$occurrences
  o<-d$occurrences[d$occurrences$round_id=="react1.r10"&d$occurrences$variable%in%c("SMOKENOW","U_AGE","RESULT"),]
  expect_equal(nrow(o),3L)
  # Add a fictional date occurrence to exercise encoding, not real counts.
  date<-o[1,,drop=FALSE];date$occurrence_id<-"fixture_date";date$variable<-"DATE_FIXTURE";date$data_type<-"DATE"
  op<-p[1,,drop=FALSE];op$occurrence_id<-date$occurrence_id;op$variable<-date$variable;op$stage<-"individual";op$governed<-"TRUE";op$minimum_age<-""
  s$profile$participation$occurrences<-rbind(p,op);o<-rbind(o,date)
  data<-data.frame(SMOKENOW=c(1,1,-92,2),U_AGE=c(17,18,55,20),RESULT=rep("Detected",4),DATE_FIXTURE=as.Date("2020-01-01")+0:3)
  context<-list(age=data$U_AGE,nonresponse=c(FALSE,TRUE,FALSE,FALSE))
  z<-.participation_enforce(data,o,s,context,list())
  expect_true(is.na(z$data$SMOKENOW[1]))
  expect_equal(z$data$SMOKENOW[2],-77)
  expect_equal(z$data$SMOKENOW[3],-92)
  expect_identical(z$data$RESULT,data$RESULT)
  expect_identical(z$data$U_AGE,data$U_AGE)
  expect_s3_class(z$data$DATE_FIXTURE,"Date")
  expect_true(is.na(z$data$DATE_FIXTURE[2]))
  expect_equal(z$reasons$DATE_FIXTURE[2],"survey_nonresponse")
  expect_equal(z$reasons$SMOKENOW[1],"structural_skip_age")
})

test_that("missing codes cannot open strict answer comparisons", {
  x<-c(-77,-91,-92,-66,-555,NA,0,1,2)
  for(op in c("not_in","gt","gte","lt","lte","equals","in","selected_any"))
    expect_false(any(.condition_true(x,op,"[1]",strict_missing=TRUE)[1:6]))
  expect_true(all(.condition_true(x,"is_missing","[]",strict_missing=TRUE)[1:6]))
  expect_false(any(.condition_true(x,"not_missing","[]",strict_missing=TRUE)[1:6]))
  expect_true(.condition_true(-77,"not_in","[1]")) # v5 unchanged
})

test_that("all-field round output aligns nonresponse and blocks adult-only children", {
  s<-react_synthetic(n_per_round=c(REACT1_R02=80L),seed=88L);d<-react_dictionary()
  o<-d$occurrences[d$occurrences$round_id=="react1.r02",]
  reg<-d$source_registry[d$source_registry$round_id=="react1.r02",]
  z<-.read_synthetic_round(s,reg,o,d);x<-z$data
  non<-x$SFREPORTFIG==-77;non[is.na(non)]<-FALSE
  p<-s$profile$participation$occurrences
  gov<-p$variable[p$round_id=="react1.r02"&p$stage=="individual"&p$governed=="TRUE"]
  for(v in intersect(gov,names(x))) {
    if(inherits(x[[v]],c("Date","POSIXt")))expect_true(all(is.na(x[[v]][non])))
    else expect_true(all(as.character(x[[v]][non])=="-77"),info=v)
  }
  expect_true(any(non));expect_true(any(!is.na(x$RESULT[non])))
  smoke<-p$variable[p$round_id=="react1.r02"&p$minimum_age=="18"]
  child<-!is.na(x$U_AGE)&x$U_AGE<18
  expect_true(any(child))
  for(v in intersect(smoke,names(x)))expect_false(any(!is.na(x[[v]][child])&suppressWarnings(as.numeric(x[[v]][child]))>=0),info=v)
  expect_true(all(x$U_AGE[!is.na(x$U_AGE)]>=5))
  selected<-o[o$variable%in%c("SMOKENOW","HEALTHA_05"),]
  needed<-.synthetic_generation_occurrences(d,selected,"react1.r02",s)
  narrow<-.read_synthetic_round(s,reg,needed,d)$data
  for(v in selected$variable)expect_identical(narrow[[v]],x[[v]],info=v)
})

test_that("preview provenance survives long and wide extraction", {
  s<-react_synthetic(n_per_round=c(REACT1_R02=60L,REACT2_S5_R01=60L),seed=15L)
  a<-react_extract(s,concepts="health.preexisting.overweight",rounds=names(s$n_per_round),output="both",progress=FALSE)
  b<-react_extract(s,concepts="health.preexisting.overweight",rounds=names(s$n_per_round),output="wide",progress=FALSE)
  expect_identical(a$data,b$data);expect_identical(a$raw_data,b$raw_data)
  expect_true(any(a$harmonised_values$missing_reason=="survey_nonresponse"))
  expect_true("participation_rate_assumed" %in% a$issues$code)
  m<-stats::setNames(a$manifest$value,a$manifest$key)
  expect_identical(m[["synthetic_participation_unestimated_rounds"]],"")
  expect_identical(m[["synthetic_participation_assumed_rounds"]],"react2.r01")
  expect_identical(m[["synthetic_participation_unavailable_empirical_rounds"]],"react2.r01")
  expect_identical(m[["synthetic_release_readiness"]],"preview_not_full_v6_acceptance")
  expect_false(any(c("raw_values","harmonised_values") %in% names(b)))
})

test_that("date nonresponse provenance is retained without invalid dates", {
  h<-data.frame(observation_id=c("one","two"),source_raw_variable="DATE_FIELD",
    missing_reason=c("input_missing","input_missing"))
  obs<-data.frame(observation_id=c("one","two"))
  rr<-list(DATE_FIELD=c("survey_nonresponse","structural_skip_age"))
  z<-.participation_harmonised_reasons(h,obs,rr)
  expect_identical(z$missing_reason,rr$DATE_FIELD)
})
