test_that("v6 pins models and v5 remains separate", {
  p<-react_synthetic_profile();expect_identical(p$dependency_models,.read_dependency_models())
  expect_null(react_synthetic_profile(version="v5")$dependency_models)
  bad<-p;bad$dependency_models$rounds[[1]]$model$coefficients[1]<-0
  expect_error(react_synthetic(bad),"missing or changed")
})

test_that("all round encoders preserve routing and coherent outcomes", {
  d<-react_dictionary();s<-react_synthetic(n_per_round=24L,seed=906L)
  for(round in d$rounds$round_id) {
    o<-d$occurrences[d$occurrences$round_id==round,,drop=FALSE]
    reg<-d$source_registry[d$source_registry$round_id==round,,drop=FALSE]
    z<-.read_synthetic_round(s,reg,o,d);x<-z$data
    expect_equal(nrow(x),24L,info=round)
    expect_match(z$model_summary,"context_fallback",info=round)
    context<-.participation_context(s,round,24L,d)
    enforced<-.model_lock(x,o,s,context,d,z$missing_reasons)
    expect_equal(enforced$data,x,info=round)
    pp<-s$profile$participation$occurrences
    governed<-pp$variable[pp$round_id==round & pp$stage=="individual" & pp$governed=="TRUE"]
    for(v in intersect(governed,names(x))) {
      value<-x[[v]][context$nonresponse]
      expect_true(if(inherits(value,c("Date","POSIXt"))) all(is.na(value)) else all(as.character(value)=="-77"),info=paste(round,v))
    }
    if(startsWith(round,"react2")) {
      outcome<-.dependency_react2_outcome(x,round)
      valid<-outcome %in% c("positive","negative")
      if("ABATTEMPT" %in% names(x)) expect_true(all(x$ABATTEMPT[valid]==1),info=round)
      if("ABCOMP" %in% names(x)) expect_true(all(x$ABCOMP[valid]==1),info=round)
      if(all(c("NEWRESULT","NEWRESULT_2") %in% names(x))) {
        expect_true(all(x$NEWRESULT_2[valid]==ifelse(outcome[valid]=="positive",2,1)),info=round)
      }
    }
  }
})

test_that("model integration is deterministic and retains wide output contract", {
  s<-react_synthetic(n_per_round=c(REACT1_R17=80L,REACT2_S5_R06=80L),seed=191L)
  a<-react_extract(s,rounds=names(s$n_per_round),output="wide",progress=FALSE)
  b<-react_extract(s,rounds=names(s$n_per_round),concepts="health.preexisting.overweight",output="both",progress=FALSE)
  keys<-b$observations$observation_id
  expect_identical(a$observations$observation_id,keys)
  for(v in names(b$raw_data)) expect_identical(a$raw_data[[v]],b$raw_data[[v]],info=v)
  expect_false(any(c("raw_values","harmonised_values") %in% names(a)))
  m<-setNames(a$manifest$value,a$manifest$key)
  expect_identical(m[["synthetic_dependency_models_sha256"]],.dependency_models_sha256)
  expect_match(m[["synthetic_outcome_relationship_scope"]],"ridge")
  expect_true(any(a$issues$code=="dependency_model_limits"))
})

test_that("outcome encoders follow model probabilities rather than old v5 draws", {
  d<-react_dictionary();s<-react_synthetic(n_per_round=32L,seed=196L)
  for(r in c("react1.r01","react1.r17","react2.r06")) {
    o<-d$occurrences[d$occurrences$round_id==r,,drop=FALSE]
    reg<-d$source_registry[d$source_registry$round_id==r,,drop=FALSE]
    oid<-unique(.dependency_specs(d,r)$outcome_id)
    for(intercept in c(-30,30)) {
      # Internal fictional model fixture; public sources reject mutated models.
      fixture<-s
      fixture$profile$dependency_models$rounds[[r]]$model$coefficients[]<-0
      fixture$profile$dependency_models$rounds[[r]]$model$coefficients[1]<-intercept
      z<-.read_synthetic_round(fixture,reg,o,d)
      outcome<-.dependency_outcome_state(z$data,r,oid,d)
      evaluable<-outcome %in% c("positive","negative")
      expect_true(any(evaluable),info=r)
      expect_true(all(outcome[evaluable]==if(intercept>0) "positive" else "negative"),info=r)
    }
  }
})
