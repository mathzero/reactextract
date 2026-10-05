test_that("approved dependency model inputs are pinned and complete", {
  x <- .read_dependency_models()
  expect_length(x$rounds,25L)
  expect_silent(.validate_dependency_models(x))
  y<-x;y$rounds[[1]]$context_counts[[1]]$table$count[1]<-3
  expect_error(.validate_dependency_models(y),"protected")
  y<-x;y$rounds[[1]]$model$coefficients[1]<-Inf
  expect_error(.validate_dependency_models(y),"coefficients")
  f<-tempfile();writeLines("not an approved model",f)
  expect_error(.read_dependency_models(f),"checksum")
})

test_that("all models predict from matching design levels without RNG changes", {
  x <- .read_dependency_models()
  set.seed(17);before<-.Random.seed
  for(z in x$rounds) {
    m<-z$model
    d<-as.data.frame(lapply(m$levels,function(v)rep(v,length.out=100)),stringsAsFactors=FALSE)
    actual<-.dependency_model_probability(m,d)
    design<-cbind(`(Intercept)`=1,do.call(cbind,lapply(names(m$levels),function(f)
      vapply(m$levels[[f]][-1],function(v)as.numeric(d[[f]]==v),numeric(nrow(d))))))
    expect_equal(actual$probability,as.numeric(plogis(design %*% m$coefficients)))
    expect_true(all(is.finite(actual$probability)))
    d[[1]][1]<-"not_a_model_level"
    expect_equal(.dependency_model_probability(m,d)$unknown_predictors[1],1L)
  }
  expect_identical(before,.Random.seed)
})

test_that("suppressed distributions retain safe fallback and sampling is stable", {
  spec<-list(parents="age",target="answer",table=data.frame(age=c("a","a","b","b"),
    answer=c("yes","no","yes","no"),count=c(NA,NA,20,80),suppressed=c(TRUE,TRUE,FALSE,FALSE)))
  d<-data.frame(age=rep(c("a","b","unknown"),each=1000))
  run<-function() .sample_model_counts(spec,d,rep("no",nrow(d)),c("yes","no"),1,"react1.r01","fixture")
  z<-run();expect_identical(z,run())
  expect_true(all(z$value[d$age!="b"]=="no"))
  expect_true(all(z$used_released_distribution==(d$age=="b")))
  expect_lt(abs(mean(z$value[d$age=="b"]=="yes")-.203),.05)
})
