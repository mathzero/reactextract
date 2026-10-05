test_that("small shared participation is an assumption, not a replacement count", {
  s <- react_synthetic(n_per_round=10L)
  before <- s$profile$participation
  total <- s$profile$round_denominators
  for (round in paste0("react2.r0",1:6)) {
    r <- .participation_rate(s,round)
    n <- as.numeric(total$count[total$round_id==round])
    expect_identical(r$status,"assumed_below_10")
    expect_equal(r$assumed_count,5)
    expect_equal(r$probability,5/n)
    expect_equal(r$lower,0)
    expect_equal(r$upper,9/(n-2)) # denominator rounded to five, not exact
    expect_gt(r$upper,r$probability)
    expect_match(r$denominator_source,"assumption")
  }
  expect_identical(s$profile$participation,before)
  old <- s$profile
  old$metadata <- old$metadata[old$metadata$key!="participation_rate_policy",]
  expect_error(react_synthetic(old),"rate policy")
  expect_true(all(is.na(before$counts$count[before$counts$suppressed])))
  for (round in sprintf("react1.r%02d",1:19)) expect_identical(.participation_rate(s,round)$status,"estimated")
  expect_identical(.participation_rate(s,"unknown")$status,"unestimated")
})

test_that("other suppression reasons and unavailable denominators cannot use five", {
  s <- react_synthetic(n_per_round=1L); round <- "react2.r01"
  cc <- s$profile$participation$counts
  idx <- which(cc$field=="individual_participation" & cc$round_id==round & cc$state=="shared_nonresponse")
  for (reason in c("linked_complement","withheld_remainder","unknown",NA_character_)) {
    z <- s; z$profile$participation$counts$protection[idx] <- reason
    expect_identical(.participation_rate(z,round)$status,"unestimated",info=reason)
  }
  z <- s; z$profile$participation$counts$protection <- NULL
  expect_identical(.participation_rate(z,round)$status,"unestimated")
  z <- s; z$profile$participation$metadata["minimum_cell"] <- "20"
  expect_identical(.participation_rate(z,round)$status,"unestimated")
  z <- s; z$profile$participation$counts$protection[idx+1L] <- "below_10"
  expect_identical(.participation_rate(z,round)$status,"unestimated")
  j <- which(s$profile$round_denominators$round_id==round)
  for (n in c(NA,0,-1,9,109076,Inf)) {
    z <- s; z$profile$round_denominators$count[j] <- n
    expect_identical(.participation_rate(z,round)$status,"unestimated")
  }
  z <- s; z$profile$round_denominators$suppressed[j] <- TRUE
  expect_identical(.participation_rate(z,round)$status,"unestimated")
  z <- s; z$profile$round_denominators <- rbind(z$profile$round_denominators,z$profile$round_denominators[j,])
  expect_identical(.participation_rate(z,round)$status,"unestimated")
  z <- s; z$profile$round_denominators <- NULL
  expect_identical(.participation_rate(z,round)$status,"unestimated")
  for (key in c("approved_profile_manifest_sha256","dictionary_manifest_sha256","count_rounding")) {
    z <- s; z$profile$metadata$value[z$profile$metadata$key==key] <- "different"
    expect_identical(.participation_rate(z,round)$status,"unestimated",info=key)
  }
  # A true fallback remains visibly unestimated in the context.
  z <- s; z$profile$round_denominators <- NULL
  ctx <- .participation_context(z,round,20L,react_dictionary())
  expect_identical(ctx$status,"unestimated")
  expect_false(any(ctx$nonresponse))
})

test_that("small-rate draws use the seed and the documented probability", {
  s <- react_synthetic(n_per_round=1L,seed=904L); d <- react_dictionary()
  a <- .participation_context(s,"react2.r01",200000L,d)
  expect_identical(a,.participation_context(s,"react2.r01",200000L,d))
  expected <- 200000*a$rate$probability
  expect_lt(abs(sum(a$nonresponse)-expected),6*sqrt(expected))
  expect_gt(sum(a$nonresponse),0)
  s$seed <- 905L
  expect_false(identical(a$nonresponse,.participation_context(s,"react2.r01",200000L,d)$nonresponse))
})

test_that("assumed shared nonresponse is row-aligned in REACT-2 and selection-stable", {
  s <- react_synthetic(n_per_round=40L,seed=906L); d <- react_dictionary(); round <- "react2.r01"
  # Fictional tiny denominator forces events so the final overwrite is tested.
  # No protected count or real denominator is altered in the shipped profile.
  s$profile$round_denominators$count[s$profile$round_denominators$round_id==round] <- 20
  context <- .participation_context(s,round,40L,d)
  expect_true(any(context$nonresponse)); expect_true(any(!context$nonresponse))
  o <- d$occurrences[d$occurrences$round_id==round,]
  registry <- d$source_registry[d$source_registry$round_id==round,]
  all <- .read_synthetic_round(s,registry,o,d)
  p <- s$profile$participation$occurrences
  governed <- p$variable[p$round_id==round & p$stage=="individual" & p$governed=="TRUE"]
  for (v in intersect(governed,names(all$data))) {
    value <- all$data[[v]][context$nonresponse]
    expect_true(if(inherits(value,c("Date","POSIXt"))) all(is.na(value)) else
      all(as.character(value)=="-77"),info=v)
    expect_true(all(all$missing_reasons[[v]][context$nonresponse]=="survey_nonresponse"),info=v)
  }
  expect_identical(all$data$U_AGE,context$age)
  expect_false(any(all$data$U_GENDER[context$nonresponse]==-77,na.rm=TRUE))
  expect_true("participation_rate_assumed" %in% all$issues$code)
  one <- o[o$variable=="NEWRESULT",]
  narrow <- .read_synthetic_round(s,registry,.synthetic_generation_occurrences(d,one,round,s),d)
  expect_identical(narrow$data$NEWRESULT,all$data$NEWRESULT)
  expect_identical(narrow$missing_reasons$NEWRESULT,all$missing_reasons$NEWRESULT)
})
