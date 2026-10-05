test_that("only approved exact options enter the generator", {
  x <- .response_options_contract(refresh = TRUE); d <- react_dictionary()
  expect_equal(nrow(x$rules), 109L)
  expect_silent(.validate_response_options(x, d))
  e <- d$occurrences[match(x$targets$target_occurrence_id,d$occurrences$occurrence_id),]
  expect_false(any(e$round_id == "react2.r05"))
  expect_false(any(e$round_id %in% sprintf("react1.r%02d",14:16) &
    startsWith(e$variable,"VACCRUFUSE1")))
  bad <- x; bad$rules$review_state[1] <- "candidate"
  expect_error(.validate_response_options(bad,d),"approval")
  bad <- x; bad$conditions$context_key[1] <- "unverified_mailgroup"
  expect_error(.validate_response_options(bad,d),"context source")
  p <- react_synthetic_profile(); p$response_options$rules$selected_code[1] <- "2"
  expect_error(react_synthetic(p),"missing or changed")
  expect_null(react_synthetic_profile(version="v5")$response_options)
})

test_that("all option boundaries and missing parents are respected", {
  x <- .response_options_contract(); d <- react_dictionary()
  age <- c(12,13,16,17,18,54,55,NA,18,18,16,16)
  for (id in x$targets$target_occurrence_id) {
    o <- d$occurrences[d$occurrences$round_id == d$occurrences$round_id[match(id,d$occurrences$occurrence_id)],]
    data <- data.frame(U_GENDER=c(rep(2,8),1,-77,2,2),INDCONF=c(rep(1,10),2,-91))
    v <- o$variable[match(id,o$occurrence_id)]; data[[v]] <- 1
    want <- if(grepl("^VACCRUFUSE",v) || v=="BOOSTERBARRIERS_25") {
      if (grepl("_14$",v) || v=="BOOSTERBARRIERS_25") !is.na(age)&age>=18&age<55&data$U_GENDER==2 else
        !is.na(age)&age>=13&age<18&data$INDCONF==1
    } else if (v=="COVIDCONPL_2") !is.na(age)&(age>=18 | (age>=13&age<18&data$INDCONF==1)) else
      if(grepl("^(Q?ISOLLEAVEREASON)_(1|2|8|12)$",v)) !is.na(age)&age>=16 else !is.na(age)&age>=18
    expect_identical(.response_option_eligible(data,id,o,list(age=age),x),want,info=v)
    # An already unasked question stays missing; option enforcement never creates an answer.
    data[[v]][want] <- NA_real_
    z <- .apply_response_options(data,o,list(age=age),x,list())
    expect_true(all(is.na(z$data[[v]])))
    expect_true(all(z$reasons[[v]][!want]=="structural_skip_option"))
    reasons <- stats::setNames(list(rep("structural_skip_questionnaire",length(age))),v)
    z <- .apply_response_options(data,o,list(age=age),x,reasons)
    expect_true(all(z$reasons[[v]]=="structural_skip_questionnaire"))
  }
})

test_that("sampling and its public prior never populate unavailable options", {
  s <- react_synthetic(n_per_round=12L); d <- react_dictionary(); x <- s$profile$response_options
  id <- x$targets$target_occurrence_id[1]; o <- d$occurrences[match(id,d$occurrences$occurrence_id),]
  eligible <- rep(c(TRUE,FALSE),6)
  z <- .participation_value(s,o,12,d,eligible=eligible)
  expect_true(all(is.na(z$value[!eligible])))
  expect_true(all(z$reason[!eligible]=="structural_skip_option"))
  # With all empirical cells suppressed the same constraint applies to the prior.
  s$profile$participation$counts$count[] <- NA_real_
  z <- .participation_value(s,o,12,d,eligible=eligible)
  expect_true(all(is.na(z$value[!eligible])))
  expect_true(all(z$value[eligible] %in% c(0,1)))
  expect_identical(.participation_value(s,o,12,d,eligible=eligible),z)
})

test_that("shared nonresponse wins over option unavailability", {
  s <- react_synthetic(n_per_round=3L); d <- react_dictionary(); x <- s$profile$response_options
  o <- d$occurrences[d$occurrences$round_id=="react1.r17",]
  id <- o$occurrence_id[o$variable=="VACCRUFUSE1_14"]
  context <- list(age=c(12,55,30),nonresponse=c(TRUE,FALSE,FALSE))
  data <- data.frame(U_GENDER=c(2,2,1),INDCONF=1,VACCRUFUSE1_14=1)
  z <- .apply_response_options(data,o,context,x,list())
  z <- .participation_enforce(z$data,o,s,context,z$reasons)
  expect_equal(z$data$VACCRUFUSE1_14,c(-77,NA,NA))
  expect_identical(z$reasons$VACCRUFUSE1_14,c("survey_nonresponse","structural_skip_option","structural_skip_option"))
})

test_that("option parents are internal and selection does not change values", {
  s <- react_synthetic(n_per_round=45L,seed=603L); d <- react_dictionary()
  o <- d$occurrences[d$occurrences$round_id=="react1.r17" & d$occurrences$variable=="VACCRUFUSE2_14",]
  needed <- .synthetic_generation_occurrences(d,o,"react1.r17",s)
  expect_true(all(c("U_GENDER","U_AGE") %in% needed$variable))
  narrow <- react_extract(s,rounds="REACT1_R17",concepts=o$primary_concept_id,output="both",progress=FALSE)
  all <- react_extract(s,rounds="REACT1_R17",output="both",progress=FALSE)
  expect_lt(ncol(narrow$raw_data),ncol(all$raw_data))
  for (name in intersect(names(narrow$raw_data),names(all$raw_data)))
    expect_identical(narrow$raw_data[[name]],all$raw_data[[name]],info=name)
  h <- narrow$harmonised_values
  expect_true(any(h$missing_reason %in% c("structural_skip_option","structural_skip_questionnaire")))
  m <- stats::setNames(all$manifest$value,all$manifest$key)
  expect_identical(m[["synthetic_response_option_field_rounds"]],"109")
  rows <- all$raw_data
  selected <- !is.na(rows$VACCRUFUSE2_14)&rows$VACCRUFUSE2_14==1
  expect_true(all(rows$U_GENDER[selected]==2 & rows$U_AGE[selected]>=18 & rows$U_AGE[selected]<55))
})

test_that("option-specific provenance survives harmonisation when the question was asked", {
  h <- data.frame(observation_id=c("a","b"),source_raw_variable="OPTION",missing_reason="input_missing")
  obs <- data.frame(observation_id=c("a","b"))
  reasons <- list(OPTION=c("structural_skip_option","structural_skip_questionnaire"))
  expect_identical(.participation_harmonised_reasons(h,obs,reasons)$missing_reason,reasons$OPTION)
})
