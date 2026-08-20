test_that("profiles contain aggregates but no respondent identifiers or raw text", {
  n <- 25L
  round <- data.frame(
    U_PASSCODE = paste0("p", seq_len(n)),
    HEALTHA05 = rep(c(0L, 1L), length.out = n),
    HEALTHA06 = rep(c(1L, 0L), length.out = n),
    U_POSTCODE = paste0("postcode ", seq_len(n)),
    check.names = FALSE
  )
  result <- react_extract(
    react_files(list(react1.r01 = round)),
    families = c("health/preexisting-conditions", "people-households/housing"),
    rounds = "react1.r01",
    output = "long"
  )
  profile <- react_profile(result)

  expect_gt(nrow(profile$categorical), 0L)
  expect_gt(nrow(profile$text_lengths), 0L)
  expect_false(any(vapply(profile, function(x) {
    is.data.frame(x) && any(c("U_PASSCODE", "SUBJECT_ID", "observation_id") %in% names(x))
  }, logical(1))))
  expect_false(any(grepl("postcode ", unlist(profile), fixed = TRUE)))
})

test_that("selected pairs produce correlations without automatic all-pairs", {
  n <- 30L
  round <- data.frame(
    U_PASSCODE = paste0("p", seq_len(n)),
    HEALTHA05 = rep(c(0L, 1L), length.out = n),
    HEALTHA06 = rep(c(1L, 0L), length.out = n),
    check.names = FALSE
  )
  result <- react_extract(
    react_files(list(react1.r01 = round)),
    families = "health/preexisting-conditions",
    rounds = "react1.r01",
    output = "long"
  )
  profile_without_pairs <- react_profile(result)
  expect_equal(nrow(profile_without_pairs$pair_crosstabs), 0L)
  profile <- react_profile(
    result,
    pairs = list(c("health.preexisting.overweight", "health.preexisting.stroke"))
  )
  expect_gt(nrow(profile$pair_crosstabs), 0L)
})

test_that("export preparation suppresses, complements, rounds, and requires review", {
  n <- 23L
  round <- data.frame(
    U_PASSCODE = paste0("p", seq_len(n)),
    HEALTHA05 = c(rep(0L, 11L), rep(1L, 12L)),
    check.names = FALSE
  )
  result <- react_extract(
    react_files(list(react1.r01 = round)),
    families = "health/preexisting-conditions",
    rounds = "react1.r01",
    concepts = "health.preexisting.overweight",
    output = "long"
  )
  export <- react_prepare_profile_export(react_profile(result))
  counts <- export$categorical$count
  expect_setequal(counts, c(10, 10))
  expect_identical(
    export$metadata$value[export$metadata$key == "status"],
    "requires_enclave_disclosure_review"
  )

  small_round <- round
  small_round$HEALTHA05 <- c(rep(0L, 20L), rep(1L, 3L))
  small <- react_extract(
    react_files(list(react1.r01 = small_round)),
    families = "health/preexisting-conditions",
    rounds = "react1.r01",
    concepts = "health.preexisting.overweight",
    output = "long"
  )
  suppressed <- react_prepare_profile_export(react_profile(small))$categorical
  expect_true(all(is.na(suppressed$count)))
  expect_true(all(suppressed$suppressed))
})
