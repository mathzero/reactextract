people <- data.frame(
  SUBJECT_ID = sprintf("P%05d", 1:3000),
  sex = rep(c("Male", "Female", NA), 1000),
  age = rep(c(8, 25, 47, 66, 88), 600),
  ethnicity = rep(c("Indian", "English / Welsh / Scottish / Northern Irish / British", NA), 1000),
  stringsAsFactors = FALSE
)
source <- react_synthetic(n_per_round = c(REACT1_R08 = 10L), seed = 1L)
hes <- react_hes(source, people, seed = 11)
apc <- hes$NHSE_HES_ADMITTED_PATIENTS
op <- hes$NHSE_HES_OUTPATIENTS
profile <- .hes_profile()

expected_class <- function(type) switch(type, DATE = "POSIXct", NUMBER = "numeric", "character")

test_that("tables have REACT's names, column order and types", {
  expect_named(hes, c("NHSE_HES_ADMITTED_PATIENTS", "NHSE_HES_OUTPATIENTS"))
  for (table in c("apc", "op")) {
    d <- hes[[profile$table_names[[table]]]]
    schema <- profile$schema[[table]]
    expect_identical(names(d), schema$column)
    classes <- vapply(d, function(x) class(x)[1], character(1))
    expected <- vapply(schema$type, expected_class, character(1))
    expected[schema$column == "SUBJECT_ID"] <- "character"
    expect_identical(unname(classes), unname(expected))
  }
})

test_that("records belong to the people supplied and keys are unique", {
  expect_true(all(apc$SUBJECT_ID %in% people$SUBJECT_ID))
  expect_true(all(op$SUBJECT_ID %in% people$SUBJECT_ID))
  expect_false(anyDuplicated(apc$EPIKEY) > 0)
  expect_false(anyDuplicated(op$ATTENDKEY) > 0)
  expect_gt(nrow(apc), 1000)
  expect_gt(nrow(op), 10000)
})

test_that("the same seed gives the same data and the global RNG is untouched", {
  set.seed(99)
  before <- .Random.seed
  again <- react_hes(source, people[1:300, ], seed = 4)
  expect_identical(.Random.seed, before)
  expect_identical(again, react_hes(source, people[1:300, ], seed = 4))
  expect_false(identical(again, react_hes(source, people[1:300, ], seed = 5)))
})

test_that("dates are ordered and inside REACT's HES coverage", {
  expect_true(all(apc$ADMIDATE <= apc$EPISTART))
  expect_true(all(apc$EPISTART <= apc$EPIEND))
  expect_true(all(apc$EPIEND <= apc$DISDATE))
  expect_equal(as.numeric(apc$EPIEND - apc$EPISTART, units = "days"), apc$EPIDUR)
  start <- as.POSIXct(profile$window[["start"]], tz = "UTC")
  end <- as.POSIXct(profile$window[["end"]], tz = "UTC")
  expect_true(all(apc$ADMIDATE >= start & apc$DISDATE <= end))
  expect_true(all(op$APPTDATE >= start & op$APPTDATE <= end))
  expect_true(all(op$REQDATE <= op$APPTDATE))
})

test_that("spells are made of consecutive episodes", {
  spell <- paste(apc$SUBJECT_ID, apc$ADMIDATE)
  first <- apc$EPIORDER == 1
  expect_true(all(tapply(apc$EPIORDER, spell, function(x) identical(sort(x), as.numeric(seq_along(x))))))
  expect_true(all(apc$SPELBGIN[first] == "2"))
  expect_true(all(apc$DISMETH[apc$SPELEND == "N"] == "8"))
  expect_true(all(is.na(apc$SPELDUR[apc$SPELEND == "N"])))
  day_case <- apc$CLASSPAT %in% c("2", "3")
  expect_true(all(apc$EPIDUR[day_case] == 0))
  expect_true(all(apc$EPIORDER[day_case] == 1))
})

test_that("each person keeps one sex and a consistent age", {
  # REACT's outpatient table has no SEX column
  expect_true(all(tapply(apc$SEX, apc$SUBJECT_ID, function(x) length(unique(x)) == 1)))
  age_2021 <- people$age[match(apc$SUBJECT_ID, people$SUBJECT_ID)]
  years <- as.numeric(apc$EPISTART - as.POSIXct("2021-04-01", tz = "UTC"), units = "days") / 365.25
  adult <- apc$STARTAGE < 7000
  expect_true(all(abs(apc$STARTAGE[adult] - (age_2021[adult] + years[adult])) <= 1.01))
  expect_true(all(apc$ADMIAGE <= apc$STARTAGE | apc$ADMIAGE >= 7000))
})

test_that("nothing happens after an in-hospital death", {
  died <- apc$DISMETH == "4"
  death <- tapply(apc$DISDATE[died], apc$SUBJECT_ID[died], min)
  expect_true(length(death) > 0)
  after_apc <- apc$ADMIDATE > death[apc$SUBJECT_ID]
  after_op <- op$APPTDATE > death[op$SUBJECT_ID]
  expect_false(any(after_apc, na.rm = TRUE))
  expect_false(any(after_op, na.rm = TRUE))
  expect_true(all(apc$DISDEST[died] == "79"))
})

test_that("diagnosis and procedure fields agree with each other", {
  expect_true(all(substr(apc$DIAG_4_01, 1, 3) == apc$DIAG_3_01))
  expect_true(all(startsWith(apc$DIAG_4_CONCAT, apc$DIAG_4_01)))
  expect_true(all(apc$DIAG_COUNT >= 1))
  stored <- rowSums(!is.na(apc[sprintf("DIAG_4_%02d", 1:12)]))
  expect_equal(stored, pmin(apc$DIAG_COUNT, 12))
  none <- apc$OPERTN_01 == "-"
  expect_true(all(apc$OPERTN_4_01[none] %in% c("-", "-   ")))
  expect_true(all(is.na(apc$OPERTN_COUNT[none])))
  expect_true(all(is.na(apc$OPDATE_01[none])))
  expect_true(all(apc$OPDATE_01[!none] >= apc$EPISTART[!none] & apc$OPDATE_01[!none] <= apc$EPIEND[!none]))
  expect_true(all(grepl("^[A-Z][0-9]{2}[0-9A-Z]$", apc$DIAG_4_01)))
})

test_that("COVID-19 codes appear only from 2020", {
  covid <- grepl("U07", apc$DIAG_4_CONCAT)
  expect_true(any(covid))
  expect_true(all(apc$EPISTART[covid] >= as.POSIXct("2020-02-01", tz = "UTC")))
  expect_false(any(grepl("^U", apc$DIAG_4_01[!covid])))
})

test_that("maternity fields appear only on maternity episodes", {
  maternity <- apc$EPITYPE %in% c("2", "3")
  expect_true(all(is.na(apc$DELMETH_1[!maternity])))
  expect_true(all(is.na(apc$BIRWEIT_1[!maternity])))
  expect_false(any(apc$EPITYPE == "2" & apc$SEX == "1"))
})

test_that("outpatient courses start with a first attendance", {
  first <- op$FIRSTATT %in% c("1", "3")
  expect_true(all(!is.na(op$WAITDAYS[first])))
  expect_true(all(is.na(op$WAITDAYS[!first])))
  expect_true(all(op$DIAG_4_01[op$NODIAGS == 0] == "R69X"))
  expect_true(all(op$OPERTN_01[op$NOPROCS == 0] == "-"))
})

test_that("the HES profile is loaded only with its approval record", {
  approval <- .read_literal_csv(system.file("extdata", "hes-profile-approval.csv", package = "reactextract"))
  expect_identical(approval$approval_status, "formally_approved")
  expect_identical(approval$profile_sha256, .hes_profile_sha256)
})
