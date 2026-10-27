# Broad sanity bounds on the default synthetic profile. Structural tests cannot
# detect a profile whose categories are permuted or collapsed, so these check
# that key table-one variables stay recognisably REACT-like.

test_that("the default synthetic profile gives plausible key distributions", {
  rounds <- c("REACT1_R08", "REACT1_R17", "REACT2_S5_R05")
  result <- react_extract(
    react_synthetic(n_per_round = 3000L, seed = 2026L),
    concepts = c(
      "infection_measurement.laboratory.interim_result",
      "infection_measurement.testing.antibody_test_result",
      "people_households.demographics.gender",
      "people_households.demographics.ethnic_group",
      "people_households.demographics.country_of_birth",
      "people_households.housing.area_deprivation_decile",
      "people_households.housing.area_deprivation_quintile",
      "people_households.housing.residential_region",
      "behaviours_mobility.behaviours.current_cigarette_smoking"
    ),
    rounds = rounds,
    progress = FALSE
  )
  data <- result$data
  raw <- result$raw_data
  share <- function(values, keep, matches) {
    values <- values[keep & !is.na(values)]
    mean(matches(values))
  }

  for (survey in rounds) {
    keep <- data$survey_id == survey
    female <- share(data$people_households.demographics.gender, keep,
      function(x) x == "Female")
    white_british <- share(
      data$people_households.demographics.ethnic_group__field__ETHNIC, keep,
      function(x) startsWith(x, "English / Welsh / Scottish")
    )
    smoking <- data$behaviours_mobility.behaviours.current_cigarette_smoking
    smokes <- share(smoking, keep, function(x) x == "Yes")
    declines <- share(smoking, keep, function(x) x == "Prefer not to say")

    expect_gt(female, 0.45, label = paste(survey, "female share"))
    expect_lt(female, 0.65, label = paste(survey, "female share"))
    expect_gt(white_british, 0.6, label = paste(survey, "White British share"))
    expect_gt(smokes, 0.03, label = paste(survey, "current smoking share"))
    expect_lt(smokes, 0.25, label = paste(survey, "current smoking share"))
    expect_lt(declines, 0.1, label = paste(survey, "smoking 'Prefer not to say' share"))
  }

  keep <- data$survey_id == "REACT1_R08"
  england <- share(
    data$people_households.demographics.country_of_birth__field__CTRYBORN, keep,
    function(x) x == "England"
  )
  expect_gt(england, 0.6, label = "REACT1_R08 born-in-England share")

  # Complementary suppression in the outcome tables must not erase large groups.
  imd_quintile <- react_derive_imd_quintile(result)
  region <- data$people_households.housing.residential_region__field__REGION
  for (survey in c("REACT1_R08", "REACT1_R17")) {
    keep <- data$survey_id == survey
    expect_gt(mean(imd_quintile[keep] %in% 1L), 0.05,
      label = paste(survey, "most-deprived IMD quintile share"))
    expect_gt(mean(grepl("North East", region[keep], fixed = TRUE)), 0.02,
      label = paste(survey, "North East share"))
  }

  for (survey in c("REACT1_R08", "REACT1_R17")) {
    result_values <- raw$RESULT[raw$survey_id == survey]
    evaluable <- result_values %in% c("Detected", "Not Detected")
    positivity <- mean(result_values[evaluable] == "Detected")
    expect_gt(sum(evaluable), 1000L)
    expect_gt(positivity, 0.002, label = paste(survey, "PCR positivity"))
    expect_lt(positivity, 0.08, label = paste(survey, "PCR positivity"))
  }

  keep <- data$survey_id == "REACT2_S5_R05"
  quintile <- data$people_households.housing.area_deprivation_quintile[keep]
  expect_lt(max(prop.table(table(quintile))), 0.5,
    label = "REACT2_S5_R05 largest deprivation-quintile share")
  igg <- share(
    data$infection_measurement.testing.antibody_test_result__field__NEWRESULT, keep,
    function(x) grepl("Ig G Positive", x, fixed = TRUE)
  )
  expect_gt(igg, 0.03, label = "REACT2_S5_R05 IgG-positive share")
  expect_lt(igg, 0.3, label = "REACT2_S5_R05 IgG-positive share")
})
