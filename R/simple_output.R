.simple_values <- function(data) {
  if (nrow(data) == 0L) {
    return(logical())
  }
  types <- unique(data$value_type)
  types <- types[!is.na(types) & nzchar(types)]
  if (length(types) != 1L) {
    return(.typed_to_character(data))
  }
  switch(
    types[[1]],
    logical = data$value_logical,
    integer = data$value_integer,
    double = data$value_double,
    date = data$value_date,
    character = data$value_character,
    .typed_to_character(data)
  )
}

.simple_identifiers <- function(observations) {
  observations[c(
    "observation_id", "study_id", "round_id", "survey_id",
    "U_PASSCODE", "SUBJECT_ID", "visit_number", "total_visits"
  )]
}

.add_simple_column <- function(output, values, name) {
  if (anyDuplicated(values$observation_id)) {
    stop(
      "Cannot make the simple output because more than one value exists for `",
      name,
      "` in the same observation.",
      call. = FALSE
    )
  }
  column <- .simple_values(values)
  aligned <- rep(column[NA_integer_], nrow(output))
  index <- match(values$observation_id, output$observation_id)
  aligned[index[!is.na(index)]] <- column[!is.na(index)]
  output[[name]] <- aligned
  output
}

.make_simple_raw_data <- function(observations, raw_values) {
  output <- .simple_identifiers(observations)
  for (variable in unique(raw_values$raw_variable)) {
    values <- raw_values[raw_values$raw_variable == variable, , drop = FALSE]
    output <- .add_simple_column(output, values, variable)
  }
  output
}

.make_simple_harmonised_data <- function(observations, harmonised_values,
                                         requested_concepts = NULL) {
  output <- .simple_identifiers(observations)
  concepts <- unique(c(requested_concepts, harmonised_values$concept_id))
  concepts <- concepts[!is.na(concepts) & nzchar(concepts)]
  for (concept in concepts) {
    values <- harmonised_values[
      harmonised_values$concept_id == concept,
      ,
      drop = FALSE
    ]
    if (nrow(values) == 0L) {
      output[[concept]] <- rep(NA, nrow(output))
    } else {
      output <- .add_simple_column(output, values, concept)
    }
  }
  output
}
