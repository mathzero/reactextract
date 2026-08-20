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

.add_simple_column <- function(output, values, name, observation_lookup = NULL) {
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
  index <- if (is.null(observation_lookup)) {
    match(values$observation_id, output$observation_id)
  } else {
    unname(observation_lookup[values$observation_id])
  }
  aligned[index[!is.na(index)]] <- column[!is.na(index)]
  output[[name]] <- aligned
  output
}

.make_simple_raw_data <- function(observations, raw_values) {
  output <- .simple_identifiers(observations)
  if (nrow(raw_values) == 0L) {
    return(output)
  }
  observation_lookup <- stats::setNames(
    seq_len(nrow(observations)), observations$observation_id
  )
  groups <- split(
    seq_len(nrow(raw_values)),
    factor(raw_values$raw_variable, levels = unique(raw_values$raw_variable)),
    drop = TRUE
  )
  for (variable in names(groups)) {
    values <- raw_values[groups[[variable]], , drop = FALSE]
    output <- .add_simple_column(
      output, values, variable,
      observation_lookup = observation_lookup
    )
  }
  output
}

.make_simple_harmonised_data <- function(observations, harmonised_values,
                                         requested_columns = NULL) {
  output <- .simple_identifiers(observations)
  columns <- unique(c(requested_columns, harmonised_values$output_column))
  columns <- columns[!is.na(columns) & nzchar(columns)]
  observation_lookup <- stats::setNames(
    seq_len(nrow(observations)), observations$observation_id
  )
  groups <- if (nrow(harmonised_values) == 0L) {
    list()
  } else {
    split(
      seq_len(nrow(harmonised_values)),
      factor(
        harmonised_values$output_column,
        levels = unique(harmonised_values$output_column)
      ),
      drop = TRUE
    )
  }
  for (column_name in columns) {
    indices <- groups[[column_name]]
    if (is.null(indices)) {
      output[[column_name]] <- rep(NA, nrow(output))
    } else {
      values <- harmonised_values[indices, , drop = FALSE]
      output <- .add_simple_column(
        output, values, column_name,
        observation_lookup = observation_lookup
      )
    }
  }
  output
}
