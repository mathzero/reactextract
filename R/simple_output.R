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

.simple_vector_type <- function(x) {
  if (inherits(x, "Date")) {
    return("date")
  }
  if (is.logical(x)) {
    return("logical")
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (is.double(x)) {
    return("double")
  }
  "character"
}

.simple_character <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%OS6%z", tz = "UTC"))
  }
  as.character(x)
}

.empty_simple_vector <- function(type, n) {
  switch(
    type,
    logical = rep(NA, n),
    integer = rep(NA_integer_, n),
    double = rep(NA_real_, n),
    date = as.Date(rep(NA_character_, n)),
    rep(NA_character_, n)
  )
}

.bind_simple_parts <- function(parts, column_order = NULL) {
  parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, parts)
  if (length(parts) == 0L) {
    columns <- unique(column_order)
    columns <- columns[!is.na(columns) & nzchar(columns)]
    out <- stats::setNames(
      lapply(columns, function(name) .empty_simple_vector("logical", 0L)),
      columns
    )
    return(as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE))
  }
  present_columns <- unique(unlist(lapply(parts, names), use.names = FALSE))
  columns <- unique(c(column_order, present_columns))
  columns <- columns[!is.na(columns) & nzchar(columns)]

  column_types <- stats::setNames(character(length(columns)), columns)
  for (column in columns) {
    types <- unique(vapply(
      Filter(Negate(is.null), lapply(parts, function(part) {
        if (column %in% names(part)) part[[column]] else NULL
      })),
      .simple_vector_type,
      character(1L)
    ))
    column_types[[column]] <- if (length(types) == 0L) {
      "logical"
    } else if (length(types) == 1L) {
      types[[1L]]
    } else {
      "character"
    }
  }

  output <- stats::setNames(vector("list", length(columns)), columns)
  for (column in columns) {
    target_type <- column_types[[column]]
    pieces <- lapply(parts, function(part) {
      if (!(column %in% names(part))) {
        return(.empty_simple_vector(target_type, nrow(part)))
      }
      value <- part[[column]]
      if (.simple_vector_type(value) != target_type) {
        return(.simple_character(value))
      }
      value
    })
    output[[column]] <- do.call(c, pieces)
  }
  as.data.frame(output, check.names = FALSE, stringsAsFactors = FALSE)
}

.sync_simple_identifiers <- function(data, observations) {
  identifiers <- .simple_identifiers(observations)
  if (nrow(data) == 0L && nrow(observations) > 0L) {
    return(identifiers)
  }
  if (nrow(data) != nrow(observations)) {
    stop("Internal error: wide output rows do not match observations.", call. = FALSE)
  }
  for (name in names(identifiers)) {
    data[[name]] <- identifiers[[name]]
  }
  data[c(names(identifiers), setdiff(names(data), names(identifiers)))]
}
