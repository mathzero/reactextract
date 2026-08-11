.empty_issues <- function() {
  data.frame(
    severity = character(),
    stage = character(),
    code = character(),
    round_id = character(),
    source_object = character(),
    variable = character(),
    affected_count = integer(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

.issue <- function(severity, stage, code, message, round_id = "",
                   source_object = "", variable = "", affected_count = NA_integer_) {
  data.frame(
    severity = severity,
    stage = stage,
    code = code,
    round_id = round_id,
    source_object = source_object,
    variable = variable,
    affected_count = as.integer(affected_count),
    message = message,
    stringsAsFactors = FALSE
  )
}

.bind_rows <- function(items, template = NULL) {
  items <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, items)
  if (length(items) == 0L) {
    if (!is.null(template)) {
      return(template)
    }
    return(data.frame())
  }
  names_union <- unique(unlist(lapply(items, names), use.names = FALSE))
  items <- lapply(items, function(item) {
    missing <- setdiff(names_union, names(item))
    for (name in missing) {
      item[[name]] <- NA
    }
    item[names_union]
  })
  rownames_out <- NULL
  result <- do.call(rbind, items)
  row.names(result) <- rownames_out
  result
}

.single_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", name, "` must be one non-empty string.", call. = FALSE)
  }
  x
}

.typed_value_frame <- function(value) {
  n <- length(value)
  out <- data.frame(
    value_type = rep("character", n),
    value_logical = rep(NA, n),
    value_integer = rep(NA_integer_, n),
    value_double = rep(NA_real_, n),
    value_date = as.Date(rep(NA_character_, n)),
    value_character = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
  present <- !is.na(value)
  if (inherits(value, "Date")) {
    out$value_type[] <- "date"
    out$value_date[present] <- value[present]
  } else if (is.logical(value)) {
    out$value_type[] <- "logical"
    out$value_logical[present] <- value[present]
  } else if (is.integer(value)) {
    out$value_type[] <- "integer"
    out$value_integer[present] <- value[present]
  } else if (is.numeric(value)) {
    out$value_type[] <- "double"
    out$value_double[present] <- as.double(value[present])
  } else if (inherits(value, "POSIXt")) {
    out$value_character[present] <- format(
      value[present], "%Y-%m-%dT%H:%M:%OS6%z", tz = "UTC"
    )
  } else {
    out$value_character[present] <- as.character(value[present])
  }
  out
}

.typed_to_character <- function(data) {
  out <- rep(NA_character_, nrow(data))
  types <- data$value_type
  out[types == "logical"] <- as.character(data$value_logical[types == "logical"])
  out[types == "integer"] <- as.character(data$value_integer[types == "integer"])
  out[types == "double"] <- as.character(data$value_double[types == "double"])
  out[types == "date"] <- as.character(data$value_date[types == "date"])
  out[types == "character"] <- data$value_character[types == "character"]
  out
}

.typed_nonmissing_count <- function(data) {
  rowSums(cbind(
    !is.na(data$value_logical),
    !is.na(data$value_integer),
    !is.na(data$value_double),
    !is.na(data$value_date),
    !is.na(data$value_character)
  ))
}

.round_significant <- function(x, digits = 3L) {
  ifelse(is.na(x) | x == 0, x, signif(x, digits = digits))
}
