# People to generate synthetic HES records for ------------------------------------

# One row per SUBJECT_ID with sex ("1" male, "2" female), age, the date the
# age applies to and HES ETHNOS code. `x` is an extraction result with the
# people-households/demographics family, or a data frame with SUBJECT_ID, sex
# ("Male"/"Female", "M"/"F" or "1"/"2"), age and optionally age_date and
# ethnicity (reactextract labels or ETHNOS codes). REACT fieldwork ran from
# May 2020 to March 2022, so ages default to 1 April 2021.
.hes_people <- function(x, age_date = as.Date("2021-04-01")) {
  if (inherits(x, "react_extract_result")) {
    d <- x$data
    field <- function(name) {
      col <- paste0("people_households.demographics.", name)
      if (!col %in% names(d)) {
        stop("The reactextract result needs the people-households/demographics family (missing ",
             col, ").", call. = FALSE)
      }
      d[[col]]
    }
    x <- data.frame(
      SUBJECT_ID = d$SUBJECT_ID,
      sex = field("gender"),
      age = field("age__field__U_AGE"),
      ethnicity = field("ethnic_group__field__ETHNIC"),
      stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(x)) stop("`x` must be a reactextract result or a data frame.", call. = FALSE)
  missing_cols <- setdiff(c("SUBJECT_ID", "sex", "age"), names(x))
  if (length(missing_cols) > 0) {
    stop("`x` is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  x <- x[!is.na(x$SUBJECT_ID) & !duplicated(x$SUBJECT_ID), , drop = FALSE]

  sex <- toupper(trimws(as.character(x$sex)))
  sex_code <- rep(NA_character_, nrow(x))
  sex_code[sex %in% c("MALE", "M", "1")] <- "1"
  sex_code[sex %in% c("FEMALE", "F", "2")] <- "2"

  ethnicity <- if ("ethnicity" %in% names(x)) as.character(x$ethnicity) else rep(NA_character_, nrow(x))
  map <- .hes_profile()$ethnicity$map
  hes_code <- ifelse(ethnicity %in% names(map), map[ethnicity], NA_character_)
  is_code <- ethnicity %in% c(unique(map), "Z", "99")
  hes_code[is_code] <- ethnicity[is_code]
  hes_code[ethnicity %in% "Prefer not to say"] <- "Z"

  dates <- if ("age_date" %in% names(x)) as.Date(x$age_date) else rep(as.Date(age_date), nrow(x))
  dates[is.na(dates)] <- as.Date(age_date)

  data.frame(
    SUBJECT_ID = x$SUBJECT_ID,
    sex = sex_code,
    age = as.numeric(x$age),
    age_date = dates,
    ethnicity = unname(hes_code),
    stringsAsFactors = FALSE
  )
}
