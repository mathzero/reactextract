#' Search REACT variables by keyword
#'
#' Finds reviewed concepts whose identifier, label or definition, or whose raw
#' field names or questionnaire wording, contain a search term. Each row is one
#' concept: pass its `concept_id` to the `concepts` argument of
#' [react_extract()]. The [REACT data wiki](https://mathzero.github.io/react_wiki/)
#' offers the same search with full question wording and response options.
#'
#' @param pattern Text to search for. Case is ignored.
#' @param study `"all"`, `"react1"` or `"react2"`.
#' @param regex Treat `pattern` as a regular expression instead of plain text.
#' @return A data frame with one row per matching concept and columns
#'   `concept_id`, `label`, `family`, `fields` (raw field names), `rounds`
#'   (rounds with at least one field), `question` (typical question wording)
#'   and `matched_in` (where the term was found).
#' @examples
#' react_search("smok")
#' react_search("vaccin", study = "react2")
#' hits <- react_search("U_AGE")
#' hits$concept_id
#' @export
react_search <- function(pattern, study = c("all", "react1", "react2"),
                         regex = FALSE) {
  pattern <- .single_string(pattern, "pattern")
  study <- match.arg(study)
  if (!is.logical(regex) || length(regex) != 1L || is.na(regex)) {
    stop("`regex` must be TRUE or FALSE.", call. = FALSE)
  }
  matches <- function(text) {
    text[is.na(text)] <- ""
    if (regex) {
      grepl(pattern, text, ignore.case = TRUE, perl = TRUE)
    } else {
      grepl(tolower(pattern), tolower(text), fixed = TRUE)
    }
  }

  dictionary <- react_dictionary()
  concepts <- dictionary$concepts
  rounds <- dictionary$rounds
  occurrences <- dictionary$occurrences
  if (study != "all") {
    occurrences <- occurrences[
      occurrences$round_id %in% rounds$round_id[rounds$study_id == study], ,
      drop = FALSE
    ]
  }

  in_concept <- concepts$concept_id[matches(paste(
    concepts$concept_id, concepts$preferred_label, concepts$definition
  ))]
  in_field <- unique(occurrences$primary_concept_id[matches(occurrences$variable)])
  in_question <- unique(occurrences$primary_concept_id[matches(occurrences$label)])
  ids <- intersect(
    union(union(in_concept, in_field), in_question),
    occurrences$primary_concept_id
  )
  concepts <- concepts[concepts$concept_id %in% ids, , drop = FALSE]

  rows <- lapply(concepts$concept_id, function(id) {
    selected <- occurrences[occurrences$primary_concept_id == id, , drop = FALSE]
    fields <- unique(selected$variable)
    # Describe the concept by the wording of its most widely used field.
    main_field <- fields[[which.max(tabulate(match(selected$variable, fields)))]]
    main <- selected[selected$variable == main_field, , drop = FALSE]
    wording <- main$label[main$label != main$variable & nzchar(main$label)]
    if (length(wording) == 0L) wording <- main$label
    found <- c(
      concept = id %in% in_concept,
      field = id %in% in_field,
      question = id %in% in_question
    )
    data.frame(
      fields = .search_collapse(fields),
      rounds = .search_rounds(selected$round_id, rounds),
      question = names(sort(table(wording), decreasing = TRUE))[[1L]],
      matched_in = paste(names(found)[found], collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  output <- data.frame(
    concept_id = concepts$concept_id,
    label = concepts$preferred_label,
    family = vapply(concepts$primary_taxonomy_id, .family_key, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  details <- if (length(rows) > 0L) {
    do.call(rbind, rows)
  } else {
    data.frame(
      fields = character(), rounds = character(), question = character(),
      matched_in = character(), stringsAsFactors = FALSE
    )
  }
  output <- cbind(output, details)
  # Concept-name matches first, then raw-field matches, then wording-only matches.
  relevance <- ifelse(output$concept_id %in% in_concept, 1L,
    ifelse(output$concept_id %in% in_field, 2L, 3L))
  output <- output[order(relevance, output$family, output$label), , drop = FALSE]
  rownames(output) <- NULL
  attr(output, "pattern") <- pattern
  class(output) <- c("react_search_result", "data.frame")
  output
}

.search_collapse <- function(values, limit = 6L) {
  if (length(values) <= limit) return(paste(values, collapse = ", "))
  paste0(
    paste(values[seq_len(limit)], collapse = ", "),
    ", ... (+", length(values) - limit, " more)"
  )
}

.search_rounds <- function(round_ids, rounds) {
  selected <- rounds[rounds$round_id %in% round_ids, , drop = FALSE]
  parts <- vapply(c("react1", "react2"), function(study) {
    numbers <- sort(as.integer(selected$round_number[selected$study_id == study]))
    if (length(numbers) == 0L) return(NA_character_)
    runs <- split(numbers, cumsum(c(1L, diff(numbers) != 1L)))
    ranges <- vapply(runs, function(run) {
      if (length(run) == 1L) as.character(run) else paste0(run[[1L]], "-", run[[length(run)]])
    }, character(1))
    paste0(if (study == "react1") "REACT-1 " else "REACT-2 ", paste(ranges, collapse = ", "))
  }, character(1))
  paste(parts[!is.na(parts)], collapse = "; ")
}

#' @export
print.react_search_result <- function(x, n = 20L, ...) {
  pattern <- attr(x, "pattern")
  total <- nrow(x)
  if (total == 0L) {
    cat("No REACT concepts match \"", pattern, "\".\n", sep = "")
    return(invisible(x))
  }
  cat(total, " concept", if (total > 1L) "s", " match \"", pattern, "\":\n", sep = "")
  for (i in seq_len(min(n, total))) {
    question <- x$question[[i]]
    if (nchar(question) > 90L) question <- paste0(substr(question, 1L, 87L), "...")
    cat("\n", x$concept_id[[i]], "\n", sep = "")
    cat("  ", x$label[[i]], "  [", x$family[[i]], "]\n", sep = "")
    cat("  Fields: ", x$fields[[i]], "\n", sep = "")
    cat("  Rounds: ", x$rounds[[i]], "\n", sep = "")
    cat("  Question: ", question, "\n", sep = "")
  }
  if (total > n) {
    cat("\nShowing ", n, " of ", total, ". Use View() or as.data.frame() to see all.\n", sep = "")
  }
  invisible(x)
}
