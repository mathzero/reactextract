.extract_clock <- function() {
  unname(proc.time()[["elapsed"]])
}

.elapsed_seconds <- function(started) {
  max(0, .extract_clock() - started)
}

.format_seconds <- function(seconds) {
  sprintf("%.1f s", seconds)
}

.format_records <- function(n) {
  format(as.numeric(n), big.mark = ",", scientific = FALSE, trim = TRUE)
}

.progress_message <- function(enabled, ...) {
  if (isTRUE(enabled)) {
    message("[reactextract] ", paste0(...))
  }
  invisible(NULL)
}

.progress_stage_start <- function(enabled, stage) {
  .progress_message(enabled, "Stage: ", stage)
}

.progress_stage_done <- function(enabled, stage, stage_seconds, total_started) {
  .progress_message(
    enabled,
    "Stage complete: ", stage,
    " | stage ", .format_seconds(stage_seconds),
    " | elapsed ", .format_seconds(.elapsed_seconds(total_started))
  )
}

.timing_manifest_rows <- function(timings) {
  if (length(timings) == 0L) {
    return(data.frame(key = character(), value = character()))
  }
  data.frame(
    key = paste0("timing_", names(timings), "_seconds"),
    value = sprintf("%.3f", as.numeric(timings)),
    stringsAsFactors = FALSE
  )
}
