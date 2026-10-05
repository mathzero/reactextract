# Offline internal reporting only. Base R; no database or network operations.
dr_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- "unavailable"
  for (pair in list(c("&","&amp;"),c("<","&lt;"),c(">","&gt;"),c('"',"&quot;")))
    x <- gsub(pair[1],pair[2],x,fixed=TRUE)
  x
}
dr_table <- function(d) {
  if (is.null(d) || !nrow(d)) return("<p>Not available in this saved result.</p>")
  paste0("<div class='scroll'><table><thead><tr>",paste0("<th>",dr_escape(names(d)),"</th>",collapse=""),
    "</tr></thead><tbody>",paste(vapply(seq_len(nrow(d)),function(i)
      paste0("<tr>",paste0("<td>",dr_escape(unlist(d[i,,drop=FALSE])),"</td>",collapse=""),"</tr>"),character(1)),collapse=""),
    "</tbody></table></div>")
}
dr_plot <- function(z) {
  good <- is.finite(z$n) & z$n > 0 & is.finite(z$positive) & is.finite(z$expected_positive)
  z <- z[good,,drop=FALSE]
  if (!nrow(z)) return("<p>No plottable calibration values.</p>")
  observed <- z$positive/z$n; predicted <- z$expected_positive/z$n
  scale <- min(1,max(.01,observed,predicted)*1.15)
  height <- 55+34*nrow(z)
  lines <- c(sprintf("<svg role='img' aria-label='Held-out observed and predicted positivity; blue observed, orange predicted' viewBox='0 0 850 %s'>",height),
    "<text x='290' y='18'>Blue: observed · Orange: predicted</text>")
  for (i in seq_len(nrow(z))) {
    y <- 25+34*i
    lines <- c(lines,sprintf("<text x='5' y='%s'>%s</text>",y,dr_escape(substr(z$level[i],1,36))),
      sprintf("<rect x='290' y='%s' width='%.2f' height='7' fill='#1769aa'/>",y-14,370*observed[i]/scale),
      sprintf("<rect x='290' y='%s' width='%.2f' height='7' fill='#b85c00'/>",y-5,370*predicted[i]/scale),
      sprintf("<text x='680' y='%s'>%.2f%% / %.2f%%</text>",y,100*observed[i],100*predicted[i]))
  }
  paste(c(lines,"</svg>"),collapse="\n")
}
dr_assess <- function(z) {
  flags <- character(); add <- function(x) flags <<- c(flags,x)
  fit <- z$fit; v <- fit$validation; cal <- fit$calibration
  if (!identical(z$status,"fitted_pending_review")) add(paste("Fit status:",z$status))
  required <- c("n","positive","observed","mean_prediction","brier","log_loss","baseline_log_loss")
  valid <- is.data.frame(v) && nrow(v)==1L && all(required %in% names(v)) &&
    all(vapply(v[required],function(x) is.numeric(x) && all(is.finite(x)),logical(1)))
  if (!valid) add("Validation metrics missing or non-finite") else {
    if (v$n <= 0 || v$positive < 0 || v$positive > v$n ||
        any(c(v$observed,v$mean_prediction,v$brier)<0 | c(v$observed,v$mean_prediction,v$brier)>1))
      add("Validation metrics outside expected ranges")
    if (v$log_loss >= v$baseline_log_loss) add("Held-out log loss is no better than the constant-rate baseline")
    if (abs(v$mean_prediction-v$observed) > max(.005,.25*v$observed)) add("Overall predicted rate differs materially from observed rate")
    if (min(v$positive,v$n-v$positive)<25) add("Few held-out outcomes in one class; performance is uncertain")
  }
  if (is.null(cal) || !nrow(cal)) add("Group calibration unavailable") else {
    req <- c("field","level","n","positive","expected_positive")
    if (!all(req %in% names(cal))) stop("Malformed calibration table.")
    if (!all(vapply(cal[c("n","positive","expected_positive")],is.numeric,logical(1)))) stop("Non-numeric calibration values.")
    bad <- !is.finite(cal$n) | cal$n<=0 | !is.finite(cal$positive) | !is.finite(cal$expected_positive) |
      cal$positive<0 | cal$positive>cal$n | cal$expected_positive<0 | cal$expected_positive>cal$n
    if (any(bad)) add("Invalid group calibration values")
    c <- cal[!bad,,drop=FALSE]
    if (any(c$n<100 | pmin(c$positive,c$n-c$positive)<10)) add("Some calibration groups are small; inspect without drawing strong conclusions")
    large <- c$n>=100 & pmin(c$positive,c$n-c$positive)>=10
    if (any(large & abs(c$expected_positive/c$n-c$positive/c$n)>pmax(.005,.25*c$positive/c$n)))
      add("One or more supported groups have a material rate mismatch")
  }
  b <- fit$model$coefficients
  if (is.null(b) || !length(b) || any(!is.finite(b))) add("Coefficients missing or non-finite") else
    if (any(abs(b[names(b)!="(Intercept)"])>5)) add("Large fitted coefficient: inspect stability and rare levels")
  if (is.null(fit$diagnostics)) add("Solver diagnostics absent (expected for an unchanged original-v1 fit)")
  list(flags=unique(flags),valid=valid)
}
react_dependency_review <- function(input, output="dependency-review-INTERNAL.html") {
  # Never overwrite the source bundle or any existing report.
  if (file.exists(output)) stop("Choose a new report filename; existing files are not overwritten.")
  path <- file.path(input,"parameters-AND-COUNTS-NOT-APPROVED.rds")
  if (!file.exists(path)) stop("Select the prepared INTERNAL review bundle folder.")
  models <- readRDS(path)
  expected <- c(sprintf("react1.r%02d",1:19),sprintf("react2.r%02d",1:6))
  if (!is.list(models) || is.null(names(models)) || anyDuplicated(names(models)) ||
      any(!names(models) %in% expected)) stop("Unexpected saved model structure.")
  rows <- list(); sections <- character()
  for (round in expected) {
    z <- models[[round]]
    if (is.null(z)) {
      rows[[round]] <- data.frame(Round=round,Review="MISSING ROUND",Notes="No saved model")
      next
    }
    if (!identical(z$round_id,round)) stop("Round identity mismatch.")
    a <- dr_assess(z)
    rows[[round]] <- data.frame(Round=round,Review=if(length(a$flags)) "Inspect" else "No automated flags",
      Notes=if(length(a$flags)) paste(a$flags,collapse="; ") else "Manual review still required")
    sections <- c(sections,paste0("<section id='",round,"'><h2>",round,"</h2>"),
      paste0("<p>",dr_escape(rows[[round]]$Notes),"</p>"),"<h3>Held-out validation</h3>",dr_table(z$fit$validation))
    cal <- z$fit$calibration
    if (!is.null(cal) && nrow(cal)) for (field in unique(cal$field)) {
      group <- cal[cal$field==field,,drop=FALSE]
      sections <- c(sections,paste0("<h3>",dr_escape(field),"</h3>"),dr_plot(group),dr_table(group))
    }
    sections <- c(sections,"<details><summary>Penalty comparison and solver diagnostics</summary>",
      dr_table(z$fit$penalty_scores),"<pre>",dr_escape(paste(capture.output(print(z$fit$diagnostics)),collapse="\n")),"</pre></details>",
      "<details><summary>Recorded modelling limitations</summary><ul>",paste0("<li>",dr_escape(z$limitations),"</li>"),"</ul></details></section>")
  }
  summary <- do.call(rbind,rows)
  html <- c("<!doctype html><html lang='en'><meta charset='utf-8'><meta name='viewport' content='width=device-width'><title>Internal dependency model review</title>",
    "<style>body{font:16px system-ui,sans-serif;color:#243746;max-width:1150px;margin:30px auto;padding:20px}h1,h2{color:#173957}.warning{padding:20px;background:#fff1da;border:2px solid #a45c00}table{border-collapse:collapse;width:100%;font-size:14px}th,td{text-align:left;padding:9px;border-bottom:1px solid #ccd5df;vertical-align:top}th{background:#edf3f8}.scroll{overflow:auto}section{border-top:3px solid #ccd5df;margin-top:40px}svg{width:100%;max-width:950px}svg text{font-size:13px;fill:#243746}pre{white-space:pre-wrap}details{margin:20px 0}a{color:#1266a3}</style>",
    "<h1>Dependency models: internal review</h1><div class='warning'><strong>Keep inside the enclave.</strong> This report contains unsuppressed counts and model diagnostics. It is neither an authorised export nor a disclosure approval. Do not upload or copy it to OneDrive.</div>",
    "<p>These plots assess the held-out portion of the data using the training fit. They do not directly validate the final all-record refit or the full synthetic generator. The best penalty was selected on this same held-out set, so this is tuning performance, not independent final test performance.</p>",
    "<p>Lower log loss and Brier score are better. The baseline predicts one constant rate. Blue bars show observed positivity; orange bars show predicted positivity. Scales vary between plots. Exact counts are shown below each plot; no error bars or statistical significance are implied.</p>",
    "<details><summary>How flags are chosen</summary><p>These are triage heuristics, not acceptance thresholds: a rate difference exceeding the larger of 0.5 percentage points or 25% of observed positivity; fewer than 25 held-out outcomes in either class; group size below 100 or fewer than 10 outcomes in either class; absolute non-intercept coefficient above 5. Group mismatch flags use groups with at least 100 records and 10 in each class. A lack of flags is not evidence of disclosure safety or model adequacy.</p></details>",
    "<h2>All-round summary</h2>",dr_table(summary),
    paste0("<p>",paste0("<a href='#",names(models),"'>",names(models),"</a>",collapse=" · "),"</p>"),
    "<h2>Checks not available in this bundle</h2><p>The prepared bundle omits the original field-failure list, scan-completion flags, and synthetic-versus-real marginal comparisons. Consult the original run status and internal report for those checks. PREVREACT was reported unavailable in Round 19; the current model plan does not use it as an outcome, predictor or participation marker. This report does not validate joint symptom vectors, Ct/result consistency or final raw-data routing.</p>",
    "<p>Review every flagged round, then assess scientific adequacy and disclosure separately using REVIEWER.md. Do not change the released generator on the basis of a technically successful fit alone.</p>",sections,
    paste0("<footer>Generated locally ",dr_escape(as.character(Sys.time())),". Input file MD5 (integrity reference only): ",unname(tools::md5sum(path)),"</footer></html>"))
  writeLines(html,output,useBytes=TRUE)
  message("Internal report saved: ",normalizePath(output),". Keep inside the enclave.")
  invisible(summary)
}
