# Compact enclave-only fitting. No full fitted R objects or training rows saved.
dm_with_seed <- function(seed, code) {
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had) assign(".Random.seed", old, .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed); force(code)
}
dm_seed <- function(seed, round, component) {
  as.integer(strtoi(substr(digest::digest(paste(seed, round, component), algo = "sha256", serialize = FALSE), 1, 7), 16L))
}
dm_design <- function(data, levels) {
  n <- nrow(data)
  columns <- list(`(Intercept)` = rep(1, n))
  for (field in names(levels)) {
    if (!field %in% names(data) || anyNA(data[[field]]) || any(!data[[field]] %in% levels[[field]]))
      stop("Unexpected modelling state for ", field)
    for (level in levels[[field]][-1L]) columns[[paste(field, level, sep = "=")]] <- as.numeric(data[[field]] == level)
  }
  do.call(cbind, columns)
}
dm_ridge <- function(x, y, lambda, initial = NULL) {
  n <- length(y)
  softplus <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))
  fn <- function(b) {
    z <- drop(x %*% b)
    mean(softplus(z) - y*z) + lambda*sum(b[-1L]^2)/2
  }
  gr <- function(b) drop(crossprod(x, stats::plogis(drop(x %*% b))-y))/n + c(0, lambda*b[-1L])
  if (is.null(initial)) initial <- c(stats::qlogis(mean(y)), rep(0, ncol(x)-1L))
  attempts <- list()
  for (limit in c(200L, 2000L)) {
    fit <- tryCatch(stats::optim(initial, fn, gr, method = "L-BFGS-B",
      control = list(maxit = limit, factr = 1e7)), error = identity)
    if (inherits(fit, "error")) {
      attempts[[length(attempts)+1L]] <- list(limit = limit, error = conditionMessage(fit))
      break
    }
    gradient <- max(abs(gr(fit$par)))
    attempts[[length(attempts)+1L]] <- list(limit = limit, convergence = fit$convergence,
      message = fit$message, evaluations = fit$counts, objective = fit$value,
      max_gradient = gradient)
    if (fit$convergence == 0L && all(is.finite(fit$par)) &&
        is.finite(fit$value) && is.finite(gradient) && gradient <= 1e-4) {
      b <- setNames(fit$par, colnames(x))
      attr(b, "fit_diagnostics") <- attempts
      return(b)
    }
    if (!all(is.finite(fit$par))) break
    initial <- fit$par
  }
  stop(structure(list(message = "Ridge fitting did not converge to the required tolerance.",
    call = NULL, diagnostics = attempts), class = c("dm_fit_error", "error", "condition")))
}
dm_predict <- function(model, data) {
  x <- dm_design(data, model$levels)
  if (!identical(colnames(x), names(model$coefficients))) stop("Model design columns differ.")
  stats::plogis(drop(x %*% model$coefficients))
}
dm_fit_outcome <- function(data, predictors, seed, min_class = 25L) {
  keep <- data$outcome %in% c("positive", "negative")
  d <- data[keep, , drop = FALSE]
  y <- as.integer(d$outcome == "positive")
  if (length(y) == 0L || min(sum(y), sum(1-y)) < min_class)
    return(list(status = "insufficient_outcome_support", model = NULL))
  train <- dm_with_seed(seed, {
    z <- rep(FALSE, length(y))
    for (state in 0:1) {
      i <- which(y == state)
      z[sample(i, max(1L, floor(.8*length(i))))] <- TRUE
    }
    z
  })
  # Select levels using training records only. Unseen validation states receive
  # an explicit unknown column with a shrunk coefficient, not row deletion.
  levels <- lapply(d[predictors], function(x) sort(unique(c(x[train], "model_unknown"))))
  levels <- levels[lengths(levels) > 2L]
  for (field in names(levels)) d[[field]][!d[[field]] %in% levels[[field]]] <- "model_unknown"
  x <- dm_design(d, levels)
  penalties <- c(.0001, .001, .01, .1)
  diagnostics <- list()
  attempt <- function(x, y, lambda, initial = NULL) {
    tryCatch({
      b <- dm_ridge(x, y, lambda, initial)
      diagnostics[[length(diagnostics)+1L]] <<- list(lambda = lambda, attempts = attr(b, "fit_diagnostics"))
      b
    }, error = function(e) {
      diagnostics[[length(diagnostics)+1L]] <<- list(lambda = lambda, error = conditionMessage(e), attempts = e$diagnostics)
      NULL
    })
  }
  fits <- lapply(penalties, function(lambda) attempt(x[train, , drop = FALSE], y[train], lambda))
  loss <- vapply(fits, function(b) {
    if (is.null(b)) return(Inf)
    p <- pmin(1-1e-10, pmax(1e-10, stats::plogis(drop(x[!train, , drop = FALSE] %*% b))))
    -mean(y[!train]*log(p) + (1-y[!train])*log1p(-p))
  }, numeric(1))
  if (!any(is.finite(loss))) return(list(status = "fitting_failed", model = NULL, diagnostics = diagnostics))
  best <- which.min(loss)
  p <- stats::plogis(drop(x[!train, , drop = FALSE] %*% fits[[best]]))
  validation <- data.frame(n = sum(!train), positive = sum(y[!train]),
    mean_prediction = mean(p), observed = mean(y[!train]), brier = mean((p-y[!train])^2),
    log_loss = loss[best], baseline_log_loss = -mean(y[!train]*log(mean(y[train]))+(1-y[!train])*log1p(-mean(y[train]))))
  calibration <- do.call(rbind, lapply(c("all", intersect(c("age_band", "vaccination_status", "vaccine_dose", "participation"), names(d))), function(field) {
    g <- if (field == "all") rep("all", sum(!train)) else d[[field]][!train]
    do.call(rbind, lapply(sort(unique(g)), function(level) {
      j <- g == level
      data.frame(field = field, level = level, n = sum(j), positive = sum(y[!train][j]), expected_positive = sum(p[j]))
    }))
  }))
  # Final refit uses the frozen training-derived design and chosen penalty.
  b <- attempt(x, y, penalties[best], initial = fits[[best]])
  if (is.null(b)) return(list(status = "refit_failed", model = NULL,
    diagnostics = diagnostics, validation = validation, calibration = calibration,
    penalty_scores = data.frame(lambda = penalties, log_loss = loss)))
  list(status = "fitted_pending_review", model = list(kind = "ridge_logistic_v1", levels = levels,
    coefficients = b, lambda = penalties[best], outcome = "positive_among_evaluable",
    interactions = "none_in_first_candidate"), validation = validation, calibration = calibration,
    diagnostics = diagnostics, penalty_scores = data.frame(lambda = penalties, log_loss = loss))
}
dm_table <- function(data, target, parents = character()) {
  parents <- intersect(parents, names(data))
  # Include progressively simpler tables as explicit, measurable fallbacks.
  sets <- lapply(seq.int(length(parents), 0L), function(k) head(parents, k))
  tables <- lapply(sets, function(ps) {
    d <- data[c(ps, target)]
    d$count <- 1L
    stats::aggregate(d$count, d[setdiff(names(d), "count")], sum) |>
      (function(z) {names(z)[ncol(z)] <- "count"; z})()
  })
  list(kind = "conditional_counts_internal", target = target, parents = sets, tables = tables)
}
dm_sample_table <- function(spec, data, seed) {
  n <- nrow(data); result <- rep(NA_character_, n); fallback <- 0L
  dm_with_seed(seed, {
    for (k in seq_along(spec$tables)) {
      remaining <- which(is.na(result)); if (!length(remaining)) break
      parents <- spec$parents[[k]]; tab <- spec$tables[[k]]
      make_key <- function(d) if (!length(parents)) rep("all", nrow(d)) else
        do.call(paste, c(d[parents], sep = "\r"))
      wanted <- make_key(data[remaining, , drop = FALSE]); have <- make_key(tab)
      for (g in unique(wanted)) {
        cells <- which(have == g); if (!length(cells)) next
        pos <- remaining[wanted == g]
        result[pos] <- sample(tab[[spec$target]][cells], length(pos), replace = TRUE, prob = tab$count[cells])
        if (k > 1L) fallback <- fallback + length(pos)
      }
    }
  })
  if (anyNA(result)) stop("No conditional-table fallback support.")
  list(value = result, fallback = fallback)
}
dm_fit_state_sampler <- function(data, predictors) {
  order <- intersect(c("age_band", "gender", "region", "ethnicity_broad", "imd_quintile", "participation",
    "registration", "household_size_band", "economic_activity_broad", "key_worker_care_role", "covid_history",
    "vaccination_status", "vaccine_dose", "vaccine_product", "vaccine_elapsed", "confirmed_contact"), predictors)
  tables <- list()
  for (field in order) {
    parents <- switch(field,
      age_band = character(), gender = "age_band", region = "age_band",
      ethnicity_broad = "region", imd_quintile = "region", participation = c("age_band", "gender"),
      registration = c("participation", "age_band"),
      vaccine_dose = c("vaccination_status", "age_band"),
      vaccine_product = c("vaccine_dose", "vaccination_status"),
      vaccine_elapsed = c("vaccine_dose", "vaccination_status"),
      confirmed_contact = c("participation", "household_size_band"),
      key_worker_care_role = c("economic_activity_broad", "age_band"),
      c("participation", "age_band"))
    tables[[field]] <- dm_table(data, field, intersect(parents, names(tables)))
  }
  tables$outcome_availability <- dm_table(data, "outcome_availability", intersect(c("participation", "age_band"), names(tables)))
  tables
}
dm_generate_states <- function(tables, model, n, seed, round) {
  d <- data.frame(row.names = seq_len(n)); fallbacks <- list()
  for (target in names(tables)) {
    z <- dm_sample_table(tables[[target]], d, dm_seed(seed, round, target))
    d[[target]] <- z$value; fallbacks[[target]] <- z$fallback
  }
  for (field in names(model$levels)) {
    if (!field %in% names(d)) stop("Sampler is missing a model predictor.")
    d[[field]][!d[[field]] %in% model$levels[[field]]] <- "model_unknown"
  }
  p <- dm_predict(model, d)
  d$outcome <- d$outcome_availability
  hit <- d$outcome_availability == "evaluable"
  d$outcome[hit] <- dm_with_seed(dm_seed(seed, round, "outcome"),
    ifelse(stats::runif(sum(hit)) < p[hit], "positive", "negative"))
  list(data = d, fallbacks = fallbacks)
}
