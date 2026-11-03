# Synthetic Clinical Study omics values -----------------------------------------

# A samples-by-features matrix for one omics platform. The kit summarises
# each platform's rank-based normal scores by eigenvalues and the loadings of
# up to ten components (33_omics_eigen.csv, 34_omics_loadings.csv), so a
# sample's normal scores are sum_k sqrt(eigenvalue_k) * s_k * loading_k plus
# independent noise that tops each feature's variance up to one. Each
# feature's score is then read through its released quantiles
# (31_omics_features.csv), and set missing at the feature's share missing.
# Features without two released quantiles are missing throughout.
.clinical_omics_matrix <- function(profile, platform, n, source = NULL) {
  pick <- function(data) {
    keep <- data$platform == platform
    if (!is.null(source)) keep <- keep & data$source == source
    data[keep, , drop = FALSE]
  }
  features <- pick(profile$omics_features)
  if (nrow(features) == 0L) stop("The clinical profile has no features for platform ", platform, ".", call. = FALSE)
  eigen_rows <- pick(profile$omics_eigen)
  eigen_rows <- eigen_rows[order(eigen_rows$pc), , drop = FALSE]
  loadings <- pick(profile$omics_loadings)

  n_features <- nrow(features)
  z <- matrix(stats::rnorm(n * n_features), n, n_features)
  pcs <- intersect(paste0("pc", eigen_rows$pc), names(loadings))
  if (length(pcs) > 0L) {
    lambda <- eigen_rows$eigenvalue[match(pcs, paste0("pc", eigen_rows$pc))]
    lambda[is.na(lambda) | lambda < 0] <- 0
    l <- as.matrix(loadings[match(features$feature, loadings$feature), pcs, drop = FALSE])
    l[is.na(l)] <- 0
    weighted <- l * rep(sqrt(lambda), each = n_features)
    common_variance <- rowSums(weighted^2)
    residual_sd <- sqrt(pmax(1 - common_variance, 0.01))
    scores <- matrix(stats::rnorm(n * length(pcs)), n, length(pcs))
    z <- scores %*% t(weighted) + z * rep(residual_sd, each = n)
    z <- z / rep(sqrt(common_variance + residual_sd^2), each = n)
  }
  u <- stats::pnorm(z)
  out <- matrix(NA_real_, n, n_features, dimnames = list(NULL, features$feature))
  for (j in seq_len(n_features)) {
    quantile <- .clinical_quantile_fun(unlist(features[j, .clinical_quantile_columns], use.names = FALSE))
    if (is.null(quantile)) next
    value <- quantile(u[, j])
    missing <- features$share_missing[[j]]
    if (!is.na(missing) && missing > 0) value[stats::runif(n) < missing] <- NA
    out[, j] <- value
  }
  out
}
