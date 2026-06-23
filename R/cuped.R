# CUPED variance reduction. Replace each metric column Y with its adjusted version
#
#   Z = Y - theta * (X - mean(X)),   theta = cov(Y, X) / var(X)
#
# using a pre-experiment covariate X. theta and mean(X) are estimated once on the
# pooled data (every arm together), so the adjustment cannot depend on the treatment
# and the estimate stays unbiased while its variance shrinks by 1 - cor(Y, X)^2.
#
# CUPED is a per-customer transform, so it composes with everything downstream: feed
# the adjusted data straight into bootstrap_base(). Adjusting a ratio's numerator and
# denominator columns separately is the per-component scheme. A customer missing the
# covariate keeps their raw value (no adjustment) rather than being dropped, since
# dropping at differing rates across arms would bias the comparison.

cuped <- function(data, ...) {
  if (!dplyr::is.grouped_df(data)) {
    stop("`data` must be a grouped data frame.")
  }
  covariates <- rlang::ensyms(...)
  metrics <- names(covariates)
  if (is.null(metrics) || any(metrics == "")) {
    stop("Each adjustment must be named by its metric column, e.g. ",
         "cuped(data, spend = spend_pre).", call. = FALSE)
  }
  covariates <- vapply(covariates, rlang::as_string, character(1))
  unknown <- setdiff(c(metrics, covariates), names(data))
  if (length(unknown)) {
    stop("Unknown column(s): ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  }

  for (i in seq_along(metrics)) {
    warn_missing_covariate(data, covariates[i])
    data[[metrics[i]]] <- cuped_adjust(data[[metrics[i]]], data[[covariates[i]]], covariates[i])
  }
  data
}

cuped_adjust <- function(y, x, covariate) {
  present <- !is.na(y) & !is.na(x)
  if (sum(present) < 2L || stats::var(x[present]) == 0) {
    warning("Covariate '", covariate, "' has no usable variation; no adjustment applied.",
            call. = FALSE)
    return(y)
  }
  theta <- stats::cov(y[present], x[present]) / stats::var(x[present])
  y - theta * ifelse(is.na(x), 0, x - mean(x[present]))
}

warn_missing_covariate <- function(data, covariate) {
  overall <- mean(is.na(data[[covariate]]))
  if (overall > 0) {
    by_arm <- vapply(dplyr::group_split(data), function(arm) mean(is.na(arm[[covariate]])),
                     numeric(1))
    warning(sprintf(
      "Covariate '%s' is missing for %.1f%% of rows (%.1f%%-%.1f%% across arms); those customers keep their raw value. Differential missingness across arms biases the comparison.",
      covariate, 100 * overall, 100 * min(by_arm), 100 * max(by_arm)
    ), call. = FALSE)
  }
}
