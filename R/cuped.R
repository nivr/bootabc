#' Reduce variance with a pre-experiment covariate (CUPED)
#'
#' Replaces each metric column `Y` with `Z = Y - theta * (X - mean(X))`, where
#' `theta = cov(Y, X) / var(X)`. Both `theta` and the covariate mean are estimated once
#' on the pooled data (all arms together), so the adjustment cannot depend on the
#' treatment: the point estimate is unchanged in expectation while its variance shrinks
#' by a factor of `1 - cor(Y, X)^2`.
#'
#' CUPED is a per-customer transform, so apply it before [bootstrap_measures()]. For a
#' ratio KPI, adjust the numerator and denominator columns separately. A customer
#' missing the covariate keeps their raw value (no adjustment) rather than being dropped
#' -- dropping at different rates across arms would bias the comparison -- and the call
#' warns with the per-arm missing rate.
#'
#' @param data A grouped data frame (grouped by the variant and any strata).
#' @param ... Named adjustments mapping each metric column to its pre-experiment
#'   covariate, e.g. `spend = spend_pre`. Both are unquoted column names.
#'
#' @return `data` with the named metric columns replaced by their adjusted values.
#' @references Deng, Xu, Kohavi and Walker (2013), "Improving the Sensitivity of Online
#'   Controlled Experiments by Utilizing Pre-Experiment Data." \doi{10.1145/2433396.2433413}
#' @seealso [bootstrap_measures()]
#' @export
#' @examples
#' pre <- rnorm(100, 50, 10)
#' customers <- dplyr::group_by(
#'   data.frame(spend = pre + rnorm(100, 0, 5), spend_pre = pre, variant = "A"),
#'   variant
#' )
#' adjusted <- cuped(customers, spend = spend_pre)
#' confidence_intervals(
#'   bootstrap_measures(adjusted, arpu = mean(spend), iterations = 1000, seed = 1)
#' )
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
