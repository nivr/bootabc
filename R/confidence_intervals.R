#' Summarise a bootstrap distribution into confidence intervals
#'
#' Turns a [bootstrap_measures()] result into a tidy table: one row per cell (or
#' comparison) and KPI, with the observed point estimate and the requested quantiles.
#' Per-arm intervals come from a plain result; lift / difference intervals come from a
#' compared one -- "direct" versus "relative" is simply which object you pass.
#'
#' Bootstrap intervals get unreliable when a cell is small (the resample space is
#' coarse), so each row carries its `n` and a `reliable` flag rather than being silently
#' dropped, and the call warns when any cell falls below `min_n`.
#'
#' @param boot_strap A `boot_strap` object from [bootstrap_measures()].
#' @param probs Named quantile probabilities; each becomes an interval column.
#' @param method `"percentile"` (quantiles of the draws), `"basic"` (those quantiles
#'   reflected about the observed estimate), or `"bca"` (bias-corrected and accelerated;
#'   needs per-customer data, so it is not available for comparison results).
#' @param min_n Cells with fewer than `min_n` customers are flagged `reliable = FALSE`
#'   and trigger a warning.
#'
#' @return A data frame with the grouping columns, `kpi`, `estimate`, one column per
#'   entry of `probs`, `n`, `reliable`, and `nonfinite` (the fraction of draws that were
#'   non-finite and so excluded from the quantiles).
#' @seealso [bootstrap_measures()]
#' @export
#' @examples
#' customers <- dplyr::group_by(
#'   data.frame(spend = rgamma(100, 2), variant = "A"), variant
#' )
#' bs <- bootstrap_measures(customers, arpu = mean(spend), iterations = 1000, seed = 1)
#' confidence_intervals(bs)
confidence_intervals <- function(boot_strap,
                                 probs = c(lower95 = 0.025, lower80 = 0.1, median = 0.5,
                                           upper80 = 0.9, upper95 = 0.975),
                                 method = c("percentile", "basic", "bca"),
                                 min_n = 30L) {
  method <- match.arg(method)
  kpis <- names(boot_strap$registry)
  if (!length(kpis)) {
    stop("No KPIs to summarise; derive() at least one first.")
  }
  if (method %in% c("basic", "bca") && is.null(boot_strap$observed)) {
    stop("The '", method, "' method needs the observed estimate, which this object lacks.")
  }
  if (method == "bca" && is.null(boot_strap$points)) {
    stop("The 'bca' method needs per-customer data, which is unavailable for comparison ",
         "results; use \"percentile\" or \"basic\".", call. = FALSE)
  }
  if (is.null(names(probs)) || any(names(probs) == "")) {
    names(probs) <- paste0("q", probs * 100)
  }
  groups <- boot_strap$group_columns

  grouped <- dplyr::group_by(boot_strap$draws, dplyr::across(dplyr::all_of(groups)))
  keys <- dplyr::group_keys(grouped)
  cells <- dplyr::group_split(grouped)
  n <- cell_sizes(boot_strap, keys, groups)
  estimates <- cell_estimates(boot_strap, keys, groups)

  points <- if (method == "bca") {
    lapply(seq_along(cells), function(cell) {
      cell_points(boot_strap, keys[cell, , drop = FALSE], groups)
    })
  }

  rows <- list()
  for (cell in seq_along(cells)) {
    for (kpi in kpis) {
      estimate <- if (is.null(estimates)) NA_real_ else estimates[[kpi]][cell]
      jack <- if (method == "bca") {
        jackknife(points[[cell]], boot_strap$registry[[kpi]], boot_strap$base_columns)
      }
      rows[[length(rows) + 1L]] <- interval_row(
        keys[cell, , drop = FALSE], kpi, cells[[cell]][[kpi]],
        probs, method, estimate, n[cell], min_n, jack
      )
    }
  }
  result <- do.call(rbind, rows)
  warn_unreliable(result, min_n)
  result
}

interval_row <- function(key, kpi, values, probs, method, estimate, n, min_n, jack = NULL) {
  columns <- c(
    as.list(key),
    list(kpi = kpi, estimate = estimate),
    stats::setNames(as.list(interval_bounds(values, probs, method, estimate, jack)),
                    names(probs)),
    list(
      n = n,
      reliable = if (is.na(n)) NA else n >= min_n,
      nonfinite = mean(!is.finite(values))
    )
  )
  do.call(data.frame, c(columns, list(check.names = FALSE, row.names = NULL,
                                      stringsAsFactors = FALSE)))
}

# Interval endpoints for one cell and KPI. Percentile reads the draws' quantiles
# directly; basic reflects them about the observed estimate; BCa shifts the quantile
# levels by the bias (z0) and acceleration (a) first, correcting for skew and median
# bias. If the bias or acceleration is undefined (e.g. a degenerate cell), BCa falls
# back to the percentile levels.
interval_bounds <- function(values, probs, method, estimate, jack) {
  finite <- values[is.finite(values)]
  if (!length(finite)) {
    return(rep(NA_real_, length(probs)))
  }
  levels <- switch(method,
    basic = 1 - probs,
    bca = bca_levels(finite, estimate, jack, probs),
    probs
  )
  bounds <- stats::quantile(finite, levels, names = FALSE)
  if (method == "basic") 2 * estimate - bounds else bounds
}

bca_levels <- function(draws, estimate, jack, probs) {
  z0 <- stats::qnorm(mean(draws < estimate))
  a <- acceleration(jack)
  if (!is.finite(z0) || !is.finite(a)) {
    return(probs)
  }
  z <- stats::qnorm(probs)
  stats::pnorm(z0 + (z0 + z) / (1 - a * (z0 + z)))
}

acceleration <- function(jack) {
  centred <- mean(jack) - jack
  scatter <- sum(centred^2)
  if (!is.finite(scatter) || scatter == 0) {
    return(NaN)
  }
  sum(centred^3) / (6 * scatter^1.5)
}

# Jackknife KPI values: recompute the KPI with each customer left out by subtracting
# that customer's row from the column totals (and dropping one from n), then evaluating
# the same derivation used on the bootstrapped sums.
jackknife <- function(points, derivation, base_columns) {
  totals <- colSums(points)
  loo <- stats::setNames(
    lapply(base_columns, function(column) totals[[column]] - points[, column]),
    base_columns
  )
  loo$n <- nrow(points) - 1L
  eval(derivation, loo)
}

cell_points <- function(boot_strap, key, groups) {
  match <- Reduce(`&`, lapply(groups, function(g) boot_strap$cells[[g]] == key[[g]]))
  boot_strap$points[[which(match)[1]]]
}

cell_sizes <- function(boot_strap, keys, groups) {
  cells <- boot_strap$cells
  if (!is.null(cells) && "n" %in% names(cells)) {
    dplyr::left_join(keys, cells[c(groups, "n")], by = groups)$n
  } else {
    rep(NA_integer_, nrow(keys))
  }
}

cell_estimates <- function(boot_strap, keys, groups) {
  if (is.null(boot_strap$observed)) {
    return(NULL)
  }
  dplyr::left_join(keys, boot_strap$observed, by = groups)
}

warn_unreliable <- function(result, min_n) {
  small <- !is.na(result$reliable) & !result$reliable
  if (any(small)) {
    warning(sum(small), " interval(s) come from cells with fewer than ", min_n,
            " customers and may be unreliable.", call. = FALSE)
  }
  if (any(result$nonfinite > 0)) {
    warning("Some bootstrap draws were non-finite (e.g. a near-zero ratio denominator); ",
            "intervals use the finite draws only.", call. = FALSE)
  }
  invisible(result)
}
