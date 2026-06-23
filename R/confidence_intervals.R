# Summarise a boot_strap into a tidy confidence-interval table: one row per cell and
# KPI, with the observed point estimate and the requested quantiles as columns. It
# summarises whatever object it is given -- a derived boot_strap yields per-cell
# intervals, a compare() result yields lift / difference intervals -- so "direct" vs
# "relative" is just which object you pass.
#
# Methods: "percentile" (quantiles of the draws) and "basic" (those quantiles
# reflected about the observed estimate). BCa follows. Small cells are flagged, not
# dropped: bootstrap intervals get unreliable when n is small (the resample space is
# coarse), so each row carries n and a reliability flag, and the call warns when any
# cell falls below `min_n`.

confidence_intervals <- function(boot_strap,
                                 probs = c(lower95 = 0.025, lower80 = 0.1, median = 0.5,
                                           upper80 = 0.9, upper95 = 0.975),
                                 method = c("percentile", "basic"),
                                 min_n = 30L) {
  method <- match.arg(method)
  kpis <- names(boot_strap$registry)
  if (!length(kpis)) {
    stop("No KPIs to summarise; derive() at least one first.")
  }
  if (method == "basic" && is.null(boot_strap$observed)) {
    stop("The 'basic' method needs the observed estimate, which this object lacks.")
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

  rows <- list()
  for (cell in seq_along(cells)) {
    for (kpi in kpis) {
      estimate <- if (is.null(estimates)) NA_real_ else estimates[[kpi]][cell]
      rows[[length(rows) + 1L]] <- interval_row(
        keys[cell, , drop = FALSE], kpi, cells[[cell]][[kpi]],
        probs, method, estimate, n[cell], min_n
      )
    }
  }
  result <- do.call(rbind, rows)
  warn_unreliable(result, min_n)
  result
}

interval_row <- function(key, kpi, values, probs, method, estimate, n, min_n) {
  finite <- values[is.finite(values)]
  bounds <- if (!length(finite)) {
    rep(NA_real_, length(probs))
  } else if (method == "basic") {
    2 * estimate - stats::quantile(finite, 1 - probs, names = FALSE)
  } else {
    stats::quantile(finite, probs, names = FALSE)
  }
  columns <- c(
    as.list(key),
    list(kpi = kpi, estimate = estimate),
    stats::setNames(as.list(bounds), names(probs)),
    list(
      n = n,
      reliable = if (is.na(n)) NA else n >= min_n,
      nonfinite = mean(!is.finite(values))
    )
  )
  do.call(data.frame, c(columns, list(check.names = FALSE, row.names = NULL,
                                      stringsAsFactors = FALSE)))
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
