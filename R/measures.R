# The front door. bootstrap_measures() is the one call most users make: it compiles
# each KPI in the measure grammar (see grammar.R), unions the columns those measures
# need, synthesises any weighted-mean product columns, then runs the low-level pipeline
# -- bootstrap_base() to resample the sums, derive() to rebuild the KPIs, and compare()
# to form within-stratum lifts when a comparison dimension is named. The result is a
# boot_strap object ready for confidence_intervals().

#' Bootstrap KPIs from an A/B test
#'
#' Resamples customers within each cell and turns the bootstrapped column sums into a
#' KPI distribution ready for confidence intervals. Each KPI is written in the measure
#' grammar -- `sum()`, `mean()`, `weighted.mean()`, and division -- so every measure is
#' a ratio of sums with an exact bootstrap. The kernel resamples those sums rather than
#' re-evaluating the statistic on each resample, which is what keeps large experiments
#' tractable.
#'
#' @param data A grouped data frame with one row per customer, grouped by the variant
#'   and any strata. Group by every dimension you want estimated separately.
#' @param ... Named measures, e.g. `arpu = mean(spend)` or
#'   `spend_per_day = sum(spend) / sum(active_days)`. Each must be named; the name
#'   becomes the KPI column. Only `sum()`, `mean()`, `weighted.mean()`, and `/` are
#'   allowed -- anything else is rejected rather than silently mis-bootstrapped.
#' @param comparison Optional grouping column whose levels are compared within each
#'   stratum, yielding lift (ratio) and/or difference distributions. Unquoted column
#'   name or a string.
#' @param reference Optional reference level of `comparison`; every other level is
#'   compared against it. With no reference, all pairs are compared.
#' @param types Which comparisons to form when `comparison` is set: `"ratio"`,
#'   `"difference"`, or both.
#' @param iterations Number of bootstrap resamples per cell.
#' @param seed Optional integer seed for reproducible resampling.
#' @param na Missing-value policy per column: `"error"` (default), `"zero"`, or
#'   `"drop"`. A single value applies to every column; name them to vary it, e.g.
#'   `c(spend = "zero")`.
#'
#' @return A `boot_strap` object holding the joint bootstrap distribution of the
#'   KPIs -- compared across variants when `comparison` is given. Pass it to
#'   [confidence_intervals()].
#'
#' @seealso [confidence_intervals()] to summarise the result, [cuped()] to reduce
#'   variance with pre-experiment data first.
#' @export
#' @examples
#' pre <- rnorm(200, 50, 10)
#' customers <- dplyr::group_by(
#'   data.frame(
#'     spend = pmax(pre + rnorm(200, 0, 5), 0),
#'     active_days = rpois(200, 3) + 1,
#'     variant = rep(c("control", "treatment"), each = 100)
#'   ),
#'   variant
#' )
#' bs <- bootstrap_measures(
#'   customers,
#'   arpu = mean(spend),
#'   spend_per_day = sum(spend) / sum(active_days),
#'   comparison = variant, reference = "control",
#'   iterations = 1000, seed = 1
#' )
#' confidence_intervals(bs)
bootstrap_measures <- function(data, ..., comparison = NULL, reference = NULL,
                               types = c("ratio", "difference"),
                               iterations = 10000L, seed = NULL, na = "error") {
  if (!dplyr::is.grouped_df(data)) {
    stop("`data` must be a grouped data frame; group by the variant (and any strata).",
         call. = FALSE)
  }
  measures <- rlang::enquos(...)
  if (!length(measures) || !all(nzchar(rlang::names2(measures)))) {
    stop("Supply at least one named measure, e.g. arpu = mean(spend).", call. = FALSE)
  }
  comparison <- rlang::enquo(comparison)
  comparing <- !rlang::quo_is_null(comparison)
  if (comparing && !rlang::as_name(comparison) %in% dplyr::group_vars(data)) {
    stop("`comparison` must be one of the grouping columns: ",
         paste(dplyr::group_vars(data), collapse = ", "), ".", call. = FALSE)
  }

  compiled <- lapply(measures, function(measure) compile_measure(rlang::quo_get_expr(measure)))
  columns <- Reduce(union, lapply(compiled, `[[`, "columns"))
  synthesis <- dedupe_synthesis(do.call(c, unname(lapply(compiled, `[[`, "synthesis"))))
  derivations <- lapply(compiled, `[[`, "derivation")

  if (length(synthesis)) {
    data <- dplyr::mutate(data, !!!synthesis)
  }
  bootstrapped <- bootstrap_base(data, columns, iterations = iterations, seed = seed, na = na)
  derived <- rlang::inject(derive(bootstrapped, !!!derivations))

  if (!comparing) {
    return(derived)
  }
  rlang::inject(
    compare(derived, !!rlang::sym(rlang::as_name(comparison)),
            reference = reference, types = types)
  )
}
