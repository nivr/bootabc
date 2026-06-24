# A forest plot of confidence intervals: each cell or comparison as a point estimate
# with its interval drawn as a horizontal range, faceted by KPI (and, for comparisons,
# by ratio versus difference so the two scales never share an axis). Plotting is
# optional, so ggplot2 lives in Suggests and is only required when this is called.

#' Plot confidence intervals as a forest plot
#'
#' Draws each cell or comparison as a point estimate with its interval as a horizontal
#' range, faceted by KPI -- and, for comparisons, by ratio versus difference, with a
#' dashed no-effect line at 1 (ratio) and 0 (difference). Pass the output of
#' [confidence_intervals()].
#'
#' @param intervals A data frame from [confidence_intervals()].
#' @param lower,upper Names of the interval-bound columns to draw, matching the `probs`
#'   you summarised with (default the 95% bounds).
#'
#' @return A [ggplot2::ggplot] object.
#' @seealso [confidence_intervals()]
#' @importFrom rlang .data
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' customers <- dplyr::group_by(
#'   data.frame(spend = rgamma(200, 2),
#'              variant = rep(c("control", "treatment"), each = 100)),
#'   variant
#' )
#' bs <- bootstrap_measures(customers, arpu = mean(spend),
#'                          comparison = variant, reference = "control",
#'                          iterations = 1000, seed = 1)
#' plot_intervals(confidence_intervals(bs))
plot_intervals <- function(intervals, lower = "lower95", upper = "upper95") {
  rlang::check_installed("ggplot2", "to plot confidence intervals.")
  missing <- setdiff(c("kpi", "estimate", lower, upper), names(intervals))
  if (length(missing)) {
    stop("`intervals` is missing column(s): ", paste(missing, collapse = ", "),
         ". Pass the output of confidence_intervals().", call. = FALSE)
  }

  group_columns <- setdiff(
    names(intervals)[seq_len(match("kpi", names(intervals)) - 1L)], ".type"
  )
  intervals$.row <- if (length(group_columns)) {
    do.call(paste, c(intervals[group_columns], sep = " / "))
  } else {
    intervals$kpi
  }

  plot <- ggplot2::ggplot(intervals, ggplot2::aes(x = .data[["estimate"]], y = .data[[".row"]])) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data[[lower]], xmax = .data[[upper]]),
      orientation = "y", width = 0.2
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal()

  if (".type" %in% names(intervals)) {
    references <- data.frame(.type = c("ratio", "difference"), .ref = c(1, 0))
    plot +
      ggplot2::geom_vline(
        data = references[references$.type %in% intervals$.type, , drop = FALSE],
        ggplot2::aes(xintercept = .data[[".ref"]]), linetype = "dashed", colour = "grey60"
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[[".type"]]), cols = ggplot2::vars(.data[["kpi"]]),
        scales = "free_x"
      )
  } else {
    plot + ggplot2::facet_wrap(ggplot2::vars(.data[["kpi"]]), scales = "free_x")
  }
}
