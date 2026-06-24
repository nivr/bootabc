# A forest plot of confidence intervals. KPIs run down the y-axis; each is drawn as its
# point estimate with a thick inner interval (default 80%) and a thin outer interval
# (default 95%), so the movements of every KPI for one comparison can be read together.
# A single comparison (or arm) becomes the plot title; comparisons split into ratio and
# difference panels, each with a dashed no-effect line. Plotting is optional, so ggplot2
# lives in Suggests and is only required when this is called.

#' Plot confidence intervals as a forest plot
#'
#' Lays the KPIs down the y-axis and draws each as its point estimate with a thick inner
#' interval (default 80%) and a thin outer interval (default 95%) -- so the movements of
#' all KPIs for one comparison can be read together. For a single comparison or arm the
#' label becomes the title; comparisons are split into ratio and difference panels, each
#' with a dashed no-effect line (at 1 and 0). Pass the output of [confidence_intervals()].
#'
#' @param intervals A data frame from [confidence_intervals()].
#' @param types For a comparison, which panels to show: any of `"ratio"` and
#'   `"difference"` (default both). Ignored for per-arm (non-comparison) results.
#' @param outer,inner Names of the outer and inner interval-bound columns, each as
#'   `c(lower, upper)`, matching the `probs` you summarised with. The inner band is
#'   omitted when its columns are absent.
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
plot_intervals <- function(intervals,
                           types = c("ratio", "difference"),
                           outer = c("lower95", "upper95"),
                           inner = c("lower80", "upper80")) {
  rlang::check_installed("ggplot2", "to plot confidence intervals.")
  types <- match.arg(types, c("ratio", "difference"), several.ok = TRUE)
  absent <- setdiff(c("kpi", "estimate", outer), names(intervals))
  if (length(absent)) {
    stop("`intervals` is missing column(s): ", paste(absent, collapse = ", "),
         ". Pass the output of confidence_intervals().", call. = FALSE)
  }
  has_type <- ".type" %in% names(intervals)
  if (has_type) {
    intervals <- intervals[intervals$.type %in% types, , drop = FALSE]
    if (!nrow(intervals)) {
      stop("No intervals to plot after filtering to type(s): ",
           paste(types, collapse = ", "), ".", call. = FALSE)
    }
  }
  has_inner <- all(inner %in% names(intervals))

  series_columns <- setdiff(
    names(intervals)[seq_len(match("kpi", names(intervals)) - 1L)], ".type"
  )
  intervals$.series <- if (length(series_columns)) {
    do.call(paste, c(intervals[series_columns], sep = " / "))
  } else {
    ""
  }
  intervals$kpi <- factor(intervals$kpi, levels = rev(unique(intervals$kpi)))

  plot <- ggplot2::ggplot(
    intervals, ggplot2::aes(y = .data[["kpi"]], x = .data[["estimate"]])
  )
  if (has_type) {
    references <- data.frame(.type = c("ratio", "difference"), .ref = c(1, 0))
    plot <- plot + ggplot2::geom_vline(
      data = references[references$.type %in% intervals$.type, , drop = FALSE],
      ggplot2::aes(xintercept = .data[[".ref"]]), linetype = "dashed", colour = "grey70"
    )
  }
  plot <- plot + ggplot2::geom_linerange(
    ggplot2::aes(xmin = .data[[outer[1]]], xmax = .data[[outer[2]]]), orientation = "y"
  )
  if (has_inner) {
    plot <- plot + ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data[[inner[1]]], xmax = .data[[inner[2]]]),
      orientation = "y", linewidth = 1.8
    )
  }
  plot <- plot +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::labs(
      x = NULL, y = NULL,
      caption = if (has_inner) {
        "Point: estimate. Bars: 80% (thick) and 95% (thin) intervals."
      } else {
        "Point: estimate. Bar: 95% interval."
      }
    ) +
    ggplot2::theme_minimal()

  if (length(unique(intervals$.series)) <= 1L) {
    label <- unique(intervals$.series)
    if (nzchar(label)) {
      plot <- plot + ggplot2::ggtitle(label)
    }
    if (has_type) {
      plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data[[".type"]]), scales = "free_x")
    }
  } else if (has_type) {
    plot <- plot + ggplot2::facet_grid(
      rows = ggplot2::vars(.data[[".series"]]), cols = ggplot2::vars(.data[[".type"]]),
      scales = "free_x"
    )
  } else {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data[[".series"]]), scales = "free_x")
  }
  plot
}
