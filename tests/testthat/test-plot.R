test_that("plot_intervals builds a forest plot for per-arm intervals", {
  skip_if_not_installed("ggplot2")
  data <- dplyr::group_by(
    data.frame(spend = as.double(1:10), variant = rep(c("A", "B"), each = 5)), variant
  )
  ci <- confidence_intervals(
    bootstrap_measures(data, arpu = mean(spend), iterations = 200, seed = 1), min_n = 1
  )

  plot <- plot_intervals(ci)

  expect_s3_class(plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot))  # forces aes/facets to resolve
})

test_that("plot_intervals facets a comparison by ratio and difference", {
  skip_if_not_installed("ggplot2")
  data <- dplyr::group_by(
    data.frame(spend = as.double(1:10), variant = rep(c("A", "B"), each = 5)), variant
  )
  ci <- confidence_intervals(
    bootstrap_measures(data, arpu = mean(spend), comparison = variant, reference = "A",
                       iterations = 200, seed = 1),
    min_n = 1
  )

  expect_no_error(ggplot2::ggplot_build(plot_intervals(ci)))
})

test_that("plot_intervals errors when interval columns are missing", {
  skip_if_not_installed("ggplot2")

  expect_error(plot_intervals(data.frame(kpi = "m", estimate = 1)), "missing column")
})
