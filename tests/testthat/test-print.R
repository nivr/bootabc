test_that("printing a boot_strap shows a compact summary, not the raw draws", {
  data <- dplyr::group_by(
    data.frame(spend = as.double(1:10), variant = rep(c("A", "B"), each = 5)), variant
  )
  bs <- bootstrap_measures(data, arpu = mean(spend), iterations = 100, seed = 1)

  expect_output(print(bs), "boot_strap")
  expect_output(print(bs), "arpu")
  expect_output(print(bs), "iterations")
  expect_lt(length(capture.output(print(bs))), 12)  # a summary, not thousands of draws
})

test_that("printing a comparison result notes the comparison", {
  data <- dplyr::group_by(
    data.frame(spend = as.double(1:10), variant = rep(c("A", "B"), each = 5)), variant
  )
  bs <- bootstrap_measures(data, arpu = mean(spend), comparison = variant, reference = "A",
                           iterations = 100, seed = 1)

  expect_output(print(bs), "comparison")
})

test_that("printing before any KPI is derived says so", {
  data <- dplyr::group_by(data.frame(spend = as.double(1:6), variant = "A"), variant)
  bs <- bootstrap_base(data, "spend", iterations = 50, seed = 1)

  expect_output(print(bs), "none derived")
})
