test_that("Output is a data frame", {
  example_data <- data.frame(
    x = c(
      stats::runif(10),
      stats::runif(10)
    ),
    group = c("A", "B")
  )
  expect_s3_class(bootstrap_measures(example_data), "data.frame")
})

test_that("Output has the same number of rows as specified in input", {
  bootstrap_iterations <- 1733
  example_data <- data.frame(
    x = c(
      stats::runif(10),
      stats::runif(10)
    ),
    group = c("A", "B")
  )
  expect_equal(nrow(bootstrap_measures(
    example_data,
    bootstrap_iterations
  )), bootstrap_iterations)
})
