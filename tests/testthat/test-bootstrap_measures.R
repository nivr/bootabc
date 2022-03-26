test_that("Expect error if input is an ungrouped data frame", {
  example_data <- data.frame(
    measure = 1:10,
    group = "A"
    )

  expect_error(bootstrap_measures(example_data,
                                     kpi1 = mean(measure)
  ), "grouped data.frame")
})

test_that("Output is a data frame", {
  num_groups <- 3

  example_data <- group_by(data.frame(
    measure = c(rep(
      stats::runif(10), num_groups
    )),
    group = paste0("group_", seq(num_groups))
  ), group)

  expect_s3_class(bootstrap_measures(example_data,
    kpi1 = mean(measure),
    kpi2 = sum(measure)
  ), "data.frame")
})

test_that("Output has bootstrap_iterations*num_groups rows", {
  bootstrap_iterations <- 173
  num_groups <- 7
  example_data <- group_by(data.frame(
    x = c(rep(
      stats::runif(10), num_groups
    )),
    group = paste0("group_", seq(num_groups))
  ), group)
  expect_equal(nrow(
    bootstrap_measures(example_data,
      bootstrap_iterations,
      kpi1 = mean(x)
    )
  ), bootstrap_iterations * num_groups)
})
