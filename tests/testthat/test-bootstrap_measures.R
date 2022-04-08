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

test_that("Output is a boot_strap", {
  num_groups <- 2

  example_data <- group_by(data.frame(
    measure = c(rep(
      stats::runif(7), num_groups
    )),
    group = paste0("group_", seq(num_groups))
  ), group)

  expect_s3_class(bootstrap_measures(example_data,
                                     kpi1 = mean(measure)
  ), "boot_strap")
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

test_that("bootstrap_measures calculation same as precomputed", {
  kpi_names <- paste0("kpi_", letters[1:7])
  expr <- rlang::parse_expr(
    paste0(
      "bootstrap_measures(dplyr::group_by(",
      "example_data_frame,experiment_variant), ",
      paste0(
        kpi_names, "_aggr", " = ", "mean(", kpi_names, ")",
        collapse = ", "
      ),
      ", bootstrap_iterations = 173",
      ")"
    )
  )

  dqrng::dqset.seed(20220404, stream = NULL)
  test_boot_strap <- eval(expr)

  expect_equal(test_boot_strap,
               example_boot_strap)
})

test_that("boot_strap ratios output expected", {
  expect_equal(
    31 / 17,
    .calc_ratios_of_combns(
      as_boot_strap(group_by(
        data.frame(
          measurement = c(17, 31),
          group = c("A", "Z"),
          bootstrap_iteration = c(1, 1)
        ),
        group
      ))
    )$measurement
  )
})

test_that("Direct CI calculation outputs correct number of rows", {
  expect_equal(
    25,
    nrow(calculate_confidence_intervals(example_boot_strap))
  )
})

test_that("Relative CI calculation outputs correct number of rows", {
  expect_equal(
    50,
    nrow(calculate_confidence_intervals(example_boot_strap,
                                        type = "relative"))
  )
})
