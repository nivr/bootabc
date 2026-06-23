test_that("the adjustment subtracts theta times the centred covariate", {
  y <- c(2, 4, 6, 8)
  x <- c(1, 2, 3, 5)
  data <- dplyr::group_by(data.frame(m = y, p = x, variant = "A"), variant)

  adj <- cuped(data, m = p)

  theta <- stats::cov(y, x) / stats::var(x)
  expect_equal(adj$m, y - theta * (x - mean(x)))
})

test_that("CUPED narrows the interval but leaves the point estimate unchanged", {
  set.seed(1)
  pre <- stats::rnorm(200, 50, 10)
  post <- pre + stats::rnorm(200, 0, 2)   # post tracks pre closely
  data <- dplyr::group_by(data.frame(spend = post, spend_pre = pre, variant = "A"), variant)

  raw <- confidence_intervals(
    derive(bootstrap_base(data, "spend", iterations = 2000, seed = 1), m = spend / n), min_n = 1L
  )
  adj <- confidence_intervals(
    derive(bootstrap_base(cuped(data, spend = spend_pre), "spend", iterations = 2000, seed = 1),
           m = spend / n), min_n = 1L
  )

  expect_lt(adj$upper95 - adj$lower95, raw$upper95 - raw$lower95)  # variance reduced
  expect_equal(adj$estimate, raw$estimate)                        # estimate unchanged
})

test_that("missing covariates keep the raw value and warn", {
  data <- dplyr::group_by(
    data.frame(spend = c(10, 20, 30, 40), spend_pre = c(1, 2, NA, 4), variant = "A"),
    variant
  )

  expect_warning(adj <- cuped(data, spend = spend_pre), "missing")
  expect_equal(adj$spend[3], 30)  # customer with no covariate is unadjusted
})

test_that("ratio KPIs are adjusted per component", {
  set.seed(2)
  data <- dplyr::group_by(
    data.frame(
      num = stats::rnorm(100, 20, 5), num_pre = stats::rnorm(100, 20, 5),
      den = stats::rnorm(100, 10, 3), den_pre = stats::rnorm(100, 10, 3),
      variant = "A"
    ),
    variant
  )

  adj <- cuped(data, num = num_pre, den = den_pre)
  bs <- derive(bootstrap_base(adj, c("num", "den"), iterations = 500, seed = 1), r = num / den)

  expect_true("r" %in% names(bs$registry))
})

test_that("cuped validates its inputs", {
  data <- dplyr::group_by(data.frame(spend = 1:3, spend_pre = 4:6, variant = "A"), variant)

  expect_error(cuped(dplyr::ungroup(data), spend = spend_pre), "grouped")
  expect_error(cuped(data, spend = nope), "Unknown column")
  expect_error(cuped(data, spend_pre), "named")  # mapping must be named by the metric
})
