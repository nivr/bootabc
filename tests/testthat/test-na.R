test_that("missing values error by default, naming the column", {
  data <- dplyr::group_by(data.frame(x = c(1, NA, 3), variant = "A"), variant)

  expect_error(bootstrap_base(data, "x", iterations = 10, seed = 1), "Missing values")
})

test_that("the zero policy treats missing as a structural zero", {
  data <- dplyr::group_by(data.frame(x = c(2, NA, 4), variant = "A"), variant)

  bs <- bootstrap_base(data, "x", iterations = 50, seed = 1, na = "zero")

  expect_equal(bs$observed$x, 6)                          # 2 + 0 + 4
  expect_true(all(bs$draws$x >= 0 & bs$draws$x <= 12))    # resamples of {2, 0, 4}
})

test_that("the drop policy removes incomplete rows and warns", {
  data <- dplyr::group_by(data.frame(x = c(10, NA, 30, 40), variant = "A"), variant)

  expect_warning(
    bs <- bootstrap_base(data, "x", iterations = 50, seed = 1, na = "drop"),
    "dropped"
  )
  expect_equal(bs$cells$n, 3L)        # 4 rows - 1 missing
  expect_equal(bs$observed$x, 80)     # 10 + 30 + 40
})

test_that("na accepts per-column policies, defaulting unnamed columns to error", {
  zeroed <- dplyr::group_by(
    data.frame(a = c(1, NA, 3), b = c(NA, 5, 6), variant = "A"), variant
  )
  bs <- bootstrap_base(zeroed, c("a", "b"), iterations = 50, seed = 1,
                       na = c(a = "zero", b = "zero"))
  expect_equal(bs$observed$a, 4)   # 1 + 0 + 3
  expect_equal(bs$observed$b, 11)  # 0 + 5 + 6

  partial <- dplyr::group_by(
    data.frame(a = c(1, NA, 3), b = c(4, 5, 6), variant = "A"), variant
  )
  expect_error(  # a is missing but has no policy -> default error
    bootstrap_base(partial, c("a", "b"), iterations = 10, seed = 1, na = c(b = "zero")),
    "Missing values"
  )
})

test_that("unknown na policies error", {
  data <- dplyr::group_by(data.frame(x = c(1, 2, 3), variant = "A"), variant)

  expect_error(
    bootstrap_base(data, "x", iterations = 10, seed = 1, na = "interpolate"),
    "Unknown"
  )
})
