test_that("confidence_intervals summarises each cell and KPI with a point estimate", {
  data <- dplyr::group_by(
    data.frame(x = as.double(1:10), variant = rep(c("A", "B"), each = 5)),
    variant
  )
  bs <- derive(bootstrap_base(data, "x", iterations = 2000, seed = 1), mean_x = x / n)

  ci <- confidence_intervals(bs, min_n = 5L)

  expect_equal(nrow(ci), 2)  # 2 cells x 1 KPI
  expect_true(all(c("variant", "kpi", "estimate", "lower95", "median", "upper95", "n",
                    "reliable", "nonfinite") %in% names(ci)))
  expect_equal(ci$n, c(5L, 5L))
  expect_equal(ci$estimate[ci$variant == "A"], 3)  # observed mean of 1:5, exact
  expect_equal(ci$estimate[ci$variant == "B"], 8)  # observed mean of 6:10, exact
  expect_true(all(ci$lower95 <= ci$median & ci$median <= ci$upper95))
})

test_that("basic intervals reflect the percentile quantiles about the estimate", {
  data <- dplyr::group_by(
    data.frame(x = as.double(1:10), variant = rep(c("A", "B"), each = 5)),
    variant
  )
  bs <- derive(bootstrap_base(data, "x", iterations = 4000, seed = 1), mean_x = x / n)

  pct <- confidence_intervals(bs, method = "percentile", min_n = 5L)
  bas <- confidence_intervals(bs, method = "basic", min_n = 5L)

  expect_equal(bas$lower95, 2 * pct$estimate - pct$upper95, tolerance = 1e-8)
  expect_true(all(bas$lower95 <= bas$median & bas$median <= bas$upper95))
})

test_that("small cells are flagged and warned about", {
  data <- dplyr::group_by(data.frame(x = as.double(1:6), variant = "A"), variant)
  bs <- derive(bootstrap_base(data, "x", iterations = 500, seed = 1), mean_x = x / n)

  expect_warning(ci <- confidence_intervals(bs, min_n = 30L), "unreliable")
  expect_false(ci$reliable)  # n = 6 < 30
})

test_that("a comparison result summarises to lift intervals with estimate and min-arm n", {
  data <- dplyr::group_by(
    data.frame(x = as.double(1:10), variant = rep(c("A", "B"), each = 5)),
    variant
  )
  bs <- derive(bootstrap_base(data, "x", iterations = 2000, seed = 1), mean_x = x / n)
  cmp <- compare(bs, variant, reference = "A")

  ci <- confidence_intervals(cmp, min_n = 5L)

  expect_true(all(c("variant", ".type", "kpi", "estimate", "median", "n") %in% names(ci)))
  expect_setequal(ci$.type, c("ratio", "difference"))
  expect_equal(unique(ci$n), 5L)  # min of the two arms
  expect_equal(ci$estimate[ci$.type == "ratio"], 8 / 3)  # observed mean_B / mean_A, exact
  expect_equal(ci$estimate[ci$.type == "difference"], 5) # observed mean_B - mean_A, exact
})

test_that("non-finite draws are excluded and warned about", {
  data <- dplyr::group_by(
    data.frame(num = c(1, 1, 1), den = c(0, 0, 2), variant = "A"),
    variant
  )
  bs <- derive(bootstrap_base(data, c("num", "den"), iterations = 500, seed = 1), r = num / den)

  expect_warning(ci <- confidence_intervals(bs, min_n = 1L), "non-finite")
  expect_true(ci$nonfinite > 0)
})
