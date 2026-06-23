two_variants <- function() {
  dplyr::group_by(
    data.frame(
      spend  = as.double(1:10),
      visits = as.double(rep(1:5, 2)),
      variant = rep(c("A", "B"), each = 5)
    ),
    variant
  )
}

test_that("derive builds a mean KPI from a sum and the cell size", {
  bs <- bootstrap_base(two_variants(), "spend", iterations = 50, seed = 1)

  out <- derive(bs, mean_spend = spend / n)

  expect_equal(out$draws$mean_spend, out$draws$spend / 5)
  expect_true("mean_spend" %in% names(out$registry))
})

test_that("derive forms a ratio of two sums on the shared resample", {
  bs <- bootstrap_base(two_variants(), c("spend", "visits"), iterations = 50, seed = 1)

  out <- derive(bs, spv = spend / visits)

  expect_equal(out$draws$spv, out$draws$spend / out$draws$visits)
  expect_false("n" %in% names(out$draws))
})

test_that("derivations chain within and across calls", {
  bs <- bootstrap_base(two_variants(), c("spend", "visits"), iterations = 20, seed = 1)

  out <- derive(bs, mean_spend = spend / n)
  out <- derive(out, spend_per_visit = mean_spend * n / visits)

  expect_equal(out$draws$spend_per_visit, out$draws$spend / out$draws$visits)
  expect_setequal(names(out$registry), c("mean_spend", "spend_per_visit"))
})
