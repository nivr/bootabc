grouped_customers <- function() {
  dplyr::group_by(
    data.frame(
      spend = as.double(1:10),
      visits = as.double(rep(1:5, 2)),
      variant = rep(c("A", "B"), each = 5)
    ),
    variant
  )
}

test_that("bootstrap_base returns a boot_strap of base-column sums", {
  bs <- bootstrap_base(grouped_customers(), c("spend", "visits"), iterations = 100, seed = 1)

  expect_s3_class(bs, "boot_strap")
  expect_equal(nrow(bs$draws), 200)
  expect_equal(bs$base_columns, c("spend", "visits"))
  expect_true(all(c("variant", ".iteration", "spend", "visits") %in% names(bs$draws)))
})

test_that("each cell records its customer count", {
  bs <- bootstrap_base(grouped_customers(), "spend", iterations = 10, seed = 1)

  expect_equal(bs$cells$n, c(5L, 5L))
})

test_that("cells are bootstrapped on their own customers only", {
  bs <- bootstrap_base(grouped_customers(), "spend", iterations = 200, seed = 1)

  ranges <- tapply(bs$draws$spend, bs$draws$variant, range)
  expect_true(all(ranges$A >= 5 & ranges$A <= 25))    # A spends are 1:5  -> sums in [5, 25]
  expect_true(all(ranges$B >= 30 & ranges$B <= 50))   # B spends are 6:10 -> sums in [30, 50]
})

test_that("bootstrap_base rejects ungrouped data and unknown columns", {
  ungrouped <- data.frame(spend = 1:5, variant = "A")
  expect_error(bootstrap_base(ungrouped, "spend"), "grouped data frame")
  expect_error(bootstrap_base(grouped_customers(), "missing"), "Unknown column")
})
