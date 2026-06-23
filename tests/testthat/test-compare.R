# One customer per cell makes each cell's bootstrap deterministic (every resample
# draws the only customer), so the comparison values are known exactly.

test_that("compare builds within-variant ratios and differences", {
  data <- dplyr::group_by(
    data.frame(spend = c(10, 20), variant = c("A", "B")),
    variant
  )
  bs <- derive(bootstrap_base(data, "spend", iterations = 5, seed = 1), m = spend / n)

  cmp <- compare(bs, variant, reference = "A")

  expect_true(all(cmp$draws$m[cmp$draws$.type == "ratio"] == 2))       # B / A = 20 / 10
  expect_true(all(cmp$draws$m[cmp$draws$.type == "difference"] == 10)) # B - A = 20 - 10
  expect_equal(unique(cmp$draws$variant), "B vs A")
})

test_that("comparisons stay within strata", {
  data <- dplyr::group_by(
    data.frame(
      spend   = c(10, 20, 100, 200),
      country = c("US", "US", "UK", "UK"),
      variant = c("A", "B", "A", "B")
    ),
    country, variant
  )
  bs <- derive(bootstrap_base(data, "spend", iterations = 4, seed = 1), m = spend / n)

  cmp <- compare(bs, variant, reference = "A")

  expect_setequal(unique(cmp$draws$country), c("US", "UK"))
  expect_true(all(cmp$draws$m[cmp$draws$country == "US" & cmp$draws$.type == "ratio"] == 2))
  expect_true(all(cmp$draws$m[cmp$draws$country == "UK" & cmp$draws$.type == "ratio"] == 2))
  expect_equal(nrow(cmp$draws), 2 * 1 * 2 * 4)  # strata x pairs x types x iterations
})

test_that("without a reference, all variant pairs are compared", {
  data <- dplyr::group_by(
    data.frame(spend = c(10, 20, 30), variant = c("A", "B", "C")),
    variant
  )
  bs <- derive(bootstrap_base(data, "spend", iterations = 3, seed = 1), m = spend / n)

  cmp <- compare(bs, variant, types = "ratio")

  expect_equal(length(unique(cmp$draws$variant)), 3)  # A vs B, A vs C, B vs C
})

test_that("compare validates its inputs", {
  data <- dplyr::group_by(
    data.frame(spend = c(10, 20), variant = c("A", "B")),
    variant
  )
  bs <- derive(bootstrap_base(data, "spend", iterations = 3, seed = 1), m = spend / n)

  expect_error(compare(bs, nope), "grouping column")
  expect_error(compare(bs, variant, reference = "Z"), "not a level")
  expect_error(
    compare(bootstrap_base(data, "spend", iterations = 3, seed = 1), variant),
    "derive"
  )
})
