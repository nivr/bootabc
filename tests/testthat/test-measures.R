test_that("a single mean is bootstrapped per cell with the right estimate", {
  data <- dplyr::group_by(
    data.frame(spend = as.double(1:10), variant = rep(c("A", "B"), each = 5)),
    variant
  )

  bs <- bootstrap_measures(data, arpu = mean(spend), iterations = 1000, seed = 1)
  ci <- confidence_intervals(bs, min_n = 5)

  expect_s3_class(bs, "boot_strap")
  expect_equal(names(bs$registry), "arpu")
  expect_equal(ci$estimate[ci$variant == "A"], 3)  # mean of 1:5
  expect_equal(ci$estimate[ci$variant == "B"], 8)  # mean of 6:10
})

test_that("a ratio of sums compiles end to end", {
  data <- dplyr::group_by(
    data.frame(billings = c(2, 4, 6, 8), active = c(1, 1, 2, 2), variant = "A"),
    variant
  )

  bs <- bootstrap_measures(data, spd = sum(billings) / sum(active), iterations = 500, seed = 1)
  ci <- confidence_intervals(bs, min_n = 1)

  expect_equal(ci$estimate, 20 / 6)  # total billings / total active days
})

test_that("weighted means are synthesised and bootstrapped", {
  data <- dplyr::group_by(data.frame(v = c(2, 4, 6), w = c(1, 2, 3), variant = "A"), variant)

  bs <- bootstrap_measures(data, wm = weighted.mean(v, w), iterations = 300, seed = 1)
  ci <- confidence_intervals(bs, min_n = 1)

  expect_equal(ci$estimate, weighted.mean(c(2, 4, 6), c(1, 2, 3)))
})

test_that("measures sharing a column union to one bootstrapped column", {
  data <- dplyr::group_by(
    data.frame(spend = as.double(1:10), days = as.double(rep(1:5, 2)),
               variant = rep(c("A", "B"), each = 5)),
    variant
  )

  bs <- bootstrap_measures(data, arpu = mean(spend), spd = sum(spend) / sum(days),
                           iterations = 300, seed = 1)

  expect_setequal(bs$base_columns, c("spend", "days"))
  expect_setequal(names(bs$registry), c("arpu", "spd"))
})

test_that("a comparison yields lift and difference intervals", {
  data <- dplyr::group_by(
    data.frame(spend = as.double(1:10), variant = rep(c("A", "B"), each = 5)),
    variant
  )

  bs <- bootstrap_measures(data, arpu = mean(spend), comparison = variant, reference = "A",
                           iterations = 1000, seed = 1)
  ci <- confidence_intervals(bs, min_n = 5)

  expect_setequal(ci$.type, c("ratio", "difference"))
  expect_equal(ci$estimate[ci$.type == "ratio"], 8 / 3)        # mean B / mean A
  expect_equal(ci$estimate[ci$.type == "difference"], 5)       # mean B - mean A
})

test_that("a fixed seed is reproducible", {
  data <- dplyr::group_by(data.frame(spend = as.double(1:10), variant = "A"), variant)

  a <- bootstrap_measures(data, m = mean(spend), iterations = 500, seed = 42)
  b <- bootstrap_measures(data, m = mean(spend), iterations = 500, seed = 42)

  expect_equal(a$draws, b$draws)
})

test_that("input is validated", {
  grouped <- dplyr::group_by(data.frame(spend = as.double(1:6), variant = "A"), variant)

  expect_error(bootstrap_measures(data.frame(spend = 1:3), m = mean(spend)), "grouped")
  expect_error(bootstrap_measures(grouped, iterations = 10, seed = 1), "at least one")
  expect_error(bootstrap_measures(grouped, mean(spend), iterations = 10, seed = 1), "named")
  expect_error(bootstrap_measures(grouped, m = median(spend), iterations = 10, seed = 1),
               "not a supported measure")
})

test_that("the comparison column must be a grouping column", {
  data <- dplyr::group_by(
    data.frame(spend = as.double(1:6), variant = rep(c("A", "B"), 3)), variant
  )

  expect_error(
    bootstrap_measures(data, m = mean(spend), comparison = country, iterations = 10, seed = 1),
    "grouping column"
  )
})
