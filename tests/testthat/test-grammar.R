test_that("sum reduces to the bootstrapped column total", {
  m <- compile_measure(quote(sum(spend)))

  expect_equal(m$columns, "spend")
  expect_equal(m$derivation, quote(spend))
  expect_equal(m$synthesis, list())
})

test_that("mean divides the column total by the cell size", {
  m <- compile_measure(quote(mean(spend)))

  expect_equal(m$columns, "spend")
  expect_equal(m$derivation, quote(spend / n))
})

test_that("a ratio of sums keeps both columns and divides them", {
  m <- compile_measure(quote(sum(billings) / sum(active_days)))

  expect_equal(m$columns, c("billings", "active_days"))
  expect_equal(m$derivation, quote(billings / active_days))
  expect_equal(m$synthesis, list())
})

test_that("weighted.mean synthesises the product and divides by the weight total", {
  m <- compile_measure(quote(weighted.mean(spend, active_days)))
  product <- ".wsum_spend_x_active_days"

  expect_equal(m$columns, c(product, "active_days"))
  expect_equal(m$synthesis, stats::setNames(list(quote(spend * active_days)), product))
  expect_equal(m$derivation, call("/", as.symbol(product), quote(active_days)))
})

test_that("nested measures compose and evaluate to the right value", {
  m <- compile_measure(quote(mean(a) / mean(b)))

  expect_equal(m$columns, c("a", "b"))
  expect_equal(eval(m$derivation, list(a = 10, b = 4, n = 5)), (10 / 5) / (4 / 5))
})

test_that("an identical product across a ratio collapses to one synthesised column", {
  m <- compile_measure(quote(weighted.mean(x, w) / sum(w)))

  expect_equal(m$columns, c(".wsum_x_x_w", "w"))
  expect_length(m$synthesis, 1)
})

test_that("namespaced reducers are recognised", {
  m <- compile_measure(quote(base::sum(spend)))

  expect_equal(m$columns, "spend")
  expect_equal(m$derivation, quote(spend))
})

test_that("compiled output drives the real pipeline for a weighted mean", {
  raw <- data.frame(spend = c(2, 4, 6), days = c(1, 2, 3), variant = "A")
  m <- compile_measure(quote(weighted.mean(spend, days)))

  prepared <- dplyr::group_by(dplyr::mutate(raw, !!!m$synthesis), variant)
  bs <- bootstrap_base(prepared, m$columns, iterations = 200, seed = 1)
  ci <- confidence_intervals(derive(bs, wm = !!m$derivation), min_n = 1L)

  expect_equal(ci$estimate, weighted.mean(raw$spend, raw$days))  # 28 / 6
})

test_that("measures outside the grammar are rejected", {
  expect_error(compile_measure(quote(median(spend))), "not a supported measure")
  expect_error(compile_measure(quote(quantile(spend, 0.9))), "not a supported measure")
  expect_error(compile_measure(quote(spend)), "not a supported measure")
  expect_error(compile_measure(quote(sum(spend) + sum(days))), "not a supported measure")
  expect_error(compile_measure(quote(2 * mean(spend))), "not a supported measure")
})

test_that("reducers must name a single bare column", {
  expect_error(compile_measure(quote(sum(spend + tax))), "single column")
  expect_error(compile_measure(quote(mean(spend, days))), "single column")
  expect_error(compile_measure(quote(weighted.mean(spend))), "two column")
})
