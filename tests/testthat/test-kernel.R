# A deliberately naive resampler the fast kernel is checked against: resample,
# then sum each draw with a plain loop. Any future (Rcpp) kernel must keep matching
# this for the same indices.
naive_sum_resamples <- function(values, indices) {
  out <- matrix(NA_real_, ncol(indices), ncol(values))
  for (i in seq_len(ncol(indices))) {
    out[i, ] <- colSums(values[indices[, i], , drop = FALSE])
  }
  colnames(out) <- colnames(values)
  out
}

test_that("vectorised sums match the naive per-resample sums", {
  values <- matrix(as.double(sample(0:9, 40, replace = TRUE)), nrow = 8)
  indices <- matrix(sample.int(8, 8 * 25, replace = TRUE), nrow = 8)

  expect_equal(sum_resamples(values, indices), naive_sum_resamples(values, indices))
})

test_that("the kernel is reproducible per seed and stream, and streams differ", {
  values <- matrix(as.double(1:12), nrow = 4)

  expect_identical(
    bootstrap_sums(values, 50, seed = 42, stream = 1),
    bootstrap_sums(values, 50, seed = 42, stream = 1)
  )
  expect_false(identical(
    bootstrap_sums(values, 50, seed = 42, stream = 1),
    bootstrap_sums(values, 50, seed = 42, stream = 2)
  ))
})

test_that("bootstrap sums are unbiased for the cell total", {
  values <- matrix(c(2, 4, 6, 8, 10), nrow = 5)

  sums <- bootstrap_sums(values, 20000, seed = 7)

  expect_equal(mean(sums), sum(values), tolerance = 0.01)
})

test_that("a single-customer cell repeats that customer every draw", {
  sums <- bootstrap_sums(matrix(c(3, 9), nrow = 1), 10, seed = 1)

  expect_equal(unname(sums), matrix(rep(c(3, 9), each = 10), nrow = 10))
})

test_that("empty and non-numeric cells are rejected", {
  expect_error(bootstrap_sums(matrix(numeric(0), nrow = 0, ncol = 2), 5), "no customers")
  expect_error(bootstrap_sums(matrix(letters[1:6], nrow = 3), 5), "numeric")
})
