test_that("a single-customer cell repeats that customer every draw", {
  sums <- bootstrap_sums(matrix(c(3, 9), nrow = 1), 10, seed = 1)

  expect_equal(unname(sums), matrix(rep(c(3, 9), each = 10), nrow = 10))
})

test_that("every resample sum is an exact combination of the cell's rows", {
  values <- matrix(c(1, 100, 7, 700), nrow = 2)   # rows (1, 7) and (100, 700)

  sums <- bootstrap_sums(values, 200, seed = 3)

  allowed <- rbind(2 * values[1, ], values[1, ] + values[2, ], 2 * values[2, ])
  in_allowed <- apply(sums, 1, function(r) any(apply(allowed, 1, function(a) all(a == r))))
  expect_true(all(in_allowed))
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

test_that("bootstrap sums match the analytic mean and sd of the cell total", {
  x <- c(2, 4, 6, 8, 10, 12, 14)
  n <- length(x)

  sums <- as.vector(bootstrap_sums(matrix(x, ncol = 1), 50000, seed = 7))

  expect_equal(mean(sums), sum(x), tolerance = 0.01)
  expect_equal(sd(sums), sqrt(n * mean((x - mean(x))^2)), tolerance = 0.05)
})

test_that("empty, non-numeric, and non-positive iterations are rejected", {
  expect_error(bootstrap_sums(matrix(numeric(0), nrow = 0, ncol = 2), 5, seed = 1), "no customers")
  expect_error(bootstrap_sums(matrix(letters[1:6], nrow = 3), 5, seed = 1), "numeric")
  expect_error(bootstrap_sums(matrix(1:4, nrow = 2), 0, seed = 1), "positive")
})
