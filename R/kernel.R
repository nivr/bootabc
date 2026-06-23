# Exact bootstrap kernel.
#
# Every supported KPI is a smooth function of per-customer column sums, so the
# kernel never sees a KPI expression: it resamples the customers in one cell and
# returns the summed base columns for each draw. The derivation layer turns those
# sums into means, ratios, weighted means and so on.

resample_indices <- function(n, iterations, seed = NULL, stream = NULL) {
  if (!is.null(seed)) {
    dqrng::dqset.seed(seed, stream = stream)
  }
  matrix(dqrng::dqsample.int(n, n * iterations, replace = TRUE), nrow = n)
}

sum_resamples <- function(values, indices) {
  counts <- matrix(apply(indices, 2, tabulate, nbins = nrow(values)), nrow = nrow(values))
  sums <- crossprod(counts, values)
  colnames(sums) <- colnames(values)
  sums
}

bootstrap_sums <- function(values, iterations, seed = NULL, stream = NULL) {
  values <- as_value_matrix(values)
  if (length(iterations) != 1 || iterations < 1) {
    stop("`iterations` must be a single positive number.")
  }
  indices <- resample_indices(nrow(values), iterations, seed, stream)
  sum_resamples(values, indices)
}

as_value_matrix <- function(values) {
  if (is.data.frame(values)) {
    values <- as.matrix(values)
  }
  if (!is.matrix(values) || !is.numeric(values)) {
    stop("Bootstrap values must be a numeric matrix or data frame.")
  }
  if (nrow(values) == 0L) {
    stop("Cannot bootstrap a cell with no customers.")
  }
  values
}
