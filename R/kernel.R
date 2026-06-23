# Exact bootstrap kernel.
#
# Resamples the customers in one cell and returns the summed base columns for each
# draw. The streaming C++ implementation (src/bootstrap.cpp) does the random draws in
# its own loop, so memory stays O(columns) per iteration. Every supported KPI is a
# smooth function of these sums, so the kernel never evaluates a KPI expression.

bootstrap_sums <- function(values, iterations, seed, stream = 0L) {
  values <- as_value_matrix(values)
  if (length(iterations) != 1 || iterations < 1) {
    stop("`iterations` must be a single positive number.")
  }
  sums <- bootstrap_sums_cpp(values, as.integer(iterations), as.double(seed), as.double(stream))
  colnames(sums) <- colnames(values)
  sums
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
