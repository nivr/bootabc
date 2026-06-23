// Streaming exact bootstrap kernel.
//
// For one cell of n customers, draw `iterations` resamples with replacement and
// return the summed base columns for each draw. The random draws happen here, in
// the loop, so memory stays O(columns) per iteration -- no n x iterations index or
// count matrix is ever built, which is what lets the 5M-customer case run.
//
// Seeding with (seed, stream) gives each cell an independent, repeatable stream, so
// results do not depend on cell order or thread count.

// [[Rcpp::depends(dqrng, BH, sitmo)]]
#include <Rcpp.h>
#include <vector>
#include <dqrng_generator.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix bootstrap_sums_cpp(const Rcpp::NumericMatrix& values,
                                       int iterations, double seed, double stream) {
  const int n = values.nrow();
  const int k = values.ncol();
  Rcpp::NumericMatrix sums(iterations, k);

  dqrng::random_64bit_wrapper<dqrng::default_64bit_generator> generator(
      static_cast<uint64_t>(seed), static_cast<uint64_t>(stream));
  // The bounded draw operator()(range) lives on the base class and is hidden by the
  // wrapper's operator(); reach it through a base reference.
  dqrng::random_64bit_generator& rng = generator;

  std::vector<double> total(k);
  for (int iter = 0; iter < iterations; ++iter) {
    std::fill(total.begin(), total.end(), 0.0);
    for (int draw = 0; draw < n; ++draw) {
      uint32_t row = rng(static_cast<uint32_t>(n));
      for (int col = 0; col < k; ++col) {
        total[col] += values(row, col);
      }
    }
    for (int col = 0; col < k; ++col) {
      sums(iter, col) = total[col];
    }
  }
  return sums;
}
