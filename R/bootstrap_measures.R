#' Bootstrap customer data from an A/B-test
#'
#' @param input_data A grouped data frame with one row per customer/visitor. It
#' should contain a (grouped-by) column with the test variant and additional
#' columns for each measure in the test for that customer/visitor.
#' @param bootstrap_iterations The number of bootstrap iterations to calculate,
#' the default is 100.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> Name-value pairs of summary
#'   functions. The name will be the name of the variable in the result.
#'
#'   The value can be:
#'
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A vector of length `n`, e.g. `quantile()`.
#'   * A data frame, to add multiple columns from a single expression.
#'
#' @return Returns a data frame with `bootstrap_iterations` rows containing
#' columns as defined in `...`
#' @export
#'
#' @examples
#' @importFrom dplyr summarise
bootstrap_measures <- function(input_data,
                               bootstrap_iterations = 100,
                               ...) {
  dplyr::sample_n(input_data,bootstrap_iterations,replace = TRUE)
}
