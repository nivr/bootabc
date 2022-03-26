#' Bootstrap customer data from an A/B-test
#'
#' @param input_data_frame A grouped data frame with one row per
#' customer/visitor. It should contain a (grouped-by) column with the test
#' variant and additional columns for each measure in the test for that
#' customer/visitor.
#' @param bootstrap_iterations The number of bootstrap iterations to calculate,
#' the default is 100.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> Name-value pairs of
#' summary functions. The name will be the name of the variable in the result.
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
#' example_data <- data.frame(
#'                 x = rep(stats::runif(10),2),
#'                 group = c("A", "B")
#'                 ) %>%
#'                 group_by(group)
#'bootstrap_measures(example_data, kpi1=mean(x))
#' @importFrom dplyr summarise group_vars group_keys count across
#' @importFrom dplyr all_of bind_rows
#' @importFrom data.table as.data.table data.table rbindlist
#' @importFrom data.table merge.data.table :=
#' @importFrom rlang enexprs
#' @importFrom dqrng dqsample.int
#' @importFrom magrittr %>%
bootstrap_measures <- function(input_data_frame,
                               bootstrap_iterations = 100,
                               ...) {
  bootstrap_iteration <- NULL

  if (!is.grouped_df(input_data_frame)) {
    stop("Your input data must be a grouped data.frame")
  }

  bootstrap_results <- data.frame()
  data_table <- data.table::as.data.table(input_data_frame)

  group_column <- dplyr::group_vars(input_data_frame)
  group_names <- dplyr::group_keys(input_data_frame)
  group_filter <- group_names %>%
    sapply(function(.x) {
      paste0(group_column, " == \"", .x, "\"")
    })
  group_sizes <- dplyr::count(input_data_frame,
                              dplyr::across(dplyr::all_of(group_column)))$n
  for (group in seq_along(group_sizes)) {
    evaluation_expression <- .get_evaluation_string(.get_expressions(...),
                                                group_column,
                                                group_names,
                                                group)
    data_table_group <- data_table[eval(parse(text = group_filter[[group]]))]
    i <- 0
    group_result <- data.table::merge.data.table(
      data.table::data.table(bootstrap_iteration = rep(NA_real_,
                                                       bootstrap_iterations)),
      data_table[0, eval(evaluation_expression)],
      all.x = TRUE
    )

    for (i in seq(bootstrap_iterations)) {
      bootstrap_shuffle <- dqrng::dqsample.int(group_sizes[[group]],
                                               replace = TRUE)
      group_result[i, ] <-
        data_table_group[bootstrap_shuffle,
                        eval(evaluation_expression)][, bootstrap_iteration := i]
    }
    bootstrap_results <- dplyr::bind_rows(bootstrap_results,
                                   group_result)

  }

bootstrap_results
}

.get_expressions <- function(...) {
  rlang::enexprs(..., .ignore_empty = "all")
}

.get_evaluation_string <- function(evaluation_expressions,
                                   group_column,
                                   group_names,
                                   group) {
  parse(
    text = paste0(
      ".(",
      "bootstrap_iteration = i, ",
      paste0(group_column, " = ", "\"", group_names[[group, 1]], "\", "),
      paste(paste0(names(evaluation_expressions),
                   " = ",
                   evaluation_expressions), collapse = ", "),
      ")"
    )
  )
}
