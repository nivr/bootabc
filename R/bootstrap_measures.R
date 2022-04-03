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
#' @importFrom dplyr all_of bind_rows is.grouped_df
#' @importFrom data.table as.data.table data.table rbindlist
#' @importFrom data.table merge.data.table :=
#' @importFrom rlang enexprs
#' @importFrom dqrng dqsample.int
#' @importFrom magrittr %>%
bootstrap_measures <- function(input_data_frame,
                               bootstrap_iterations = 100,
                               ...) {
  bootstrap_iteration <- NULL

  if (!dplyr::is.grouped_df(input_data_frame)) {
    stop("Your input data must be a grouped data.frame")
  }

  bootstrap_results <- new_boot_strap()

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

  bootstrap_results <- bootstrap_results %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(group_column)))

#class(bootstrap_results) <- c("boot_strap",class(bootstrap_results))
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

new_boot_strap <- function(x = data.frame()) {
  stopifnot(is.data.frame(x))
  #structure(x, class = "secret")
  class(x) <- c("boot_strap",class(x))
  x
}

as_boot_strap <- function(x) {
  stopifnot(is.data.frame(x))
  class(x) <- c("boot_strap",class(x))
  x
}

#' Division function for boot.strap objects
#' @rdname /
#' @export
#' @importFrom purrr negate
#' @importFrom dplyr select ungroup group_vars
`/.boot_strap` <- function(numerator,denominator) {
  if (!"boot_strap" %in% class(numerator)) stop("Cannot divide a non-boot.strap object by a boot_strap object.")
  if (!("boot_strap" %in% class(denominator)) && !("numeric" %in% class(denominator))) stop("Can only divide a boot_strap object by a numeric or boot_strap object.")

  group_column <- dplyr::group_vars(numerator)
  numerator <- dplyr::ungroup(numerator)

  numerator_numeric <- numerator %>%
    dplyr::select(-bootstrap_iteration) %>%
    dplyr::select(where(is.numeric)) %>%
    as_boot_strap()
  numerator_nonnumeric <- numerator %>%
    dplyr::select(bootstrap_iteration,where(purrr::negate(is.numeric))) %>%
    as_boot_strap()
  if ("boot_strap" %in% class(denominator)) {
    denominator <- dplyr::ungroup(denominator) %>%
      as_boot_strap()
    denominator_numeric <- denominator %>%
      dplyr::select(-bootstrap_iteration) %>%
      dplyr::select(where(is.numeric)) %>%
      as_boot_strap()
    denominator_nonnumeric <- denominator %>%
      dplyr::select(bootstrap_iteration,where(purrr::negate(is.numeric))) %>%
      as_boot_strap()
  } else {
    denominator_numeric <- denominator
    denominator_nonnumeric <- NULL
  }

  if ("numeric" %in% class(denominator)) return(cbind(numerator_nonnumeric,.Primitive("/")(as.data.frame(numerator_numeric),denominator)))  %>%
    as_boot_strap()

  #if (!identical(numerator_nonnumeric,denominator_nonnumeric)) stop("Non-numeric elements are not identical.")
  if (!identical(sapply(numerator_nonnumeric,class),sapply(denominator_nonnumeric,class))) stop("Non-numeric elements are not identical.")
  if (!identical(numerator_nonnumeric$bootstrap_iteration,denominator_nonnumeric$bootstrap_iteration)) stop("Arguments don't come from the same boot_strap")
  cbind(numerator_nonnumeric,.Primitive("/")(as.data.frame(numerator_numeric),as.data.frame(denominator_numeric))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_column)))  %>%
    as_boot_strap()
}

#' @importFrom rlang .data
.calc_ratios_of_combns <- function(.boot_strap) {
  group_column <- dplyr::group_vars(.boot_strap)
  comparisons <- .boot_strap %>%
    dplyr::pull(group_column) %>%
    unique() %>%
    combn(2, simplify = FALSE)

  boot_strap_ratio <- new_boot_strap()

  for (comparison in comparisons) {
    group_column <- dplyr::group_vars(.boot_strap)
    comparison_left <- .boot_strap %>%
      dplyr::filter(.data[[group_column]] == comparison[[1]]) %>%
      as_boot_strap()
    comparison_right <- .boot_strap %>%
      dplyr::filter(.data[[group_column]] == comparison[[2]]) %>%
      as_boot_strap()

    boot_strap_ratio <- rbind(
      boot_strap_ratio,
      dplyr::mutate(
        comparison_right / comparison_left,
        !!group_column := paste0(comparison[[1]], " vs ", comparison[[2]])
      )
    )
  }
  as_boot_strap(boot_strap_ratio)
}
