# The measure grammar. A KPI is written as a ratio of column reducers:
#
#   sum(col)             the column total
#   mean(col)            sum(col) / n
#   weighted.mean(x, w)  sum(x * w) / sum(w)
#   a / b                ratio (lift) of two measures
#
# Every reducer is linear in the per-customer columns, so any measure reduces to a
# ratio of column sums -- exactly what the kernel resamples. A measure can therefore be
# bootstrapped by resampling the sums it names and evaluating it on them, without ever
# resampling the statistic itself. compile_measure() rewrites one KPI expression into
# the three pieces the pipeline consumes:
#
#   columns      per-customer columns to bootstrap
#   synthesis    product columns to create before bootstrapping (for weighted means)
#   derivation   an expression rebuilding the KPI from those sums and the cell size n
#
# The grammar is closed on purpose. median(), quantile(), a bare column, or arithmetic
# other than division is rejected: those are not ratios of sums, so they have no exact
# sum-resampling bootstrap and accepting them would silently return wrong intervals.

compile_measure <- function(expr) {
  if (!rlang::is_call(expr)) {
    reject_measure(expr)
  }
  name <- rlang::call_name(expr)
  if (is.null(name)) {
    reject_measure(expr)
  }
  switch(name,
    "/"             = compile_ratio(expr),
    "sum"           = compile_sum(expr),
    "mean"          = compile_mean(expr),
    "weighted.mean" = compile_weighted_mean(expr),
    reject_measure(expr)
  )
}

compile_ratio <- function(expr) {
  parts <- rlang::call_args(expr)
  numerator <- compile_measure(parts[[1]])
  denominator <- compile_measure(parts[[2]])
  measure(
    columns = union(numerator$columns, denominator$columns),
    derivation = call("/", numerator$derivation, denominator$derivation),
    synthesis = dedupe_synthesis(c(numerator$synthesis, denominator$synthesis))
  )
}

compile_sum <- function(expr) {
  column <- reducer_column(expr, "sum")
  measure(column, derivation = as.symbol(column))
}

compile_mean <- function(expr) {
  column <- reducer_column(expr, "mean")
  measure(column, derivation = call("/", as.symbol(column), quote(n)))
}

compile_weighted_mean <- function(expr) {
  columns <- weighted_columns(expr)
  product <- product_column(columns$value, columns$weight)
  measure(
    columns = c(product, columns$weight),
    derivation = call("/", as.symbol(product), as.symbol(columns$weight)),
    synthesis = stats::setNames(
      list(call("*", as.symbol(columns$value), as.symbol(columns$weight))),
      product
    )
  )
}

measure <- function(columns, derivation, synthesis = list()) {
  list(columns = columns, synthesis = synthesis, derivation = derivation)
}

reducer_column <- function(expr, fn) {
  args <- rlang::call_args(expr)
  if (length(args) != 1L || !rlang::is_symbol(args[[1]])) {
    stop(fn, "() takes a single column name, as in ", fn, "(spend).", call. = FALSE)
  }
  rlang::as_string(args[[1]])
}

weighted_columns <- function(expr) {
  args <- rlang::call_args(expr)
  if (length(args) != 2L || !all(vapply(args, rlang::is_symbol, logical(1)))) {
    stop("weighted.mean() takes two column names, as in weighted.mean(spend, active_days).",
         call. = FALSE)
  }
  list(value = rlang::as_string(args[[1]]), weight = rlang::as_string(args[[2]]))
}

# Weighted means need the per-customer product summed, so it lives in a reserved column
# created before bootstrapping. Encoding the definition in the name means identical
# weighted means across KPIs collapse to a single synthesised column.
product_column <- function(value, weight) {
  paste0(".wsum_", value, "_x_", weight)
}

dedupe_synthesis <- function(synthesis) {
  if (!length(synthesis)) {
    return(list())
  }
  synthesis[!duplicated(names(synthesis))]
}

reject_measure <- function(expr) {
  stop(
    rlang::as_label(expr), " is not a supported measure. Build KPIs from sum(), mean(), ",
    "weighted.mean(), and division, e.g. sum(spend) / sum(active_days).",
    call. = FALSE
  )
}
