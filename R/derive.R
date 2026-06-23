# Derivations turn the bootstrapped base sums into KPIs. Each KPI is a function of
# the sum columns and the cell size `n`, evaluated on the small draws table -- means
# as sum/n, ratios as sum/sum, weighted means as sum(xw)/sum(w). The same expressions
# are applied to the observed (full-sample) sums to give point estimates. Expressions
# are tracked in the registry so the object stays self-describing.

derive <- function(.boot_strap, ...) {
  expressions <- rlang::enquos(..., .named = TRUE)
  derive_one <- function(df) {
    apply_derivations(df, .boot_strap$cells, .boot_strap$group_columns, expressions)
  }

  .boot_strap$draws <- derive_one(.boot_strap$draws)
  if (!is.null(.boot_strap$observed)) {
    .boot_strap$observed <- derive_one(.boot_strap$observed)
  }
  .boot_strap$registry <- utils::modifyList(
    .boot_strap$registry, lapply(expressions, rlang::quo_get_expr)
  )
  .boot_strap
}

apply_derivations <- function(df, cells, group_columns, expressions) {
  helpers <- setdiff(names(cells), group_columns)
  joined <- dplyr::left_join(df, cells, by = group_columns)
  joined <- dplyr::mutate(joined, !!!expressions)
  joined[, setdiff(names(joined), helpers), drop = FALSE]
}
