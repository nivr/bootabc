# Derivations turn the bootstrapped base sums into KPIs. Each KPI is a function of
# the sum columns and the cell size `n`, evaluated per iteration on the small draws
# table -- means as sum/n, ratios as sum/sum, weighted means as sum(xw)/sum(w).
# Expressions are tracked in the registry so the object stays self-describing.

derive <- function(.boot_strap, ...) {
  expressions <- rlang::enquos(..., .named = TRUE)
  helpers <- setdiff(names(.boot_strap$cells), .boot_strap$group_columns)

  draws <- dplyr::left_join(
    .boot_strap$draws, .boot_strap$cells,
    by = .boot_strap$group_columns
  )
  draws <- dplyr::mutate(draws, !!!expressions)

  .boot_strap$draws <- draws[, setdiff(names(draws), helpers), drop = FALSE]
  .boot_strap$registry <- utils::modifyList(
    .boot_strap$registry,
    lapply(expressions, rlang::quo_get_expr)
  )
  .boot_strap
}
