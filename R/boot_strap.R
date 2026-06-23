# The boot_strap object holds the joint bootstrap distribution of the base
# sufficient statistics: the resampled column sums plus each cell's size. KPIs are
# derived from these downstream, so the object stays small and self-describing
# rather than carrying pre-computed measures.

boot_strap <- function(draws, cells, base_columns, group_columns,
                       registry = list(), meta = list()) {
  structure(
    list(
      draws = draws,
      cells = cells,
      base_columns = base_columns,
      group_columns = group_columns,
      registry = registry,
      meta = meta
    ),
    class = "boot_strap"
  )
}

bootstrap_base <- function(data, columns, iterations = 10000L, seed = NULL) {
  if (!dplyr::is.grouped_df(data)) {
    stop("`data` must be a grouped data frame; group by the variant (and any strata).")
  }
  unknown <- setdiff(columns, names(data))
  if (length(unknown)) {
    stop("Unknown column(s): ", paste(unknown, collapse = ", "), ".")
  }
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  keys <- dplyr::group_keys(data)
  groups <- dplyr::group_split(data)

  per_cell <- lapply(seq_along(groups), function(cell) {
    values <- as.matrix(groups[[cell]][columns])
    list(
      n = nrow(values),
      sums = as.data.frame(bootstrap_sums(values, iterations, seed = seed, stream = cell))
    )
  })

  draws <- do.call(rbind, lapply(seq_along(per_cell), function(cell) {
    cell_draws(keys[cell, , drop = FALSE], per_cell[[cell]]$sums, iterations)
  }))
  cells <- cbind(keys, n = vapply(per_cell, `[[`, integer(1), "n"))

  boot_strap(
    draws = draws,
    cells = cells,
    base_columns = columns,
    group_columns = dplyr::group_vars(data),
    meta = list(seed = seed, iterations = iterations)
  )
}

cell_draws <- function(key, sums, iterations) {
  data.frame(
    key[rep(1L, iterations), , drop = FALSE],
    .iteration = seq_len(iterations),
    sums,
    row.names = NULL,
    check.names = FALSE
  )
}
