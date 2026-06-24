# The boot_strap object holds the joint bootstrap distribution of the base
# sufficient statistics: the resampled column sums, each cell's size, and the
# observed (full-sample) sums used for point estimates. KPIs are derived from these
# downstream, so the object stays small and self-describing rather than carrying
# pre-computed measures.

boot_strap <- function(draws, cells, base_columns, group_columns,
                       observed = NULL, registry = list(), meta = list(),
                       points = NULL) {
  structure(
    list(
      draws = draws,
      cells = cells,
      observed = observed,
      points = points,
      base_columns = base_columns,
      group_columns = group_columns,
      registry = registry,
      meta = meta
    ),
    class = "boot_strap"
  )
}

#' Print a bootstrap distribution
#'
#' Shows a one-screen summary -- the grouping, the number of cells, the iterations, and
#' the derived KPIs -- rather than the (large) resampled draws the object holds.
#'
#' @param x A `boot_strap` object.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @examples
#' customers <- dplyr::group_by(
#'   data.frame(spend = rgamma(50, 2), variant = rep(c("A", "B"), each = 25)),
#'   variant
#' )
#' bootstrap_measures(customers, arpu = mean(spend), iterations = 500, seed = 1)
#' @export
print.boot_strap <- function(x, ...) {
  comparison <- x$meta$comparison
  cat(if (is.null(comparison)) "<boot_strap> bootstrap distribution"
      else "<boot_strap> variant comparison", "\n", sep = "")
  cat("  groups:     ", paste(setdiff(x$group_columns, ".type"), collapse = ", "), "\n", sep = "")
  cat("  cells:      ", if (is.null(x$cells)) 0L else nrow(x$cells), "\n", sep = "")
  iterations <- x$meta$iterations
  cat("  iterations: ", if (is.null(iterations)) "?" else iterations, "\n", sep = "")
  kpis <- names(x$registry)
  cat("  KPIs:       ",
      if (length(kpis)) paste(kpis, collapse = ", ") else "(none derived yet)", "\n", sep = "")
  if (!is.null(comparison)) {
    cat("  comparison: ", comparison,
        if (is.null(x$meta$reference)) " (all pairs)" else paste0(" (vs ", x$meta$reference, ")"),
        "\n", sep = "")
  }
  cat("Summarise with confidence_intervals().\n")
  invisible(x)
}

bootstrap_base <- function(data, columns, iterations = 10000L, seed = NULL, na = "error") {
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

  group_vars <- dplyr::group_vars(data)
  data <- dplyr::group_by(
    resolve_na(dplyr::ungroup(data), columns, na),
    dplyr::across(dplyr::all_of(group_vars))
  )

  keys <- dplyr::group_keys(data)
  groups <- dplyr::group_split(data)

  per_cell <- lapply(seq_along(groups), function(cell) {
    values <- as.matrix(groups[[cell]][columns])
    list(
      n = nrow(values),
      observed = colSums(values),
      points = values,
      sums = as.data.frame(bootstrap_sums(values, iterations, seed = seed, stream = cell))
    )
  })

  draws <- do.call(rbind, lapply(seq_along(per_cell), function(cell) {
    cell_draws(keys[cell, , drop = FALSE], per_cell[[cell]]$sums, iterations)
  }))
  cells <- cbind(keys, n = vapply(per_cell, `[[`, integer(1), "n"))
  observed <- cbind(keys, as.data.frame(do.call(rbind, lapply(per_cell, `[[`, "observed"))))
  rownames(observed) <- NULL

  boot_strap(
    draws = draws,
    cells = cells,
    base_columns = columns,
    group_columns = dplyr::group_vars(data),
    observed = observed,
    points = lapply(per_cell, `[[`, "points"),
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

# Resolve missing values per column before bootstrapping. The default errors so
# missingness is a deliberate choice; "zero" treats NA as a structural zero, "drop"
# removes incomplete rows (warning, since that can shift the population).
resolve_na <- function(data, columns, na) {
  policy <- na_policy(na, columns)

  errors <- columns[policy == "error" &
                      vapply(columns, function(col) anyNA(data[[col]]), logical(1))]
  if (length(errors)) {
    stop("Missing values in column(s) ", paste(errors, collapse = ", "),
         "; set an `na` policy (\"zero\" or \"drop\") for them.", call. = FALSE)
  }

  dropped <- columns[policy == "drop"]
  if (length(dropped)) {
    keep <- stats::complete.cases(data[dropped])
    if (!all(keep)) {
      warning(sum(!keep), " row(s) dropped for missing ", paste(dropped, collapse = ", "),
              "; this can shift the population.", call. = FALSE)
      data <- data[keep, , drop = FALSE]
    }
  }

  for (col in columns[policy == "zero"]) {
    data[[col]][is.na(data[[col]])] <- 0
  }
  data
}

na_policy <- function(na, columns) {
  na <- unlist(na)
  policy <- stats::setNames(rep("error", length(columns)), columns)
  if (is.null(names(na))) {
    if (length(na) != 1L) {
      stop("An unnamed `na` must be a single policy applied to every column.", call. = FALSE)
    }
    policy[] <- na
  } else {
    overrides <- intersect(names(na), columns)
    policy[overrides] <- na[overrides]
  }
  unknown <- setdiff(policy, c("error", "zero", "drop"))
  if (length(unknown)) {
    stop("Unknown `na` policy: ", paste(unique(unknown), collapse = ", "),
         ". Use \"error\", \"zero\", or \"drop\".", call. = FALSE)
  }
  policy
}
