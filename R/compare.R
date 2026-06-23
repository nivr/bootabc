# Between-variant comparisons. Within each stratum, pair the levels of the
# comparison dimension and combine their per-iteration KPI distributions into
# ratios (lift) and differences. Variants are never compared across strata: a
# stratum is every grouping column except the comparison one.
#
# With a reference level, each variant is compared against it; otherwise all pairs
# are formed. Pairs align by iteration index -- the two variants were resampled on
# independent streams, so this is the joint distribution of the comparison. The same
# pairing is applied to the observed sums to give the comparison's point estimate.

compare <- function(.boot_strap, comparison, reference = NULL,
                    types = c("ratio", "difference")) {
  comparison <- rlang::as_string(rlang::ensym(comparison))
  types <- match.arg(types, c("ratio", "difference"), several.ok = TRUE)

  if (!comparison %in% .boot_strap$group_columns) {
    stop("`comparison` must name a grouping column: ",
         paste(.boot_strap$group_columns, collapse = ", "), ".")
  }
  kpis <- names(.boot_strap$registry)
  if (!length(kpis)) {
    stop("Derive at least one KPI with derive() before comparing.")
  }

  strata <- setdiff(.boot_strap$group_columns, comparison)
  pairs <- variant_pairs(unique(.boot_strap$draws[[comparison]]), reference)

  draws <- combine_pairs(.boot_strap$draws, comparison, strata, kpis, pairs, types,
                         keep = ".iteration")
  observed <- if (!is.null(.boot_strap$observed)) {
    combine_pairs(.boot_strap$observed, comparison, strata, kpis, pairs, types,
                  keep = character(0))
  }

  boot_strap(
    draws = draws,
    cells = comparison_cells(.boot_strap$cells, comparison, strata, pairs, types),
    base_columns = .boot_strap$base_columns,
    group_columns = c(strata, comparison, ".type"),
    observed = observed,
    registry = .boot_strap$registry,
    meta = c(.boot_strap$meta, list(comparison = comparison, reference = reference))
  )
}

combine_pairs <- function(data, comparison, strata, kpis, pairs, types, keep) {
  by <- c(strata, keep)
  use_key <- length(by) == 0L          # nothing to align on -> a single row per variant
  variant_rows <- function(variant) {
    rows <- data[data[[comparison]] == variant, c(strata, keep, kpis), drop = FALSE]
    if (use_key) rows[[".pair"]] <- 1L
    rows
  }

  out <- list()
  for (pair in pairs) {
    joined <- dplyr::inner_join(
      variant_rows(pair[["focal"]]), variant_rows(pair[["base"]]),
      by = if (use_key) ".pair" else by, suffix = c("", "__base")
    )
    for (type in types) {
      measures <- lapply(kpis, function(kpi) {
        if (type == "ratio") {
          joined[[kpi]] / joined[[paste0(kpi, "__base")]]
        } else {
          joined[[kpi]] - joined[[paste0(kpi, "__base")]]
        }
      })
      columns <- c(
        as.list(joined[strata]),
        stats::setNames(list(paste(pair[["focal"]], "vs", pair[["base"]])), comparison),
        list(.type = type),
        if (length(keep)) stats::setNames(list(joined[[keep]]), keep),
        stats::setNames(measures, kpis)
      )
      out[[length(out) + 1L]] <- do.call(
        data.frame,
        c(columns, list(check.names = FALSE, row.names = NULL, stringsAsFactors = FALSE))
      )
    }
  }
  do.call(rbind, out)
}

variant_pairs <- function(variants, reference) {
  if (!is.null(reference)) {
    if (!reference %in% variants) {
      stop("`reference` (\"", reference, "\") is not a level of the comparison column.")
    }
    lapply(setdiff(variants, reference), function(variant) c(focal = variant, base = reference))
  } else {
    lapply(
      utils::combn(variants, 2, simplify = FALSE),
      function(pair) c(focal = pair[[1]], base = pair[[2]])
    )
  }
}

# A comparison's sample size is the smaller of the two arms it pairs, so a lift in a
# thin segment is flagged as such downstream.
comparison_cells <- function(input_cells, comparison, strata, pairs, types) {
  has_n <- !is.null(input_cells) && "n" %in% names(input_cells)
  out <- list()
  for (pair in pairs) {
    label <- paste(pair[["focal"]], "vs", pair[["base"]])
    if (has_n && length(strata)) {
      focal <- input_cells[input_cells[[comparison]] == pair[["focal"]], c(strata, "n"), drop = FALSE]
      base  <- input_cells[input_cells[[comparison]] == pair[["base"]],  c(strata, "n"), drop = FALSE]
      merged <- merge(focal, base, by = strata, suffixes = c(".focal", ".base"))
      strata_cols <- as.list(merged[strata])
      n <- pmin(merged[["n.focal"]], merged[["n.base"]])
    } else if (has_n) {
      strata_cols <- list()
      n <- min(input_cells[input_cells[[comparison]] == pair[["focal"]], "n"],
               input_cells[input_cells[[comparison]] == pair[["base"]], "n"])
    } else {
      strata_cols <- list()
      n <- NA_integer_
    }
    for (type in types) {
      columns <- c(strata_cols, stats::setNames(list(label), comparison),
                   list(.type = type, n = n))
      out[[length(out) + 1L]] <- do.call(
        data.frame,
        c(columns, list(check.names = FALSE, row.names = NULL, stringsAsFactors = FALSE))
      )
    }
  }
  do.call(rbind, out)
}
