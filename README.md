
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bootabc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bootabc)](https://CRAN.R-project.org/package=bootabc)
[![Codecov test
coverage](https://codecov.io/gh/nivr/bootabc/branch/master/graph/badge.svg)](https://app.codecov.io/gh/nivr/bootabc?branch=master)
[![R-CMD-check](https://github.com/nivr/bootabc/workflows/R-CMD-check/badge.svg)](https://github.com/nivr/bootabc/actions)
<!-- badges: end -->

The goal of bootabc is to bootstrap customer/visitor measures in
hypothesis testing. Data is gathered throughout the duration of the test
and aggregated on a per customer/visitor level.

## Performance example

To try it out for yourself with fake (generated) data and get a feel for
the performance, try the following:

    library(tidyverse)
    library(profvis)

    # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    sample_size_roughly <- 250000
    num_groups <- 4
    num_kpis <- 4
    bootstrap_iterations <- 1000

    # -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    group_names <- LETTERS[1:num_groups]
    kpi_names <- paste0("kpi_", letters[1:num_kpis])
    sample_sizes <- ceiling(stats::runif(num_groups, floor(sample_size_roughly * 0.9), ceiling(sample_size_roughly * 1.1)))

    kpis <- sample_sizes %>%
      sum() %>%
      stats::runif() %>%
      rep(num_kpis) %>%
      matrix(ncol = num_kpis, nrow = sum(sample_sizes))
    colnames(kpis) <- kpi_names

    input_data_frame <- data.frame(
      cid = sample(1:sum(sample_sizes), sum(sample_sizes)),
      experiment_variant = rep(group_names, sample_sizes)
    ) %>%
      cbind(kpis)

    expr <- rlang::parse_expr(paste0(
      "bootstrap_measures(group_by(input_data_frame,experiment_variant), ",
      paste0(kpi_names, "_aggr", " = ", "mean(", kpi_names, ")", collapse = ", "),
      ", bootstrap_iterations = bootstrap_iterations",
      ")"
    ))

    profvis::profvis({
      testthing <- eval(expr)
    })

## Code of Conduct

Please note that the bootabc project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
