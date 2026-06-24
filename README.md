<!-- README.md is generated from README.Rmd. Please edit that file -->



# bootabc

<!-- badges: start -->
[![R-CMD-check](https://github.com/nivr/bootabc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nivr/bootabc/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/nivr/bootabc/branch/master/graph/badge.svg)](https://app.codecov.io/gh/nivr/bootabc?branch=master)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

bootabc computes bootstrap confidence intervals for the KPIs of an A/B test, from data
aggregated to one row per customer. It handles compound ratio KPIs (such as spend per
active day), variant comparisons (lift and difference), CUPED variance reduction, and BCa
intervals, with the resampling done in a streaming C++ kernel.

bootabc works on data with **one row per customer**: each column is a per-customer
aggregate, either a sum (e.g. total `spend`) or a count (e.g. `active_days`, the number of
days that customer was active). A KPI such as `sum(spend) / sum(active_days)` is then total
spend divided by total active-days across customers -- the per-customer day count is made
when you build the table, not inside the KPI. Because every KPI is a ratio of column sums,
the bootstrap resamples those sums rather than re-evaluating the statistic, and the
interval is exact for the supported grammar.

## Installation

``` r
# requires a C++ toolchain (Rtools on Windows)
# install.packages("remotes")
remotes::install_github("nivr/bootabc")
```

## Quick start


``` r
library(bootabc)
library(dplyr)

set.seed(1)
customers <- group_by(
  data.frame(
    spend       = rgamma(4000, shape = 2),
    active_days = rpois(4000, 4) + 1,
    variant     = sample(c("control", "treatment"), 4000, replace = TRUE)
  ),
  variant
)

result <- bootstrap_measures(
  customers,
  arpu          = mean(spend),                   # a simple mean
  spend_per_day = sum(spend) / sum(active_days), # a compound ratio KPI
  comparison = variant, reference = "control",
  iterations = 2000, seed = 1
)

result
#> <boot_strap> variant comparison
#>   groups:     variant
#>   cells:      2
#>   iterations: 2000
#>   KPIs:       arpu, spend_per_day
#>   comparison: variant (vs control)
#> Summarise with confidence_intervals().

confidence_intervals(result, probs = c(lower = 0.025, upper = 0.975))
#>                variant      .type           kpi     estimate      lower      upper    n reliable nonfinite
#> 1 treatment vs control difference          arpu -0.052463716 -0.1410089 0.02950770 1947     TRUE         0
#> 2 treatment vs control difference spend_per_day -0.005527471 -0.0265773 0.01478313 1947     TRUE         0
#> 3 treatment vs control      ratio          arpu  0.974365528  0.9325827 1.01482755 1947     TRUE         0
#> 4 treatment vs control      ratio spend_per_day  0.986514436  0.9379736 1.03670204 1947     TRUE         0
```

A `ratio` interval that straddles 1 (or a `difference` that straddles 0) means the effect
is not distinguishable from noise -- as here, where the data is null.

## The measure grammar

KPIs are built from `sum()`, `mean()`, `weighted.mean()`, and division:

``` r
bootstrap_measures(
  customers,
  revenue       = sum(spend),
  arpu          = mean(spend),
  spend_per_day = sum(spend) / sum(active_days),
  weighted      = weighted.mean(spend, active_days)
)
```

Anything outside the grammar -- `median()`, `quantile()`, a bare column, arithmetic other
than `/` -- is rejected, because only ratios of sums have an exact sum-resampling
bootstrap.

## Comparisons and strata

Name a `comparison` column to get within-stratum lift (ratio) and difference
distributions; pass a `reference` level to compare each arm against it (otherwise all
pairs are formed). Group by extra columns to estimate each stratum separately:

``` r
bootstrap_measures(
  group_by(customers, country, variant),
  arpu = mean(spend),
  comparison = variant, reference = "control"
)
```

## CUPED variance reduction

With a pre-experiment covariate, de-noise the metric before bootstrapping -- the point
estimate is unchanged in expectation while the interval narrows:

``` r
adjusted <- cuped(customers, spend = spend_pre)
bootstrap_measures(adjusted, arpu = mean(spend), comparison = variant, reference = "control")
```

## Interval methods

`confidence_intervals(result, method = ...)` supports:

- `"percentile"` (default) -- quantiles of the draws
- `"basic"` -- those quantiles reflected about the observed estimate
- `"bca"` -- bias-corrected and accelerated, from the per-customer jackknife (a two-sample
  jackknife for comparisons)

## Plotting


``` r
plot_intervals(confidence_intervals(result))
```

<div class="figure">
<img src="man/figures/README-forest-1.png" alt="Lift (ratio) and difference for each KPI, with 95% intervals." width="100%" />
<p class="caption">Lift (ratio) and difference for each KPI, with 95% intervals.</p>
</div>

## Performance

Exact resampling is unavoidably `O(iterations x customers x columns)`, but the streaming
kernel keeps memory at `O(columns)` per iteration and runs in C++. Single-threaded timings
(indicative; hardware varies):

| customers | iterations | KPIs |  time |
|----------:|-----------:|-----:|------:|
|   100,000 |     10,000 |    2 |  2.3s |
| 1,000,000 |     10,000 |    2 | 26.4s |

``` r
n <- 1e6
data <- group_by(data.frame(
  spend = rgamma(n, 2), active_days = rpois(n, 4) + 1,
  variant = sample(c("control", "treatment"), n, replace = TRUE)
), variant)

system.time(bootstrap_measures(
  data, arpu = mean(spend), spend_per_day = sum(spend) / sum(active_days),
  comparison = variant, reference = "control", iterations = 10000, seed = 1
))
```

## License

MIT.
