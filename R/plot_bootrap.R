plot_bootstrap <- function(boot_strap) {
  ci_bars_95 <- calculate_confidence_intervals(boot_strap)%>%
    filter(experiment_variant == "A") %>%
    pivot_longer(cols = starts_with("kpi_")) %>%
    pivot_wider(values_from = value,
                names_from = quantile) %>%
    mutate(kpi_num = row_number()) %>%
    split(seq(nrow(.))) %>%
    lapply(function(x) {
      list(
        type = "rect",
        fillcolor = "green",
        line = list(color = "black"),
        x0 = unname(x$lower95),
        x1 = unname(x$upper95),
        y0 = unname(x$kpi_num) - 0.25,
        y1 = unname(x$kpi_num) + 0.25
      )
    }) %>% unname()

  ci_bars_80 <- calculate_confidence_intervals(boot_strap)%>%
    filter(experiment_variant == "A") %>%
    pivot_longer(cols = starts_with("kpi_")) %>%
    pivot_wider(values_from = value,
                names_from = quantile) %>%
    mutate(kpi_num = row_number()) %>%
    split(seq(nrow(.))) %>%
    lapply(function(x) {
      list(
        type = "rect",
        fillcolor = "yellowgreen",
        line = list(color = "black"),
        x0 = unname(x$lower80),
        x1 = unname(x$upper80),
        y0 = unname(x$kpi_num) - 0.25,
        y1 = unname(x$kpi_num) + 0.25
      )
    }) %>% unname()

  plot_ly() %>%
    add_trace(type = "scatter",
              ) %>%
    layout(shapes = append(ci_bars_95,ci_bars_80))
}
