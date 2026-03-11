library(ggplot2)
library(dplyr)

# ── Data ──────────────────────────────────────────────────────────────────────

df_scatter <- df_full_v2 %>%
  filter(group %in% c("BCS exercise", "Non-cancer controls")) %>%
  select(id, group, delta_vo2mlkgmin, delta_cs, delta_caf_total, delta_hadh) %>%
  mutate(group = factor(group, levels = c("BCS exercise", "Non-cancer controls")))

# ── Funksjon for scatter + korrelasjonslinje ──────────────────────────────────

plot_scatter <- function(x_var, y_var, x_label, y_label) {
  
  df_plot <- df_scatter %>%
    select(id, group, x = all_of(x_var), y = all_of(y_var)) %>%
    filter(!is.na(x), !is.na(y))
  
  # Korrelasjon per gruppe
  cor_labels <- df_plot %>%
    group_by(group) %>%
    summarise(
      r     = cor(x, y, method = "pearson"),
      p     = cor.test(x, y)$p.value,
      x_pos = max(x, na.rm = TRUE),
      y_pos = max(y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = sprintf("r = %.2f, p = %.3f", r, p),
      # Forskjøv andre gruppe nedover så labels ikke overlapper
      y_pos = y_pos - (row_number() - 1) * diff(range(df_plot$y, na.rm = TRUE)) * 0.08
    )
  
  ggplot(df_plot, aes(x = x, y = y, colour = group)) +
    
    geom_point(size = 2.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1) +
    
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
    
    geom_text(data = cor_labels,
              aes(x = x_pos, y = y_pos, label = label, colour = group),
              hjust = 1, size = 3.5, fontface = "italic",
              inherit.aes = FALSE) +
    
    scale_colour_manual(values = c(
      "BCS exercise"        = "#E07B54",
      "Non-cancer controls" = "#4A90B8"
    )) +
    
    labs(x = x_label, y = y_label, colour = NULL) +
    
    theme_bw(base_size = 12) +
    theme(
      legend.position  = "top",
      panel.grid.minor = element_blank()
    )
}

# ── Lag plottene ──────────────────────────────────────────────────────────────

p_cs <- plot_scatter(
  x_var   = "delta_vo2mlkgmin",
  y_var   = "delta_hadh",
  x_label = "Change in VO2peak (ml/kg/min)",
  y_label = "Change in HADH"
)

p_caf <- plot_scatter(
  x_var   = "delta_vo2mlkgmin",
  y_var   = "delta_caf_total",
  x_label = "Change in VO2peak (ml/kg/min)",
  y_label = "Change in CAF total"
)

# ── Vis side om side ──────────────────────────────────────────────────────────

library(patchwork)

p_cs + p_caf +
  plot_annotation(
    title = "Correlation between change in VO2peak and peripheral adaptations",
    theme = theme(plot.title = element_text(size = 13, face = "bold"))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")
