# ── Funksjon for waterfall plot ───────────────────────────────────────────────

plot_waterfall <- function(var) {
  
  delta_col <- paste0("delta_", var)
  y_label   <- var_labels[var]
  
  df_plot <- df_full_v2 %>%
    filter(group %in% c("BCS exercise", "BCS usual care", "Non-cancer controls")) %>%
    select(id, group, delta = all_of(delta_col)) %>%
    filter(!is.na(delta)) %>%
    mutate(
      group = factor(group, levels = c("BCS exercise", "BCS usual care", "Non-cancer controls"))
    ) %>%
    arrange(desc(delta)) %>%
    mutate(rank = row_number())
  
  # Andel som forbedret seg per gruppe
  pct_labels <- df_plot %>%
    group_by(group) %>%
    summarise(
      pct_improved = round(100 * mean(delta > 0, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(label = sprintf("%s: %d%% improved", group, pct_improved))
  
  y_max <- max(df_plot$delta, na.rm = TRUE)
  y_min <- min(df_plot$delta, na.rm = TRUE)
  y_range <- y_max - y_min
  
  ggplot(df_plot, aes(x = rank, y = delta, fill = group)) +
    
    geom_col(width = 0.8, alpha = 0.85) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
    
    scale_fill_manual(values = c(
      "BCS exercise"        = "#E07B54",
      "BCS usual care"      = "maroon",
      "Non-cancer controls" = "#4A90B8"
    )) +
    
    # Tre label-rader med jevn avstand
    annotate("text",
             x = max(df_plot$rank) * 0.02, y = y_max - y_range * 0.02,
             label = pct_labels$label[pct_labels$group == "BCS exercise"],
             colour = "#E07B54", hjust = 0, size = 3.5, fontface = "bold") +
    annotate("text",
             x = max(df_plot$rank) * 0.02, y = y_max - y_range * 0.12,
             label = pct_labels$label[pct_labels$group == "BCS usual care"],
             colour = "maroon", hjust = 0, size = 3.5, fontface = "bold") +
    annotate("text",
             x = max(df_plot$rank) * 0.02, y = y_max - y_range * 0.22,
             label = pct_labels$label[pct_labels$group == "Non-cancer controls"],
             colour = "#4A90B8", hjust = 0, size = 3.5, fontface = "bold") +
    
    labs(
      title = y_label,
      x     = "Participants (ranked by change)",
      y     = paste0("Change in ", y_label),
      fill  = NULL
    ) +
    
    theme_bw(base_size = 11) +
    theme(
      legend.position    = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      plot.title         = element_text(face = "bold", size = 11)
    )
}

# ── Lag plottene ──────────────────────────────────────────────────────────────

waterfall_vars <- c("cs", "hadh")

var_labels <- c(
  var_labels,           # behold de eksisterende
  "cs"   = "Citrate synthase",
  "hadh" = "HADH"
)

p_wf_cs   <- plot_waterfall("cs")
p_wf_hadh <- plot_waterfall("hadh")

# ── Sett sammen med felles legend ────────────────────────────────────────────

combined_wf <- (p_wf_cs + p_wf_hadh) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

combined_wf

# ── Lagre ────────────────────────────────────────────────────────────────────

ggsave(
  filename = "figures, tables/waterfall_cs_hadh.svg",
  plot     = combined_wf,
  width    = 12,
  height   = 6,
  units    = "in"
)