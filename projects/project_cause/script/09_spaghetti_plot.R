# ── Variabelliste og labels ───────────────────────────────────────────────────

spaghetti_vars <- c("vo2mlkgmin", "o2puls", "cs", "hadh", "caf_total", "hb")

var_labels <- c(
  "vo2mlkgmin" = "VO2peak (ml/kg/min)",
  "o2puls"     = "O2 pulse (mL/beat)",
  "cs"         = "Citrate synthase",
  "hadh"       = "HADH",
  "caf_total"  = "CAF total",
  "hb"         = "Haemoglobin mass (g)"
)

# ── Funksjon ──────────────────────────────────────────────────────────────────

plot_spaghetti <- function(var) {
  
  pre_col  <- paste0(var, "_pre")
  post_col <- paste0(var, "_post")
  y_label  <- var_labels[var]
  
  df_spaghetti <- df_full_v2 %>%
    filter(group %in% c("Non-cancer controls", "BCS exercise", "BCS usual care")) %>%
    select(id, group, pre = all_of(pre_col), post = all_of(post_col)) %>%
    pivot_longer(
      cols      = c(pre, post),
      names_to  = "timepoint",
      values_to = "value"
    ) %>%
    mutate(
      timepoint = factor(timepoint, levels = c("pre", "post")),
      group     = factor(group, levels = c("Non-cancer controls", "BCS exercise", "BCS usual care"))
    )
  
  df_mean <- df_spaghetti %>%
    group_by(group, timepoint) %>%
    summarise(
      mean_val      = mean(value, na.rm = TRUE),
      sd_val        = sd(value, na.rm = TRUE),
      n             = sum(!is.na(value)),
      se            = sd_val / sqrt(n),
      ci            = qt(0.975, df = n - 1) * se,
      .groups       = "drop"
    ) %>%
    mutate(
      group         = factor(group, levels = c("Non-cancer controls", "BCS exercise", "BCS usual care")),
      mean_sd_label = sprintf("%.1f +/- %.1f", mean_val, sd_val)
    )
  
  ggplot(df_spaghetti, aes(x = timepoint, y = value, group = interaction(id, group))) +
    
    geom_line(alpha = 0.4, linewidth = 0.5, colour = "grey70") +
    geom_point(alpha = 0.5, size = 1.5, colour = "grey70") +
    
    geom_errorbar(data = df_mean,
                  aes(x = timepoint, y = mean_val, group = group, colour = group,
                      ymin = mean_val - ci, ymax = mean_val + ci),
                  width = 0.08, linewidth = 0.9,
                  inherit.aes = FALSE) +
    geom_line(data = df_mean,
              aes(x = timepoint, y = mean_val, group = group, colour = group),
              linewidth = 1.8, inherit.aes = FALSE) +
    geom_point(data = df_mean,
               aes(x = timepoint, y = mean_val, colour = group),
               size = 3.5, inherit.aes = FALSE) +
    geom_text(data = df_mean,
              aes(x = timepoint, y = mean_val, 
                  label = sprintf("%.1f", mean_val), 
                  colour = group),
              vjust = -1.8, size = 3, fontface = "bold",
              inherit.aes = FALSE) +
    
    facet_wrap(~ group) +
    
    scale_colour_manual(values = c(
      "BCS exercise"        = "#E07B54",
      "BCS usual care"      = "maroon",
      "Non-cancer controls" = "#4A90B8"
    )) +
    
    labs(
      title  = y_label,
      x      = NULL,
      y      = y_label,
      colour = NULL
    ) +
    
    theme_classic(base_size = 11) +
    theme(
      legend.position  = "none",
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = 11),
      axis.text.x      = element_text(size = 10),
      plot.title       = element_text(face = "bold", size = 11)
    )
}

# ── Lag alle plottene og sett sammen ─────────────────────────────────────────

plots <- map(spaghetti_vars, plot_spaghetti)

combined <- wrap_plots(plots, ncol = 2) +
  plot_annotation(
    title = "Individual changes pre to post by group",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

combined

# ── Lagre ────────────────────────────────────────────────────────────────────

ggsave(
  filename = "figures, tables/spaghetti_combined.svg",
  plot     = combined,
  width    = 14,
  height   = 18,
  units    = "in"
)