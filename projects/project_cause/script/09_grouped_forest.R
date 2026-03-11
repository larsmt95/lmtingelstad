# Hent Cohen's d fra results_comparison (ujustert)
df_forest_std <- results_comparison %>%
  filter(
    adjusted == "unadjusted",
    variable %in% report_vars
  ) %>%
  mutate(
    group    = factor("BCS exercise", levels = c("BCS exercise", "Non-cancer controls")),
    domain   = case_when(
      variable %in% cardio_vars ~ "Cardiorespiratory",
      variable %in% muscle_vars ~ "Muscle",
      variable %in% blood_vars  ~ "Blood",
      TRUE                      ~ "Other"
    ),
    domain    = factor(domain, levels = c("Cardiorespiratory", "Blood", "Muscle")),
    var_label = recode(variable,
                       "vo2mlkgmin"      = "VO2peak (ml/kg/min)",
                       "vo2lmin"         = "VO2peak (L/min)",
                       "hfmaks"          = "HRmax",
                       "o2puls"          = "O2 pulse",
                       "rer_maks"        = "RERmax",
                       "laktat"          = "Lactate",
                       "ve_maks"         = "VEmax",
                       "borgs"           = "Borg RPE",
                       "hr_ekko"         = "HR (echo)",
                       "q_lmin"          = "Cardiac output",
                       "sv_m_l"          = "Stroke volume",
                       "mvv_l_min"       = "MVV",
                       "hb"              = "Haemoglobin mass",
                       "cs"              = "Citrate synthase",
                       "hadh"            = "HADH",
                       "caf_total"       = "CAF total",
                       "caf_mhc1"        = "CAF type I",
                       "caf_mhc2"        = "CAF type II",
                       "cafa_total"      = "CAFA total",
                       "cafa_mhc1"       = "CAFA type I",
                       "cafa_mhc2"       = "CAFA type II",
                       "csa_total"       = "CSA total",
                       "csa_mhc1"        = "CSA type I",
                       "csa_mhc2"        = "CSA type II",
                       "proportion_mhc1" = "Fibre type I (%)",
                       "proportion_mhc2" = "Fibre type II (%)"
    ),
    var_label = factor(var_label, levels = rev(unique(var_label))),
    # KI for Cohen's d (approx)
    d_ci = 1.96 * sqrt(1/21 + 1/19 + cohens_d^2 / (2 * (21 + 19)))
  )

df_forest_std <- df_forest_std %>%
  mutate(
    d_magnitude = factor(d_magnitude, levels = c("trivial", "small", "medium", "large"))
  )


ggplot(df_forest_std, aes(x = cohens_d, y = var_label, colour = d_magnitude)) +
  
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", colour = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", colour = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = c(-0.8, 0.8), linetype = "dotted", colour = "grey50", linewidth = 0.4) +
  
  geom_errorbarh(aes(xmin = cohens_d - d_ci, xmax = cohens_d + d_ci),
                 height = 0.25, linewidth = 0.8) +
  geom_point(size = 3) +
  
  scale_colour_manual(
    values = c(
      "trivial" = "grey75",
      "small"   = "grey75",
      "medium"  = "salmon",   # rosa/oransje
      "large"   = "#4CAF7D"    # grønn
    ),
    name = "Effect size"
  ) +
  
  annotate("text", x = 0.21, y = 0.4, label = "small",  size = 3, colour = "grey50", hjust = 0) +
  annotate("text", x = 0.51, y = 0.4, label = "medium", size = 3, colour = "grey50", hjust = 0) +
  annotate("text", x = 0.81, y = 0.4, label = "large",  size = 3, colour = "grey50", hjust = 0) +
  
  facet_col(~ domain, scales = "free_y", space = "free") +
  
  labs(
    title = "BCS exercise vs Non-cancer controls: effect size (Cohen's d)",
    x     = "Cohen's d (BCS exercise - Non-cancer controls)",
    y     = NULL
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position    = "top",
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 10)
  )

# ── BCS exercise vs BCS usual care (ANCOVA) ──────────────────────────────────────────────────────────────

df_forest_ancova <- results_ancova %>%
  filter(variable %in% report_vars) %>%
  mutate(
    domain = case_when(
      variable %in% cardio_vars ~ "Cardiorespiratory",
      variable %in% muscle_vars ~ "Muscle",
      variable %in% blood_vars  ~ "Blood",
      TRUE                      ~ "Other"
    ),
    domain      = factor(domain, levels = c("Cardiorespiratory", "Blood", "Muscle")),
    d_magnitude = factor(d_magnitude, levels = c("trivial", "small", "medium", "large")),
    var_label   = recode(variable,
                         "vo2mlkgmin"      = "VO2peak (ml/kg/min)",
                         "vo2lmin"         = "VO2peak (L/min)",
                         "hfmaks"          = "HRmax",
                         "o2puls"          = "O2 pulse",
                         "rer_maks"        = "RERmax",
                         "laktat"          = "Lactate",
                         "ve_maks"         = "VEmax",
                         "borgs"           = "Borg RPE",
                         "hr_ekko"         = "HR (echo)",
                         "q_lmin"          = "Cardiac output",
                         "sv_m_l"          = "Stroke volume",
                         "mvv_l_min"       = "MVV",
                         "hb"              = "Haemoglobin mass",
                         "cs"              = "Citrate synthase",
                         "hadh"            = "HADH",
                         "caf_total"       = "CAF total",
                         "caf_mhc1"        = "CAF type I",
                         "caf_mhc2"        = "CAF type II",
                         "cafa_total"      = "CAFA total",
                         "cafa_mhc1"       = "CAFA type I",
                         "cafa_mhc2"       = "CAFA type II",
                         "csa_total"       = "CSA total",
                         "csa_mhc1"        = "CSA type I",
                         "csa_mhc2"        = "CSA type II",
                         "proportion_mhc1" = "Fibre type I (%)",
                         "proportion_mhc2" = "Fibre type II (%)"
    ),
    var_label = factor(var_label, levels = rev(unique(var_label))),
    # KI for Cohen's d
    d_ci = 1.96 * sqrt(1/21 + 1/13 + cohens_d^2 / (2 * (21 + 13)))
  )

plot_forest_es <- function(df, title) {
  ggplot(df, aes(x = cohens_d, y = var_label, colour = d_magnitude)) +
    
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", colour = "grey70", linewidth = 0.4) +
    geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", colour = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = c(-0.8, 0.8), linetype = "dotted", colour = "grey50", linewidth = 0.4) +
    
    geom_errorbarh(aes(xmin = cohens_d - d_ci, xmax = cohens_d + d_ci),
                   height = 0.25, linewidth = 0.8) +
    geom_point(size = 3) +
    
    scale_colour_manual(
      values = c(
        "trivial" = "grey75",
        "small"   = "grey75",
        "medium"  = "#E07B54",
        "large"   = "#4CAF7D"
      ),
      name = "Effect size",
      drop = FALSE   # vis alle nivåer i legend selv om ikke alle er til stede
    ) +
    
    annotate("text", x = 0.21, y = 0.4, label = "small",  size = 3, colour = "grey50", hjust = 0) +
    annotate("text", x = 0.51, y = 0.4, label = "medium", size = 3, colour = "grey50", hjust = 0) +
    annotate("text", x = 0.81, y = 0.4, label = "large",  size = 3, colour = "grey50", hjust = 0) +
    
    facet_col(~ domain, scales = "free_y", space = "free") +
    
    labs(title = title, x = "Cohen's d", y = NULL) +
    
    theme_bw(base_size = 12) +
    theme(
      legend.position    = "top",
      strip.background   = element_blank(),
      strip.text         = element_text(face = "bold", size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.y        = element_text(size = 10)
    )
}

# ── Lag begge plottene ────────────────────────────────────────────────────────

p_ancova <- plot_forest_es(
  df_forest_ancova,
  title = "BCS exercise vs BCS usual care (ANCOVA)"
)

p_change <- plot_forest_es(
  df_forest_std,
  title = "BCS exercise vs Non-cancer controls (change score)"
)

# Vis side om side
forest_combined <- (p_ancova + p_change) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggsave(
  filename = "figures, tables/forest_plot_sara.svg",
  plot     = forest_combined,
  width    = 13,
  height   = 10,
  units    = "in"
)
