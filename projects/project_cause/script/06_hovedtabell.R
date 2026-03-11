library(dplyr)
library(tidyr)
library(openxlsx)

# ── 1. BEREGN MEAN ± SD FOR PRE OG POST PER GRUPPE ───────────────────────────

all_report_vars <- c(cardio_vars, blood_vars, muscle_vars, body_comp_vars)

get_descriptives <- function(var) {
  pre_col  <- paste0(var, "_pre")
  post_col <- paste0(var, "_post")
  
  # Sjekk at kolonnene finnes
  if (!all(c(pre_col, post_col) %in% names(df_full_v2))) return(NULL)
  
  df_full_v2 %>%
    select(group, pre = all_of(pre_col), post = all_of(post_col)) %>%
    group_by(group) %>%
    summarise(
      n         = sum(!is.na(pre)),
      pre_mean  = mean(pre,  na.rm = TRUE),
      pre_sd    = sd(pre,    na.rm = TRUE),
      post_mean = mean(post, na.rm = TRUE),
      post_sd   = sd(post,   na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(
      variable  = var,
      pre_label  = sprintf("%.1f +/- %.1f", pre_mean,  pre_sd),
      post_label = sprintf("%.1f +/- %.1f", post_mean, post_sd)
    )
}

df_desc <- map_dfr(all_report_vars, get_descriptives)

# ── 2. PIVOT TIL BRED FORMAT (én rad per variabel, kolonner per gruppe) ───────

df_desc_wide <- df_desc %>%
  select(variable, group, n, pre_label, post_label) %>%
  pivot_wider(
    names_from  = group,
    values_from = c(n, pre_label, post_label)
  ) %>%
  # Rydd kolonnenavn
  rename_with(~ gsub(" ", "_", .x))

# ── 3. HENT P-VERDIER FRA MODELLENE ──────────────────────────────────────────

# ANCOVA: BCS exercise vs BCS usual care
p_ancova <- results_ancova %>%
  select(variable, p_ancova = p.value, p_ancova_adj = p.adj)

# Change: BCS exercise vs Non-cancer (ujustert som primær)
p_change <- results_comparison %>%
  filter(adjusted == "unadjusted") %>%
  select(variable, p_change = p.value, p_change_adj = p.adj)

# ── 4. SETT SAMMEN FERDIG TABELL ─────────────────────────────────────────────

df_table <- df_desc_wide %>%
  left_join(p_ancova, by = "variable") %>%
  left_join(p_change, by = "variable") %>%
  mutate(
    domain = case_when(
      variable %in% cardio_vars    ~ "Cardiorespiratory",
      variable %in% muscle_vars    ~ "Muscle",
      variable %in% body_comp_vars ~ "Body composition",
      variable %in% blood_vars     ~ "Blood",
      TRUE                         ~ "Other"
    ),
    var_label = recode(variable,
                       "vo2mlkgmin"      = "VO2peak (ml/kg/min)",
                       "vo2lmin"         = "VO2peak (L/min)",
                       "hfmaks"          = "HRmax (bpm)",
                       "o2puls"          = "O2 pulse (mL/beat)",
                       "rer_maks"        = "RERmax",
                       "laktat"          = "Lactate (mmol/L)",
                       "ve_maks"         = "VEmax (L/min)",
                       "borgs"           = "Borg RPE",
                       "q_lmin"          = "Cardiac output (L/min)",
                       "sv_m_l"          = "Stroke volume (mL)",
                       "mvv_l_min"       = "MVV (L/min)",
                       "hb"              = "Haemoglobin mass (g)",
                       "cs"              = "Citrate synthase",
                       "hadh"            = "HADH",
                       "caf_total"       = "CAF total",
                       "caf_mhc1"        = "CAF type I",
                       "caf_mhc2"        = "CAF type II",
                       "cafa_total"      = "CAFA total",
                       "cafa_mhc1"       = "CAFA type I",
                       "cafa_mhc2"       = "CAFA type II",
                       "csa_total"       = "CSA total (um2)",
                       "csa_mhc1"        = "CSA type I (um2)",
                       "csa_mhc2"        = "CSA type II (um2)",
                       "proportion_mhc1" = "Fibre type I (%)",
                       "proportion_mhc2" = "Fibre type II (%)",
                       "vekt"            = "Body weight (kg)",
                       "bmi"             = "BMI (kg/m2)",
                       "hoyde"           = "Height (cm)",
                       "tot_lean"        = "Total lean mass (kg)",
                       "legs_lean"       = "Leg lean mass (g)",
                       "legs_lean_kg"    = "Leg lean mass (kg)"
    ),
    # Runde p-verdier
    p_ancova     = round(p_ancova, 3),
    p_ancova_adj = round(p_ancova_adj, 3),
    p_change     = round(p_change, 3),
    p_change_adj = round(p_change_adj, 3)
  ) %>%
  arrange(domain, variable) %>%
  select(
    domain,
    variable = var_label,
    # BCS exercise
    `n (BCS exercise)`           = `n_BCS_exercise`,
    `Pre (BCS exercise)`         = `pre_label_BCS_exercise`,
    `Post (BCS exercise)`        = `post_label_BCS_exercise`,
    # BCS usual care
    `n (BCS usual care)`         = `n_BCS_usual_care`,
    `Pre (BCS usual care)`       = `pre_label_BCS_usual_care`,
    `Post (BCS usual care)`      = `post_label_BCS_usual_care`,
    # Non-cancer
    `n (Non-cancer)`             = `n_Non-cancer_controls`,
    `Pre (Non-cancer)`           = `pre_label_Non-cancer_controls`,
    `Post (Non-cancer)`          = `post_label_Non-cancer_controls`,
    # P-verdier
    `p (Ex vs UC)`               = p_ancova,
    `p.adj (Ex vs UC)`           = p_ancova_adj,
    `p (Ex vs Non-cancer)`       = p_change,
    `p.adj (Ex vs Non-cancer)`   = p_change_adj
  )

# ── 5. EKSPORT TIL EXCEL ──────────────────────────────────────────────────────

wb <- loadWorkbook("data/mekanismeartikkel.xlsx")

writeData(wb, sheet = "hoveddtabell", df_table, startRow = 1, startCol = 1)

saveWorkbook(wb, "data/mekanismeartikkel.xlsx", overwrite = TRUE)

message("Tabell eksportert til sheet 'tabell2'")
