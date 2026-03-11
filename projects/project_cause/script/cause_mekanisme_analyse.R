# =============================================================================
# The CAUSE – Mekanismeartikkel
# Analyse av fysiologiske determinanter av VO2peak
# =============================================================================
# Forfattere:  [navn]
# Dato:        [dato]
# R-versjon:   [versjon]
#
# Studiedesign:
#   - BCS Exercise (n=21):        Brystkreftoverlevere, treningsgruppe (RCT)
#   - BCS Usual Care (n=13):      Brystkreftoverlevere, kontrollgruppe (RCT)
#   - Non-cancer controls (n=19): Friske kvinner, alle trente (ikke randomisert)
#
# Analyser:
#   1. ANCOVA – BCS Exercise vs BCS Usual Care (randomisert sammenligning)
#      Modell: post ~ group + pre
#   2. Change score – BCS Exercise vs Non-cancer controls (observasjonell)
#      Modell: delta ~ group  [+ alder / + baseline VO2peak]
#
# Multiple testing: Benjamini-Hochberg FDR-korreksjon per variabeldomene
# Effektstørrelse:  Cohen's d (estimate / pooled SD)
# =============================================================================


# ── 0. PAKKER ─────────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(openxlsx)


# ── 1. VARIABELLISTER ─────────────────────────────────────────────────────────

cardio_vars <- c(
  "vo2lmin", "vo2mlkgmin", "laktat", "hfmaks",
  "o2puls", "borgs", "rer_maks", "ve_maks",
  "hr_ekko", "q_lmin", "sv_m_l", "mvv_l_min"
)

muscle_vars <- c(
  "csa_mhc1", "csa_mhc2", "csa_total",
  "proportion_mhc1", "proportion_mhc2",
  "caf_mhc1", "caf_mhc2", "caf_total",
  "cafa_mhc1", "cafa_mhc2", "cafa_total",
  "cs", "hadh"
)

body_comp_vars <- c(
  "vekt", "bmi", "hoyde",
  "tot_lean", "legs_lean", "legs_lean_kg"
)

blood_vars <- c("hb")

# Primære rapporteringsvariabler (ekskluderer body comp fra hovedtabell)
report_vars <- c(cardio_vars, muscle_vars, blood_vars)

# Alle variabler tilgjengelig i datasettet
all_vars <- names(df_full) %>%
  gsub("_(pre|post)$", "", .) %>%
  unique() %>%
  setdiff(c("id", "group", "alder"))

delta_vars <- names(df_full_delta) %>%
  grep("^delta_", ., value = TRUE) %>%
  gsub("^delta_", "", .) %>%
  unique()


# ── 2. HJELPEFUNKSJONER ───────────────────────────────────────────────────────

# Tilordne variabeldomene
assign_domain <- function(df) {
  df %>%
    mutate(domain = case_when(
      variable %in% cardio_vars    ~ "cardio",
      variable %in% muscle_vars    ~ "muscle",
      variable %in% body_comp_vars ~ "body_comp",
      variable %in% blood_vars     ~ "blood",
      TRUE                         ~ "other"
    ))
}

# Benjamini-Hochberg FDR-korreksjon per domene (og evt. per modelltype)
apply_bh <- function(df, group_vars = "domain") {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(p.adj = p.adjust(p.value, method = "BH")) %>%
    ungroup()
}

# Cohen's d = estimate / pooled SD
# SD beregnes kun over de to gruppene som inngår i modellen
add_cohens_d <- function(results_df, sd_table) {
  results_df %>%
    select(-any_of(c("sd_pooled", "cohens_d", "d_magnitude"))) %>%
    left_join(sd_table, by = "variable") %>%
    mutate(
      cohens_d    = estimate / sd_pooled,
      d_magnitude = case_when(
        abs(cohens_d) < 0.2 ~ "trivial",
        abs(cohens_d) < 0.5 ~ "small",
        abs(cohens_d) < 0.8 ~ "medium",
        TRUE                ~ "large"
      )
    )
}


# ── 3. POOLED SD PER VARIABEL ─────────────────────────────────────────────────
# Beregnes separat for hver sammenligning (kun over relevante grupper)

sd_table_ancova <- df_full_v2 %>%
  filter(group %in% c("BCS exercise", "BCS usual care")) %>%
  pivot_longer(
    cols         = starts_with("delta_"),
    names_to     = "variable",
    names_prefix = "delta_",
    values_to    = "value"
  ) %>%
  group_by(variable) %>%
  summarise(sd_pooled = sd(value, na.rm = TRUE), .groups = "drop")

sd_table_change <- df_full_v2 %>%
  filter(group %in% c("BCS exercise", "Non-cancer controls")) %>%
  pivot_longer(
    cols         = starts_with("delta_"),
    names_to     = "variable",
    names_prefix = "delta_",
    values_to    = "value"
  ) %>%
  group_by(variable) %>%
  summarise(sd_pooled = sd(value, na.rm = TRUE), .groups = "drop")


# ── 4. ANCOVA: BCS EXERCISE vs BCS USUAL CARE ────────────────────────────────
# Modell: post ~ group + pre
# Referansegruppe: BCS usual care
# Estimat tolkes som: justert forskjell i post-verdi (BCS exercise − BCS usual care)

run_ancova <- function(var) {
  pre  <- paste0(var, "_pre")
  post <- paste0(var, "_post")

  model <- lm(
    as.formula(paste(post, "~ group +", pre)),
    data = df_full_v2 %>%
      filter(group %in% c("BCS exercise", "BCS usual care"))
  )

  broom::tidy(model, conf.int = TRUE) %>%
    filter(grepl("group", term)) %>%
    mutate(variable = var, df = df.residual(model))
}

results_ancova <- map_dfr(all_vars, run_ancova) %>%
  select(variable, estimate, df, conf.low, conf.high, p.value) %>%
  assign_domain() %>%
  apply_bh(group_vars = "domain") %>%
  add_cohens_d(sd_table_ancova)


# ── 5. CHANGE SCORE: BCS EXERCISE vs NON-CANCER CONTROLS ─────────────────────
# Modell: delta ~ group  [ujustert / + alder / + baseline VO2peak]
# Referansegruppe: Non-cancer controls
# Estimat tolkes som: forskjell i endring fra pre til post (BCS exercise − Non-cancer)
# NB: Ikke-randomisert sammenligning – resultater er observasjonelle

# 5a. Ujustert
run_change_unadj <- function(var) {
  delta <- paste0("delta_", var)

  model <- lm(
    as.formula(paste(delta, "~ group")),
    data = df_full_v2 %>%
      filter(group %in% c("BCS exercise", "Non-cancer controls"))
  )

  broom::tidy(model, conf.int = TRUE) %>%
    filter(term == "groupBCS exercise") %>%
    mutate(variable = var, df = df.residual(model), adjusted = "unadjusted")
}

# 5b. Justert for alder
run_change_adj_age <- function(var) {
  delta <- paste0("delta_", var)

  model <- lm(
    as.formula(paste(delta, "~ group + alder_pre")),
    data = df_full_v2 %>%
      filter(group %in% c("BCS exercise", "Non-cancer controls"))
  )

  broom::tidy(model, conf.int = TRUE) %>%
    filter(term == "groupBCS exercise") %>%
    mutate(variable = var, df = df.residual(model), adjusted = "age")
}

# 5c. Justert for baseline VO2peak
run_change_adj_vo2 <- function(var) {
  delta <- paste0("delta_", var)

  model <- lm(
    as.formula(paste(delta, "~ group + vo2mlkgmin_pre")),
    data = df_full_v2 %>%
      filter(group %in% c("BCS exercise", "Non-cancer controls"))
  )

  broom::tidy(model, conf.int = TRUE) %>%
    filter(term == "groupBCS exercise") %>%
    mutate(variable = var, df = df.residual(model), adjusted = "baseline_vo2")
}

results_comparison <- bind_rows(
  map_dfr(delta_vars, run_change_unadj),
  map_dfr(delta_vars, run_change_adj_age),
  map_dfr(delta_vars, run_change_adj_vo2)
) %>%
  select(variable, adjusted, estimate, df, conf.low, conf.high, p.value) %>%
  assign_domain() %>%
  apply_bh(group_vars = c("domain", "adjusted")) %>%   # BH per domene og modelltype
  add_cohens_d(sd_table_change)


# ── 6. RAPPORTERINGSTABELLER ──────────────────────────────────────────────────

# ANCOVA – formatert for publisering
results_ancova_report <- results_ancova %>%
  filter(variable %in% report_vars) %>%
  mutate(
    beta_ci  = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
    p.value  = round(p.value, 3),
    p.adj    = round(p.adj, 3),
    cohens_d = round(cohens_d, 2)
  ) %>%
  select(domain, variable, beta_ci, p.value, p.adj, cohens_d, d_magnitude) %>%
  arrange(domain, variable)

# Change score – formatert for publisering (ujustert som primær)
results_comparison_report <- results_comparison %>%
  filter(variable %in% report_vars) %>%
  mutate(
    beta_ci  = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
    p.value  = round(p.value, 3),
    p.adj    = round(p.adj, 3),
    cohens_d = round(cohens_d, 2)
  ) %>%
  select(domain, variable, adjusted, beta_ci, p.value, p.adj, cohens_d, d_magnitude) %>%
  arrange(domain, variable, adjusted)


# ── 7. EKSPORT TIL EXCEL ──────────────────────────────────────────────────────

wb <- loadWorkbook("data/mekanismeartikkel.xlsx")

writeData(wb, sheet = "stat_random_full",
          results_ancova_report,     startRow = 1, startCol = 1)

writeData(wb, sheet = "stat_endring_full",
          results_comparison_report, startRow = 1, startCol = 1)

saveWorkbook(wb, "data/mekanismeartikkel.xlsx", overwrite = TRUE)

message("✓ Analyse fullført og eksportert til data/mekanismeartikkel.xlsx")
