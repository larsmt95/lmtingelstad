library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)

# ── 1. ANTALL KOMPLETTE PRE+POST PER VARIABEL PER GRUPPE ─────────────────────

all_report_vars <- c(cardio_vars, muscle_vars, blood_vars, body_comp_vars)

get_completeness <- function(var) {
  pre_col  <- paste0(var, "_pre")
  post_col <- paste0(var, "_post")
  
  if (!all(c(pre_col, post_col) %in% names(df_full_v2))) return(NULL)
  
  df_full_v2 %>%
    select(group, pre = all_of(pre_col), post = all_of(post_col)) %>%
    group_by(group) %>%
    summarise(
      n_total    = n(),
      n_pre      = sum(!is.na(pre)),
      n_post     = sum(!is.na(post)),
      n_complete = sum(!is.na(pre) & !is.na(post)),
      n_missing  = n_total - n_complete,
      pct_complete = round(100 * n_complete / n_total, 1),
      .groups = "drop"
    ) %>%
    mutate(variable = var)
}

df_completeness <- map_dfr(all_report_vars, get_completeness) %>%
  select(variable, group, n_total, n_pre, n_post, n_complete, n_missing, pct_complete) %>%
  arrange(variable, group)

# Oversikt: hvilke variabler har missing
df_missing_summary <- df_completeness %>%
  group_by(variable) %>%
  summarise(
    total_missing = sum(n_missing),
    .groups = "drop"
  ) %>%
  filter(total_missing > 0) %>%
  arrange(desc(total_missing))

cat("\n── Variabler med missing data ──\n")
print(df_missing_summary, n = Inf)


# ── 2. ER MISSING SYSTEMATISK? ────────────────────────────────────────────────
# Sammenlign baseline VO2peak og alder mellom de med og uten komplette data
# Bruker muskelvariablene som eksempel siden biopsi er den viktigste kilden til missing

# Lag indikator: har komplett pre+post for minst én muskelvariabel
df_full_v2 <- df_full_v2 %>%
  mutate(
    has_muscle_complete = rowSums(
      across(
        all_of(paste0(muscle_vars[muscle_vars %in% gsub("_pre|_post", "", names(df_full_v2))], "_pre")),
        ~ !is.na(.x)
      )
    ) > 0
  )

# Sammenlign baseline VO2peak og alder mellom komplette vs. ikke-komplette
df_missing_check <- df_full_v2 %>%
  select(group, has_muscle_complete, vo2mlkgmin_pre, alder_pre) %>%
  group_by(group, has_muscle_complete) %>%
  summarise(
    n             = n(),
    vo2_mean_sd   = sprintf("%.1f +/- %.1f",
                            mean(vo2mlkgmin_pre, na.rm = TRUE),
                            sd(vo2mlkgmin_pre,   na.rm = TRUE)),
    age_mean_sd   = sprintf("%.1f +/- %.1f",
                            mean(alder_pre, na.rm = TRUE),
                            sd(alder_pre,   na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(has_muscle_complete = ifelse(has_muscle_complete, "Complete", "Missing"))

cat("\n── Systematisk missing: baseline VO2peak og alder ──\n")
print(df_missing_check, n = Inf)

# T-test: er baseline VO2peak forskjellig mellom komplette og ikke-komplette?
missing_test <- df_full_v2 %>%
  group_by(group) %>%
  summarise(
    p_vo2 = tryCatch({
      t.test(vo2mlkgmin_pre ~ has_muscle_complete)$p.value %>% round(3)
    }, error = function(e) NA),
    p_age = tryCatch({
      t.test(alder_pre ~ has_muscle_complete)$p.value %>% round(3)
    }, error = function(e) NA),
    .groups = "drop"
  )

cat("\n── P-verdier: komplette vs. ikke-komplette ──\n")
print(missing_test)


# ── 3. N PER ANALYSE (COMPLETE CASE) ─────────────────────────────────────────

n_per_analysis <- map_dfr(all_report_vars, function(var) {
  pre_col  <- paste0(var, "_pre")
  post_col <- paste0(var, "_post")
  delta_col <- paste0("delta_", var)
  
  # ANCOVA: BCS exercise vs BCS usual care
  n_ancova <- df_full_v2 %>%
    filter(group %in% c("BCS exercise", "BCS usual care")) %>%
    filter(!is.na(.data[[pre_col]]), !is.na(.data[[post_col]])) %>%
    count(group) %>%
    pivot_wider(names_from = group, values_from = n) %>%
    mutate(analysis = "ANCOVA")
  
  # Change: BCS exercise vs Non-cancer
  n_change <- df_full_v2 %>%
    filter(group %in% c("BCS exercise", "Non-cancer controls")) %>%
    filter(!is.na(.data[[delta_col]])) %>%
    count(group) %>%
    pivot_wider(names_from = group, values_from = n) %>%
    mutate(analysis = "Change")
  
  bind_rows(n_ancova, n_change) %>%
    mutate(variable = var)
}) %>%
  select(variable, analysis, everything()) %>%
  arrange(variable, analysis)

cat("\n── N per analyse (complete case) ──\n")
print(n_per_analysis, n = Inf)


# ── 4. EKSPORT ────────────────────────────────────────────────────────────────

wb <- loadWorkbook("data/mekanismeartikkel.xlsx")

writeData(wb, sheet = "missing_completeness",
          df_completeness,  startRow = 1, startCol = 1)

writeData(wb, sheet = "missing_systematic",
          df_missing_check, startRow = 1, startCol = 1)

writeData(wb, sheet = "missing_n_per_analysis",
          n_per_analysis,   startRow = 1, startCol = 1)

saveWorkbook(wb, "data/mekanismeartikkel.xlsx", overwrite = TRUE)

message("Missing-dokumentasjon eksportert")