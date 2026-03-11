library(dplyr)
library(purrr)
library(tidyr)

## For a vurdere gruppekomparabilitet ved baseline

# ── 1. HJELPEFUNKSJONER ───────────────────────────────────────────────────────

# Velger automatisk t-test eller Mann-Whitney basert på Shapiro-Wilk
compare_two_groups <- function(var, group1, group2) {
  pre_col <- paste0(var, "_pre")
  if (!pre_col %in% names(df_full_v2)) return(NULL)
  
  df <- df_full_v2 %>%
    filter(group %in% c(group1, group2)) %>%
    droplevels() %>%
    select(group, value = all_of(pre_col)) %>%
    filter(!is.na(value))
  
  g1 <- df %>% filter(group == group1) %>% pull(value)
  g2 <- df %>% filter(group == group2) %>% pull(value)
  
  # Shapiro-Wilk per gruppe (kun hvis n >= 3) -> tester for normalitet 
  sw1 <- if (length(g1) >= 3) shapiro.test(g1)$p.value else NA
  sw2 <- if (length(g2) >= 3) shapiro.test(g2)$p.value else NA
  normal <- all(c(sw1, sw2) > 0.05, na.rm = TRUE)
  
  if (normal) {
    test    <- t.test(g1, g2)
    method  <- "t-test"
  } else {
    test    <- wilcox.test(g1, g2, exact = FALSE)
    method  <- "Mann-Whitney"
  }
  
  tibble(
    variable   = var,
    mean_sd_g1 = sprintf("%.1f +/- %.1f", mean(g1, na.rm=TRUE), sd(g1, na.rm=TRUE)),
    mean_sd_g2 = sprintf("%.1f +/- %.1f", mean(g2, na.rm=TRUE), sd(g2, na.rm=TRUE)),
    p.value    = round(test$p.value, 3),
    method     = method,
    sw_p_g1    = round(sw1, 3),
    sw_p_g2    = round(sw2, 3)
  )
}

## Vurdere seleksjonsskjevhet

# Velger automatisk ANOVA eller Kruskal-Wallis
compare_three_groups <- function(var) {
  pre_col <- paste0(var, "_pre")
  if (!pre_col %in% names(df_full_v2)) return(NULL)
  
  df <- df_full_v2 %>%
    droplevels() %>%
    select(group, value = all_of(pre_col)) %>%
    filter(!is.na(value))
  
  # Shapiro-Wilk per gruppe
  sw_p <- df %>%
    group_by(group) %>%
    summarise(
      sw = if (n() >= 3) shapiro.test(value)$p.value else NA,
      .groups = "drop"
    )
  
  normal <- all(sw_p$sw > 0.05, na.rm = TRUE)
  
  if (normal) {
    test   <- oneway.test(value ~ group, data = df, var.equal = FALSE)
    method <- "Welch ANOVA"
    p_val  <- test$p.value
  } else {
    test   <- kruskal.test(value ~ group, data = df)
    method <- "Kruskal-Wallis"
    p_val  <- test$p.value
  }
  
  # Mean +/- SD per gruppe
  desc <- df %>%
    group_by(group) %>%
    summarise(
      label = sprintf("%.1f +/- %.1f", mean(value, na.rm=TRUE), sd(value, na.rm=TRUE)),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = group, values_from = label)
  
  bind_cols(
    tibble(variable = var, p.value = round(p_val, 3), method = method),
    desc
  )
}


# ── 2. KJØR ANALYSER ──────────────────────────────────────────────────────────

all_report_vars <- c(cardio_vars, muscle_vars, blood_vars, body_comp_vars)

# Baseline: BCS exercise vs BCS usual care (RCT-randomisert)
baseline_rct <- map_dfr(all_report_vars, ~ compare_two_groups(
  .x,
  group1 = "BCS exercise",
  group2 = "BCS usual care"
)) %>%
  rename(
    `BCS exercise`   = mean_sd_g1,
    `BCS usual care` = mean_sd_g2
  ) %>%
  mutate(comparison = "BCS exercise vs BCS usual care") %>%
  arrange(match(variable, all_report_vars))

# Baseline: alle tre grupper (seleksjonsskjevhet)
baseline_all <- map_dfr(all_report_vars, compare_three_groups) %>%
  arrange(match(variable, all_report_vars))


# ── 3. PRINT OVERSIKT ─────────────────────────────────────────────────────────

cat("\n── Baseline: BCS Exercise vs BCS Usual Care ──\n")
print(baseline_rct, n = Inf)

cat("\n── Baseline: Alle tre grupper ──\n")
print(baseline_all, n = Inf)

# Flagg signifikante baselineforskjeller (p < 0.05)
cat("\n── Signifikante baselineforskjeller (p < 0.05) ──\n")
bind_rows(
  baseline_rct %>% mutate(analyse = "RCT"),
  baseline_all %>% mutate(analyse = "Alle tre")
) %>%
  filter(p.value < 0.05) %>%
  select(analyse, variable, p.value, method) %>%
  print()


# ── 4. EKSPORT TIL EXCEL ──────────────────────────────────────────────────────

wb <- loadWorkbook("data/mekanismeartikkel.xlsx")

writeData(wb, sheet = "eda_baseline_rct",
          baseline_rct,  startRow = 1, startCol = 1)

writeData(wb, sheet = "eda_baseline_alle",
          baseline_all, startRow = 1, startCol = 1)

saveWorkbook(wb, "data/mekanismeartikkel.xlsx", overwrite = TRUE)

message("EDA baseline eksportert")


# ── DOBBELTSJEKKE RESULTATER ──────────────────────────────────────────────────────
var <- "caf_total_pre"
pre_col <- var

# Hent data
g1 <- df_full_v2 %>% 
  filter(group == "BCS exercise") %>% 
  pull(all_of(pre_col))

g2 <- df_full_v2 %>% 
  filter(group == "BCS usual care") %>% 
  pull(all_of(pre_col))


shapiro.test(g1)
shapiro.test(g2)
  
t.test(g1, g2)

baseline_rct %>% filter(variable == "caf_total")

# Se hvem utliggeren er
df_full_v2 %>%
  filter(group == "BCS exercise") %>%
  select(id, caf_total_pre) %>%
  arrange(desc(caf_total_pre))
  

