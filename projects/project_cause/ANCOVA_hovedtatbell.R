library(dplyr)
library(broom)
library(purrr)
library(openxlsx)


names(df_full_v2)

## hvilke variabler skal analyseres
cardio_vars <- c(
  "vo2lmin",
  "vo2mlkgmin",
  "laktat",
  "hfmaks",
  "o2puls",
  "borgs",
  "rer_maks",
  "ve_maks"
)

muscle_vars <- c(
  "csa_mhc1",
  "csa_mhc2",
  "csa_total",
  "proportion_mhc1",
  "proportion_mhc2",
  "caf_mhc1",
  "caf_mhc2",
  "caf_total",
  "cafa_mhc1",
  "cafa_mhc2",
  "cafa_total",
  "cs",
  "hadh"
)

all_vars <- names(df_full) %>%
  gsub("_(pre|post)$", "", .) %>%
  unique() %>% 
  setdiff(c("id", "group","alder"))

delta_vars <- names(df_full_delta) %>%
  grep("^delta_", ., value = TRUE) %>% 
  gsub("^delta_", "", .) %>%
  unique()

cardio_vars <- c(
  "vo2lmin", "vo2mlkgmin", "laktat", "hfmaks", 
  "o2puls", "borgs", "rer_maks", "ve_maks",
  "hr_ekko", "q_lmin", "sv_m_l", "mvv_l_min"  # ekko + MVV hører hit
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

## ANCOVA (BCS exercise vs usual care) ─────────────────────────────────────────────────────────
    # modell: post = group + baseline

run_ancova <- function(var){
  
  pre  <- paste0(var,"_pre")
  post <- paste0(var,"_post")
  
  model <- lm(
    as.formula(paste(post,"~ group +",pre)),
    data = df_full_v2 %>% 
      filter(group %in% c("BCS exercise","BCS usual care"))
  )
  
  broom::tidy(model, conf.int = TRUE) %>%
    filter(grepl("group", term)) %>%
    mutate(
      variable = var,
      df = df.residual(model)
    )
}

results_ancova <- map_dfr(all_vars, run_ancova)
summary(results_ancova)

df_full_v2 %>% select(group)


## Change score analyse (BCS vs non-cancer) ─────────────────────────────────────────────────────────

run_change_unadj <- function(var){
  
  delta <- paste0("delta_",var)
  
  model <- lm(
    as.formula(paste(delta,"~ group")),
    data = df_full_v2 %>% filter(group %in% c("BCS exercise","Non-cancer controls"))
  )
  
  broom::tidy(model, conf.int = TRUE) %>%
    filter(term == "groupBCS exercise") %>%
    mutate(
      variable = var,
      df = df.residual(model)
    )
}

results_change <- map_dfr(delta_vars, run_change)


  # Justert for alder
run_change_adj_age <- function(var) {
  delta <- paste0("delta_", var)
  
  model <- lm(
    as.formula(paste(delta, "~ group + alder_pre")),
    data = df_full_v2 %>% filter(group %in% c("BCS exercise", "Non-cancer controls"))
  )
  
  broom::tidy(model, conf.int = TRUE) %>%
    filter(term == "groupBCS exercise") %>%
    mutate(variable = var, df = df.residual(model), adjusted = "Age")
}

  # Endring justert for baseline VO2peak
run_change_adj_pre <- function(var) {
  delta <- paste0("delta_", var)
  pre   <- paste0("pre_", var)       # <-- baseline av samme variabel
  
  model <- lm(
    as.formula(paste(delta, "~ group +", "vo2mlkgmin_pre")),
    data = df_full_v2 %>% filter(group %in% c("BCS exercise", "Non-cancer controls"))
  )
  
  broom::tidy(model, conf.int = TRUE) %>%
    filter(term == "groupBCS exercise") %>%
    mutate(variable = var, df = df.residual(model), adjusted = "Baseline")
}

results_comparison <- bind_rows(
  map_dfr(delta_vars, run_change_unadj),
  map_dfr(delta_vars, run_change_adj_age),
  map_dfr(delta_vars, run_change_adj_pre)
) %>% 
  print(n = Inf)

## Rydde resultatene ─────────────────────────────────────────────────────────

# trening vs ikke-trening
results_ancova <- results_ancova %>%
  select(variable, estimate, df, conf.low, conf.high, p.value) %>%
  print(n = Inf)

  ## KORRIGERT
results_ancova_korr <- results_ancova %>%
  mutate(domain = case_when(
    variable %in% cardio_vars    ~ "cardio",
    variable %in% muscle_vars    ~ "muscle",
    variable %in% body_comp_vars ~ "body_comp",
    variable %in% blood_vars     ~ "blood",
    TRUE                         ~ "other"  # fanger opp tidtil_q og lignende
  )) %>% 
  group_by(domain) %>%
    mutate(p.adj = p.adjust(p.value, method = "BH")) %>%
    ungroup()

# trening vs kontroll
results_comparison <- results_change %>%
  select(variable, estimate, df, conf.low, conf.high, p.value) %>% 
  print(n = Inf)

results_comparison_korr <- results_change %>%
mutate(domain = case_when(
  variable %in% cardio_vars    ~ "cardio",
  variable %in% muscle_vars    ~ "muscle",
  variable %in% body_comp_vars ~ "body_comp",
  variable %in% blood_vars     ~ "blood",
  TRUE                         ~ "other"  # fanger opp tidtil_q og lignende
  )) %>% 
  group_by(domain) %>%
    mutate(p.adj = p.adjust(p.value, method = "BH")) %>%
    ungroup()

names(results_ancova)

 ## Finere tabell
results_ancova %>%
  mutate(
    beta_ci = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)
  ) %>%
  select(variable, beta_ci, p.value)


# laste til excel (random)
wb_mekanisme <- loadWorkbook("data/mekanismeartikkel.xlsx")
writeData(wb_mekanisme, sheet = "stat_random", results_ancova, startRow = 1, startCol = 1)
saveWorkbook(wb_mekanisme, "data/mekanismeartikkel.xlsx", overwrite = TRUE)

# laste til excel (non cancer)
wb_mekanisme <- loadWorkbook("data/mekanismeartikkel.xlsx")
writeData(wb_mekanisme, sheet = "stat_endring", results_comparison, startRow = 1, startCol = 1)
saveWorkbook(wb_mekanisme, "data/mekanismeartikkel.xlsx", overwrite = TRUE)

# laste til excel (random_korr)
wb_mekanisme <- loadWorkbook("data/mekanismeartikkel.xlsx")
writeData(wb_mekanisme, sheet = "stat_random_korr", results_ancova_korr, startRow = 1, startCol = 1)
saveWorkbook(wb_mekanisme, "data/mekanismeartikkel.xlsx", overwrite = TRUE)

# laste til excel (non cancer_korr)
wb_mekanisme <- loadWorkbook("data/mekanismeartikkel.xlsx")
writeData(wb_mekanisme, sheet = "stat_endring_korr", results_comparison_korr, startRow = 1, startCol = 1)
saveWorkbook(wb_mekanisme, "data/mekanismeartikkel.xlsx", overwrite = TRUE)

