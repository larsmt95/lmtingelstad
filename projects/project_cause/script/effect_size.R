# =============================================================================
# Effect size calculation
# Cohen's d = model estimate / pooled SD of change scores
# =============================================================================

# ─────────────────────────────────────────────────────────────
# 1. PACKAGES
# ─────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)


# ─────────────────────────────────────────────────────────────
# 2. COMPUTE POOLED SD
# ─────────────────────────────────────────────────────────────

compute_pooled_sd <- function(groups){
  
  df_full_v2 %>%
    filter(group %in% groups) %>%
    pivot_longer(
      cols = starts_with("delta_"),
      names_to = "variable",
      names_prefix = "delta_",
      values_to = "value"
    ) %>%
    group_by(variable) %>%
    summarise(
      sd_pooled = sd(value, na.rm = TRUE),
      .groups = "drop"
    )
  
}

sd_table_ancova <- compute_pooled_sd(
  c("BCS exercise","BCS usual care")
)

sd_table_change <- compute_pooled_sd(
  c("BCS exercise","Non-cancer controls")
)


# ─────────────────────────────────────────────────────────────
# 3. ADD COHEN'S D
# ─────────────────────────────────────────────────────────────

add_cohens_d <- function(results_df, sd_table){
  
  results_df %>%
    select(-any_of(c("sd_pooled","cohens_d","d_low","d_high","d_magnitude"))) %>%
    left_join(sd_table, by = "variable") %>%
    mutate(
      
      cohens_d = ifelse(sd_pooled == 0, NA, estimate / sd_pooled),
      d_low    = conf.low  / sd_pooled,
      d_high   = conf.high / sd_pooled,
      
      d_magnitude = case_when(
        abs(cohens_d) < 0.2 ~ "trivial",
        abs(cohens_d) < 0.5 ~ "small",
        abs(cohens_d) < 0.8 ~ "medium",
        TRUE ~ "large"
      )
      
    )
  
}


# ─────────────────────────────────────────────────────────────
# 4. APPLY EFFECT SIZE
# ─────────────────────────────────────────────────────────────

results_ancova <- add_cohens_d(
  results_ancova,
  sd_table_ancova
)

results_comparison <- add_cohens_d(
  results_comparison,
  sd_table_change
)
