library(dplyr)
library(broom)
library(purrr)
library(openxlsx)

# Pooled SD per variabel  ─────────────────────────────────────────────────────────
sd_table_ancova <- df_full_v2 %>%
  filter(group %in% c("BCS exercise", "BCS usual care")) %>%
  pivot_longer(
    cols = starts_with("delta_"),
    names_to = "variable",
    names_prefix = "delta_",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(sd_pooled = sd(value, na.rm = TRUE), .groups = "drop")

sd_table_change <- df_full_v2 %>%
  filter(group %in% c("BCS exercise", "Non-cancer controls")) %>%
  pivot_longer(
    cols = starts_with("delta_"),
    names_to = "variable",
    names_prefix = "delta_",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(sd_pooled = sd(value, na.rm = TRUE), .groups = "drop")

# Legg til Cohen's d  ─────────────────────────────────────────────────────────
add_cohens_d <- function(results_df, sd_table) {
  results_df %>%
    left_join(sd_table, by = "variable") %>%
    mutate(
      cohens_d = estimate / sd_pooled,
      d_magnitude = case_when(
        abs(cohens_d) < 0.2 ~ "trivial",
        abs(cohens_d) < 0.5 ~ "small",
        abs(cohens_d) < 0.8 ~ "medium",
        TRUE                ~ "large"
      )
    )
}



results_ancova <- results_ancova %>%
  select(-any_of(c("sd_pooled", "cohens_d", "d_magnitude")))

results_ancova <- add_cohens_d(results_ancova, sd_table_ancova)
head(results_ancova)

results_comparison <- results_comparison %>%
  select(-any_of(c("sd_pooled", "cohens_d", "d_magnitude")))

results_comparison <- add_cohens_d(results_comparison, sd_table_change)
head(results_comparison)