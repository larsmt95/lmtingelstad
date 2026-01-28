library(lme4)
library(lmerTest)
library(tidyverse)
library(openxlsx)

# --- Sett opp referanser ---
ref_combinations <- list(
  list(time_ref = "round1", group_ref = "boysU14"),
  list(time_ref = "round1", group_ref = "girlsU16"),
  list(time_ref = "round2", group_ref = "girlsU16"),
  list(time_ref = "round2", group_ref = "boysU14")
)

# --- Tom tabell for alle resultater ---
all_results <- data.frame()

# --- Loop over referanser ---
for (ref in ref_combinations) {
  
  time_ref <- ref$time_ref
  group_ref <- ref$group_ref
  
  # Relevel faktorer
  paper3_longer_vol2$time <- relevel(paper3_longer_vol2$time, ref = time_ref)
  paper3_longer_vol2$group <- relevel(paper3_longer_vol2$group, ref = group_ref)
  
  # Loop over tester
  for (test_name in unique(paper3_longer_vol2$test)) {
    temp_data <- paper3_longer_vol2 %>% filter(test == test_name)
    
    m1 <- lmer(value ~ time * group + (1 | id), data = temp_data, REML = TRUE)
    
    # Koeffisienter
    model_summary <- coef(summary(m1))
    temp <- as.data.frame(model_summary)
    temp$effect <- rownames(model_summary)
    temp$test <- test_name
    temp$reference <- paste0(group_ref, "_", time_ref)
    temp$time_ref <- time_ref
    temp$group_ref <- group_ref
    
    # Konfidensintervaller
    ci <- confint(m1, method = "Wald")
    ci_df <- as.data.frame(ci)
    ci_df$effect <- rownames(ci_df)
    
    temp <- left_join(temp, ci_df, by = "effect")
    colnames(temp)[grepl("2.5 %", colnames(temp))] <- "CI_lower"
    colnames(temp)[grepl("97.5 %", colnames(temp))] <- "CI_upper"
    
    all_results <- bind_rows(all_results, temp)
  }
}

# Rename kolonner for lesbarhet
all_results <- all_results %>%
  rename(Estimate = Estimate,
         Std_Error = `Std. Error`,
         t_value = `t value`,
         p_value = `Pr(>|t|)`)

# --- Sett kolonnerekkefølge ---
all_results <- all_results %>%
  select(Estimate, Std_Error, df, t_value, p_value, effect, test, CI_lower, CI_upper,
         reference, time_ref, group_ref)

wb_paper3 <- loadWorkbook("data/paper3_main_table.xlsx")
writeData(wb_paper3, sheet = "mixed_final_2_0", all_results, startRow = 1, startCol = 1)
saveWorkbook(wb_paper3, "data/paper3_main_table.xlsx", overwrite = TRUE)


all_results
