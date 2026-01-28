# --- AGE-SEX contrasts ----
# Formål: Beregne estimerte marginale middelverdier (emmeans) og kontraster for hver test
# med hensyn til tid og gruppe, med ref. kategori som basis for sammenligning.

# --- Sett referansekategorier ---
paper3_longer_vol2$time <- relevel(paper3_longer_vol2$time, ref = "round3")
paper3_longer_vol2$group <- relevel(paper3_longer_vol2$group, ref = "boysU16")

# --- Tom tabell for å lagre kontraster ---
summary_table_m1 <- data.frame()  
unique_tests <- unique(paper3_longer_vol2$test)

# --- Loop over alle tester ---
for (test_name in unique_tests) {
  temp_result <- paper3_longer_vol2 %>% filter(test == test_name)
  
  m1 <- lmer(value ~ time * group + (1 | id), data = temp_result, REML = TRUE)
  
  # Beregn estimerte marginale middelverdier og kontraster
  emm_results <- emmeans(m1, pairwise ~ time | group)
  
  # Lagre kontrastene
  contrast_results <- emm_results$contrasts %>% 
    as.data.frame() %>%
    mutate(test = test_name)  # Legg til testnavn
  
  # Legg til kontrastene i samlet tabell
  summary_table_m1 <- rbind(summary_table_m1, contrast_results)
}

summary_table_m1_ref2 <- summary_table_m1 

summary_table_m1_ref3 <- summary_table_m1 %>% 
  mutate(contrast = recode(contrast,
                           "round3 - round2" = 'T2-T3'))

# --- Lagre kontrastene i Excel ---
wb_paper3 <- loadWorkbook("data/paper3_main_table.xlsx")
writeData(wb_paper3, sheet = "estimates_final", summary_table_m1, startRow = 1, startCol = 1)
saveWorkbook(wb_paper3, "data/paper3_main_table.xlsx", overwrite = TRUE)


summary_table_m1
