# --- Rounding rules for each test ---
rounding_rules <- list(
  cmj = 1,
  sprint10 = 2,
  sprint30 = 2,
  bodymass = 1,
  height = 0,
  age = 1,
  cod = 2,
  yyir = 0,
  totforce = 0,
  relforce = 1,
  relpwr = 1,
  totpwr = 0,
  meanhams = 0,
  meanadd = 0,
  meanabd = 0,
  leglength = 2,
  ir = 0,
  erot = 0,
  relir = 2,
  reler = 2,
  phv = 2,
  relhams = 2,
  reladd = 2,
  relabd = 2
)

# --- Eksempelsjekk for height ---
check <- paper3_longer_vol2 %>% 
  filter(test == "height", !is.na(group)) %>% 
  group_by(id) %>%
  filter(n_distinct(time) >= 2) %>%  # bare inkludere de med min. 2 tidspunkter
  ungroup() %>% 
  group_by(time, group) %>%
  summarise(
    mean = mean(value, na.rm = TRUE), # gjennomsnitt per gruppe og tid
    sd = sd(value, na.rm = TRUE),     # standardavvik
    n = n()                           # antall observasjoner
  ) %>% 
  arrange(group) %>% 
  print()


# --- Hovedløkken: lag tabell for alle tester ---
results <- data.frame()
# Get unique categories in `test`
unique_tests <- unique(paper3_longer_vol2$test)

# Loop over each `test` category
for (test_name in unique_tests) {
  # Filter for the current test and IDs with all time points
  
  decimal_places <- rounding_rules[[test_name]]
  
  temp_result <- paper3_longer_vol2 %>%
    filter(test == test_name) %>%
    group_by(id, test) %>%
    filter(n_distinct(time) >= 2) %>%
    ungroup() %>% 
    group_by(time, group) %>%
    summarise(
      mean = round(mean(value, na.rm = TRUE), decimal_places),  # Calculate mean
      sd = round(sd(value, na.rm = TRUE), decimal_places),
      n = n(),  # Count rows
      .groups = "drop" 
    ) %>% 
    arrange(group) %>% 
    mutate(test = test_name)  # Add a column for the current test
  
  
  # Save the result to the list
  results <- bind_rows(results, temp_result)
}

# --- Lagre til Excel ---

wb_paper3 <- loadWorkbook("data/paper3_main_table.xlsx")
writeData(wb_paper3, sheet = "table_final", results, startRow = 1, startCol = 1)
saveWorkbook(wb_paper3, "data/paper3_main_table.xlsx", overwrite = TRUE)