## --- MULTIPPEL REGRESJON: CHATTEN ----
# Formål: Kjøre flere lineære regresjonsmodeller for ulike responsvariabler
# med ulike kombinasjoner av prediktorer, hente R², AIC, F-statistikk og p-verdier.

## ANALYSER FOR HVERT KJØNN
mm_model <- percent_age2_with_phv %>% 
  filter(sex == "boys")
variation_fixed <- lm(cmj ~ phv_base, data = mm_model)
summary(variation_fixed)

# --- Liste over modeller med ulike prediktorkombinasjoner ---
# models_2: mer omfattende modeller med interaksjoner og flere prediktorer

models_2 <- list(
  #"sprint10" = c("sex", "sex + phv_base", "sex + height", "sex + bodymass", "sex + leglength", "sex + relforce", "sex + phv_base*sex"),
  "sprint30" = c("sex", "sex + phv_base", "sex + height", "sex + bodymass", "sex + leglength", "sex + relforce", "sex + phv_base*sex", "sex + height*sex", "sex + bodymass*sex"),
  "cmj" = c("sex", "sex + phv_base", "sex + height", "sex + bodymass", "sex + leglength", "sex + relpwr", "sex + relpwr + sex:relpwr", "sex + relforce", "sex + phv_base*sex", "sex + height*sex", "sex + bodymass*sex"),
  "cod" = c("sex", "sex + phv_base", "sex + height", "sex + bodymass", "sex + leglength", "sex + relforce", "sex + sprint30", "sex + phv_base*sex", "sex + height*sex", "sex + bodymass*sex"),
  # "yyir" = c("sex", "sex + phv_base", "sex + height", "sex + bodymass", "sex + leglength", "sex + relforce", "sex + relpwr", "sex + phv_base*sex"),
  "totforce" = c("sex", "sex + phv_base", "sex + bodymass", "sex + height", "sex + leglength", "sex + phv_base*sex", "sex + height*sex", "sex + bodymass*sex"),
  # "relforce" = c("sex", "sex + phv_base", "sex + bodymass", "sex + phv_base*sex"),
  "height" = c("sex", "sex + phv_base", "sex + phv_base*sex"),
  "bodymass" = c("sex", "sex + phv_base", "sex + phv_base*sex")
)

# models_1: enklere modeller med færre interaksjoner
models_1 <- list(
  "sprint30" = c("sex", "sex * phv_base", "sex * phv_base * age"),
  "cmj" = c("sex", "sex * phv_base", "sex * phv_base * age"),
  "cod" = c("sex", "sex * phv_base", "sex * phv_base * age"),
  "totforce" = c("sex", "sex * phv_base", "sex * phv_base * age"),
  "height" = c("sex", "sex * phv_base", "sex * phv_base * age"),
  "bodymass" = c("sex", "sex * phv_base", "sex * phv_base * age")
)

# models: standard modeller med kombinasjoner av grunnleggende prediktorer
models <- list(
  "sprint30" = c("sex", "sex * phv_base", "sex * height", "sex * bodymass"),
  "cmj" = c("sex", "sex * phv_base", "sex * height", "sex * bodymass"),
  "cod" = c("sex", "sex * phv_base", "sex * height", "sex * bodymass"),
  "totforce" = c("sex", "sex * phv_base", "sex * height", "sex * bodymass"),
  "height" = c("sex", "sex * phv_base", "sex * height", "sex * bodymass"),
  "bodymass" = c("sex", "sex * phv_base", "sex * height", "sex * bodymass")
)

models_u_sex <- list(
  "sprint30" = c("phv_base", "  height", "  bodymass"),
  "cmj" = c("  phv_base", "  height", "  bodymass"),
  "cod" = c("  phv_base", "  height", "  bodymass"),
  "totforce" = c("  phv_base", "  height", "  bodymass"),
  "height" = c("  phv_base", "  height", "  bodymass"),
  "bodymass" = c("  phv_base", "  height", "  bodymass")
)


percent_age2_with_phv_girls <- percent_age2_with_phv %>% 
  filter(sex == "girls") %>% 
  select(-sex)

percent_age2_with_phv_boys <- percent_age2_with_phv %>% 
  filter(sex == "boys") %>% 
  select(-sex)

# --- Data ---
data_whole <- percent_age2_with_phv_boys  # dataset med alle variabler

# Funksjon for å kjøre modellene og hente statistikk
run_models <- function(response, formulas, data) {
  results <- list()
  
  for (formula_str in formulas) {
    # Lag formel og kjør lineær modell
    formula <- as.formula(paste(response, "~", formula_str))
    model <- lm(formula, data = data)
    summary_model <- summary(model)
    
    # Hent modellstatistikk
    r_squared <- summary_model$r.squared
    aic_value <- AIC(model)
    f_statistic <- summary_model$fstatistic[1]
    df1 <- summary_model$fstatistic[2]
    df2 <- summary_model$fstatistic[3]
    model_p_value <- pf(f_statistic, df1, df2, lower.tail = FALSE)
    
    # Hent prediktor-estimater og p-verdier
    predictors <- rownames(summary_model$coefficients)
    p_values <- summary_model$coefficients[, 4]
    estimates <- summary_model$coefficients[, 1]
    
    # Lag resultatdataframe
    results[[formula_str]] <- data.frame(
      Response = response,
      Model = formula_str,
      R2 = r_squared,
      AIC = aic_value,
      F = f_statistic,
      df1 = df1,
      df2 = df2,
      Model_p_value = model_p_value,
      Predictor = predictors,
      Estimate = estimates,
      Predictor_p_value = p_values
    )
  }
  
  do.call(rbind, results)
}

# Kjøre modellene for alle responsvariabler
all_results <- do.call(rbind, lapply(names(models_u_sex), function(resp) {
  run_models(resp, models_u_sex[[resp]], data_whole)
}))


# --- Lagre kontrastene i Excel ---
wb_paper3 <- loadWorkbook("data/paper3_main_table.xlsx")
writeData(wb_paper3, sheet = "multippel_final", all_results, startRow = 83, startCol = 1)
saveWorkbook(wb_paper3, "data/paper3_main_table.xlsx", overwrite = TRUE)

#83, 166

run_models <- function(response, formulas, data) {
  results <- list()
  
  for (formula_str in formulas) {
    formula <- as.formula(paste(response, "~", formula_str))
    model <- lm(formula, data = data)
    summary_model <- summary(model)
    
    # Hent modellstatistikk
    r_squared <- summary_model$r.squared
    aic_value <- AIC(model)
    
    # Sjekk om F-statistikk finnes
    if (!is.null(summary_model$fstatistic)) {
      f_statistic <- summary_model$fstatistic[1]
      df1 <- summary_model$fstatistic[2]
      df2 <- summary_model$fstatistic[3]
      model_p_value <- pf(f_statistic, df1, df2, lower.tail = FALSE)
    } else {
      f_statistic <- NA
      df1 <- NA
      df2 <- NA
      model_p_value <- NA
    }
    
    # Hent prediktor-estimater og p-verdier
    predictors <- rownames(summary_model$coefficients)
    p_values <- summary_model$coefficients[, 4]
    estimates <- summary_model$coefficients[, 1]
    
    # Lag resultatdataframe
    results[[formula_str]] <- data.frame(
      Response = response,
      Model = formula_str,
      R2 = r_squared,
      AIC = aic_value,
      F = f_statistic,
      df1 = df1,
      df2 = df2,
      Model_p_value = model_p_value,
      Predictor = predictors,
      Estimate = estimates,
      Predictor_p_value = p_values
    )
  }
  
  do.call(rbind, results)
}
