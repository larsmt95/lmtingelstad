library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)

# ── Variabellister med ryddige labels ─────────────────────────────────────────

var_sections <- list(
  "Cardiorespiratory fitness" = list(
    vars = c("delta_vo2lmin", "delta_vo2mlkgmin", "delta_hfmaks", "delta_o2puls",
             "delta_rer_maks", "delta_laktat", "delta_ve_maks", "delta_borgs"),
    labels = c(
      "delta_vo2lmin"    = "VO2peak, L/min",
      "delta_vo2mlkgmin" = "VO2peak, ml/kg/min",
      "delta_hfmaks"     = "HRmax, beats/min",
      "delta_o2puls"     = "O2 pulse, mL/beat",
      "delta_rer_maks"   = "RERmax",
      "delta_laktat"     = "Lactate, mmol/L",
      "delta_ve_maks"    = "VEmax, L/min",
      "delta_borgs"      = "Borg RPE"
    )
  ),
  "Stress echocardiography" = list(
    vars = c("delta_q_lmin", "delta_sv_m_l"),
    labels = c(
      "delta_q_lmin"  = "Cardiac output, L/min",
      "delta_sv_m_l"  = "Stroke volume, mL"
    )
  ),
  "Lung function" = list(
    vars = c("delta_mvv_l_min"),
    labels = c("delta_mvv_l_min" = "MVV, L/min")
  ),
  "Blood volume" = list(
    vars = c("delta_hb"),
    labels = c("delta_hb" = "Haemoglobin mass, g")
  ),
  "Body composition" = list(
    vars = c("delta_tot_lean", "delta_legs_lean_kg"),
    labels = c(
      "delta_tot_lean"    = "Total lean mass, kg",
      "delta_legs_lean_kg" = "Leg lean mass, kg"
    )
  ),
  "Muscle characteristics" = list(
    vars = c(
      "delta_csa_mhc1", "delta_csa_mhc2", "delta_csa_total",
      "delta_proportion_mhc1", "delta_proportion_mhc2",
      "delta_caf_mhc1", "delta_caf_mhc2", "delta_caf_total",
      "delta_cafa_mhc1", "delta_cafa_mhc2", "delta_cafa_total",
      "delta_cs", "delta_hadh"
    ),
    labels = c(
      "delta_csa_mhc1"       = "CSA type I, um2",
      "delta_csa_mhc2"       = "CSA type II, um2",
      "delta_csa_total"      = "CSA total, um2",
      "delta_proportion_mhc1" = "Fibre type I, %",
      "delta_proportion_mhc2" = "Fibre type II, %",
      "delta_caf_mhc1"       = "CAF type I",
      "delta_caf_mhc2"       = "CAF type II",
      "delta_caf_total"      = "CAF total",
      "delta_cafa_mhc1"      = "CAFA type I",
      "delta_cafa_mhc2"      = "CAFA type II",
      "delta_cafa_total"     = "CAFA total",
      "delta_cs"             = "Citrate synthase",
      "delta_hadh"           = "HADH"
    )
  )
)

# ── Beregn mean ± SD og n per variabel per gruppe ────────────────────────────

group_levels <- c("BCS exercise", "BCS usual care", "Non-cancer controls")

calc_mean_sd <- function(var) {
  df_full_v2 %>%
    filter(group %in% group_levels) %>%
    group_by(group) %>%
    summarise(
      n        = sum(!is.na(.data[[var]])),
      mean_val = mean(.data[[var]], na.rm = TRUE),
      sd_val   = sd(.data[[var]],   na.rm = TRUE),
      label    = sprintf("%.2f +/- %.2f", mean_val, sd_val),
      .groups  = "drop"
    ) %>%
    mutate(group = factor(group, levels = group_levels)) %>%
    arrange(group)
}

# ── Bygg tabell rad for rad ───────────────────────────────────────────────────

build_table <- function() {
  rows <- list()
  
  for (section_name in names(var_sections)) {
    section <- var_sections[[section_name]]
    
    # Seksjonshode
    rows[[length(rows) + 1]] <- tibble(
      Variable                      = section_name,
      `n (BCS exercise)`            = NA_integer_,
      `BCS exercise`                = "",
      `n (BCS usual care)`          = NA_integer_,
      `BCS usual care`              = "",
      `n (Non-cancer controls)`     = NA_integer_,
      `Non-cancer controls`         = "",
      is_header                     = TRUE
    )
    
    for (var in section$vars) {
      if (!var %in% names(df_full_v2)) next
      
      label  <- section$labels[[var]]
      result <- calc_mean_sd(var)
      
      get_n <- function(grp) {
        r <- result %>% filter(group == grp)
        if (nrow(r) == 0) return(NA_integer_)
        as.integer(r$n)
      }
      
      get_label <- function(grp) {
        r <- result %>% filter(group == grp)
        if (nrow(r) == 0 || r$n == 0) return("-")
        r$label
      }
      
      rows[[length(rows) + 1]] <- tibble(
        Variable                  = paste0("  ", label),
        `n1`                       = get_n("BCS exercise"),
        `BCS exercise`            = get_label("BCS exercise"),
        `n2`                       = get_n("BCS usual care"),
        `BCS usual care`          = get_label("BCS usual care"),
        `n3`                       = get_n("Non-cancer controls"),
        `Non-cancer controls`     = get_label("Non-cancer controls"),
        is_header                 = FALSE
      )
    }
  }
  
  bind_rows(rows)
}

df_table <- build_table()

# ── Skriv til Excel med formatering ──────────────────────────────────────────

wb <- loadWorkbook("data/mekanismeartikkel.xlsx")

if (!"change_table" %in% names(wb)) addWorksheet(wb, "change_table")

writeData(wb, sheet = "change_table",
          df_table %>% select(-is_header),
          startRow = 1, startCol = 1)

# Stiler
header_style <- createStyle(
  fontName = "Arial", fontSize = 11, fontColour = "white",
  fgFill = "#2E75B6", textDecoration = "bold",
  halign = "left", border = "Bottom", borderColour = "#CCCCCC"
)
section_style <- createStyle(
  fontName = "Arial", fontSize = 10,
  textDecoration = "bold", fgFill = "#EEF4FB"
)
normal_style <- createStyle(fontName = "Arial", fontSize = 10)
n_col_style  <- createStyle(fontName = "Arial", fontSize = 10,
                            halign = "center", fontColour = "#595959")

# Kolonnebredder: Variable, n, mean±SD, n, mean±SD, n, mean±SD
setColWidths(wb, sheet = "change_table",
             cols = 1:7,
             widths = c(32, 6, 20, 6, 20, 6, 20))

# Header
addStyle(wb, sheet = "change_table", style = header_style,
         rows = 1, cols = 1:7, gridExpand = TRUE)

# Innholdsrader
for (i in seq_len(nrow(df_table))) {
  row_i <- i + 1
  style <- if (df_table$is_header[i]) section_style else normal_style
  addStyle(wb, sheet = "change_table", style = style,
           rows = row_i, cols = 1:7, gridExpand = TRUE)
  
  # n-kolonner sentrert og grå
  if (!df_table$is_header[i]) {
    addStyle(wb, sheet = "change_table", style = n_col_style,
             rows = row_i, cols = c(2, 4, 6), gridExpand = TRUE)
  }
}

# Fotnote
footnote_row <- nrow(df_table) + 3
writeData(wb, sheet = "change_table",
          "Values are mean +/- SD.",
          startRow = footnote_row, startCol = 1)
addStyle(wb, sheet = "change_table",
         style = createStyle(fontName = "Arial", fontSize = 9, fontColour = "#595959"),
         rows = footnote_row, cols = 1)

saveWorkbook(wb, "data/mekanismeartikkel.xlsx", overwrite = TRUE)
message("change_table eksportert")