library(dplyr)
library(broom)
library(purrr)
library(ggplot2)
library(patchwork)

# ── QQ-plot per modell ────────────────────────────────────────────────────────
# Kjører alle modeller på nytt og henter residualer

# ANCOVA
get_residuals_ancova <- function(var){
  pre  <- paste0(var, "_pre")
  post <- paste0(var, "_post")
  
  df <- df_full_v2 %>%
    dplyr::filter(group %in% c("BCS exercise","BCS usual care")) %>%
    dplyr::select(group, dplyr::all_of(pre), dplyr::all_of(post)) %>%
    tidyr::drop_na()
  
  m  <- stats::lm(stats::reformulate(c("group", pre), response = post), data = df)
  r  <- stats::residuals(m)
  
  tibble::as_tibble(data.frame(
    variable = rep(var, length(r)),
    model    = "ANCOVA",
    residual = r
  ))
}

# Change score ujustert
get_residuals_change <- function(var){
  
  delta <- paste0("delta_", var)
  
  df <- df_full_v2 %>%
    dplyr::filter(group %in% c("BCS exercise","Non-cancer controls")) %>%
    dplyr::select(group, dplyr::all_of(delta)) %>%
    tidyr::drop_na()
  
  m <- stats::lm(stats::reformulate("group", response = delta), data = df)
  r <- stats::residuals(m)
  
  tibble::as_tibble(data.frame(
    variable = rep(var, length(r)),
    model    = "Change",
    residual = r
  ))
}

# Samle alle residualer
df_residuals <- bind_rows(
  map_dfr(all_vars,   get_residuals_ancova),
  map_dfr(delta_vars, get_residuals_change)
) %>%
  mutate(
    domain = case_when(
      variable %in% cardio_vars  ~ "Cardiorespiratory",
      variable %in% muscle_vars  ~ "Muscle",
      variable %in% blood_vars   ~ "Blood",
      TRUE                       ~ "Other"
    )
  )

# ── QQ-plot funksjon per variabel ─────────────────────────────────────────────

plot_qq <- function(var, model_type) {
  df_plot <- df_residuals %>%
    filter(variable == var, model == model_type)
  
  ggplot(df_plot, aes(sample = residual)) +
    stat_qq(colour = "#4A90B8", size = 1.5, alpha = 0.8) +
    stat_qq_line(colour = "grey60", linewidth = 0.5) +
    labs(
      title = paste0(var, " (", model_type, ")"),
      x     = "Theoretical quantiles",
      y     = "Sample quantiles"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(size = 9, face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# ── Lag og lagre QQ-plots per domene ─────────────────────────────────────────

save_qq_panel <- function(domain_name, model_type, vars) {
  
  # Filtrer til variabler som faktisk finnes i residualdata
  available_vars <- df_residuals %>%
    filter(domain == domain_name, model == model_type) %>%
    pull(variable) %>%
    unique()
  
  vars_to_plot <- intersect(vars, available_vars)
  
  plots <- map(vars_to_plot, ~ plot_qq(.x, model_type))
  
  # Sett sammen med patchwork
  n      <- length(plots)
  ncols  <- 4
  nrows  <- ceiling(n / ncols)
  
  combined <- wrap_plots(plots, ncol = ncols) +
    plot_annotation(
      title    = paste0("QQ-plots: ", domain_name, " (", model_type, ")"),
      theme    = theme(plot.title = element_text(size = 13, face = "bold"))
    )
  
  filename <- paste0("qq_", tolower(domain_name), "_", tolower(model_type), ".png")
  
  ggsave(
    filename = file.path("output", filename),
    plot     = combined,
    width    = ncols * 3.5,
    height   = nrows * 3.5,
    dpi      = 150
  )
  
  message("Lagret: ", filename)
  combined
}

# Lag output-mappe hvis den ikke finnes
dir.create("output", showWarnings = FALSE)

# ── Kjør og lagre alle kombinasjoner ─────────────────────────────────────────

qq_cardio_ancova  <- save_qq_panel("Cardiorespiratory", "ANCOVA",  cardio_vars)
qq_cardio_change  <- save_qq_panel("Cardiorespiratory", "Change",  cardio_vars)
qq_muscle_ancova  <- save_qq_panel("Muscle",            "ANCOVA",  muscle_vars)
qq_muscle_change  <- save_qq_panel("Muscle",            "Change",  muscle_vars)
qq_blood_ancova   <- save_qq_panel("Blood",             "ANCOVA",  blood_vars)
qq_blood_change   <- save_qq_panel("Blood",             "Change",  blood_vars)

# ── Vis i R direkte ───────────────────────────────────────────────────────────
# Bytt ut variabelnavn og modell for å inspisere enkeltvariabler
plot_qq("vo2mlkgmin", "ANCOVA")
plot_qq("cs",         "Change")
plot_qq("hadh",       "Change")



plot_domain_qq <- function(domain_name, model_type){
  
  df_plot <- df_residuals %>%
    filter(domain == domain_name, model == model_type)
  
  ggplot(df_plot, aes(sample = residual)) +
    stat_qq(colour = "#4A90B8", size = 1.2, alpha = 0.8) +
    stat_qq_line(colour = "grey60", linewidth = 0.5) +
    facet_wrap(~variable, scales = "free", ncol = 4) +
    labs(
      title = paste0("QQ-plots: ", domain_name, " (", model_type, ")"),
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 9, face = "bold"),
      panel.grid.minor = element_blank()
    )
}

domains <- unique(df_residuals$domain)

plots <- expand.grid(
  domain = domains,
  model  = c("ANCOVA","Change")
) %>%
  mutate(plot = purrr::map2(domain, model, plot_domain_qq))

purrr::walk2(
  plots$plot,
  paste0("output/qq_", tolower(plots$domain), "_", tolower(plots$model), ".png"),
  ~ggsave(.y, .x, width = 12, height = 8, dpi = 150)
)


plots %>% filter(domain == "Muscle", model == "ANCOVA") %>% pull(plot) %>% .[[1]]

plot_domain_qq("Cardiorespiratory","ANCOVA")
plot_domain_qq("Muscle","ANCOVA")
plot_domain_qq("Blood","ANCOVA")
