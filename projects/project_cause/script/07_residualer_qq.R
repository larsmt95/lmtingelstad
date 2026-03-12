# =============================================================================
# Residual diagnostics – QQ plots
# Used to assess normality assumptions of linear models
# =============================================================================

# ─────────────────────────────────────────────────────────────
# 1. PACKAGES
# ─────────────────────────────────────────────────────────────

library(dplyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(tidyr)


# ─────────────────────────────────────────────────────────────
# 2. MODEL RESIDUALS
# ─────────────────────────────────────────────────────────────

get_residuals <- function(var, model_type){
  
  if(model_type == "ANCOVA"){
    
    pre  <- paste0(var,"_pre")
    post <- paste0(var,"_post")
    
    df <- df_full_v2 %>%
      filter(group %in% c("BCS exercise","BCS usual care")) %>%
      select(group, all_of(pre), all_of(post)) %>%
      drop_na()
    
    model <- lm(reformulate(c("group",pre), response = post), data = df)
    
  }
  
  if(model_type == "Change"){
    
    delta <- paste0("delta_",var)
    
    df <- df_full_v2 %>%
      filter(group %in% c("BCS exercise","Non-cancer controls")) %>%
      select(group, all_of(delta)) %>%
      drop_na()
    
    model <- lm(reformulate("group", response = delta), data = df)
    
  }
  
  tibble(
    variable = var,
    model    = model_type,
    residual = residuals(model)
  )
  
}


# ─────────────────────────────────────────────────────────────
# 3. COLLECT RESIDUALS
# ─────────────────────────────────────────────────────────────

df_residuals <- bind_rows(
  
  map_dfr(all_vars,   get_residuals, model_type = "ANCOVA"),
  map_dfr(delta_vars, get_residuals, model_type = "Change")
  
) %>%
  mutate(
    domain = case_when(
      variable %in% cardio_vars ~ "Cardiorespiratory",
      variable %in% muscle_vars ~ "Muscle",
      variable %in% blood_vars  ~ "Blood",
      TRUE ~ "Other"
    )
  )


# ─────────────────────────────────────────────────────────────
# 4. QQ PLOT FUNCTION
# ─────────────────────────────────────────────────────────────

plot_qq <- function(df){
  
  ggplot(df, aes(sample = residual)) +
    stat_qq(colour = "#4A90B8", size = 1.2, alpha = 0.8) +
    stat_qq_line(colour = "grey60", linewidth = 0.5) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 9, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
}


# ─────────────────────────────────────────────────────────────
# 5. DOMAIN PANEL PLOTS
# ─────────────────────────────────────────────────────────────

plot_domain_qq <- function(domain_name, model_type){
  
  df_plot <- df_residuals %>%
    filter(domain == domain_name,
           model  == model_type)
  
  p <- plot_qq(df_plot) +
    facet_wrap(~variable, scales = "free", ncol = 4) +
    labs(
      title = paste0(
        "Residual QQ plots: ",
        domain_name,
        " (",model_type,")"
      ),
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    )
  
  p
  
}


# ─────────────────────────────────────────────────────────────
# 6. SAVE ALL QQ PANELS
# ─────────────────────────────────────────────────────────────

dir.create("output", showWarnings = FALSE)

domains <- unique(df_residuals$domain)

plots <- expand.grid(
  domain = domains,
  model  = c("ANCOVA","Change")
)

walk2(
  plots$domain,
  plots$model,
  function(d,m){
    
    p <- plot_domain_qq(d,m)
    
    filename <- paste0(
      "output/qq_",
      tolower(d),"_",
      tolower(m),
      ".png"
    )
    
    ggsave(
      filename,
      p,
      width  = 12,
      height = 8,
      dpi    = 150
    )
    
    message("Saved: ",filename)
    
  }
)


# ─────────────────────────────────────────────────────────────
# 7. OPTIONAL: INSPECT INDIVIDUAL VARIABLES
# ─────────────────────────────────────────────────────────────

df_residuals %>%
  filter(variable == "vo2mlkgmin",
         model == "ANCOVA") %>%
  plot_qq()

df_residuals %>%
  filter(variable == "cs",
         model == "Change") %>%
  plot_qq()