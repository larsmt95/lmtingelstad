# Libraries ----
library(readxl)
library(writexl)
library(tidyverse)
library(lme4)
library(boot.pval)
library(lmerTest)
library(openxlsx)
library(dplyr)
library(tidyr)
library(emmeans)
library(cowplot)
library(broom)
library(ggpmisc)
library(patchwork)


# ---- Utregning av prosent ----

paper3_longer_vol2$time <- relevel(paper3_longer_vol2$time, ref = "round1")

percent_age2 <- paper3_longer_vol2 %>%
  group_by(test, id) %>%
  filter(n_distinct(time) >= 2, time != "round2") %>%
  arrange(id, time) %>%
  mutate(
    pct_change = (value / lag(value) - 1) * 100,
    abs_change = value - lag(value)
  ) %>%
  ungroup()

# ---- PIVOT WIDER ----
percent_age2_var <- percent_age2 %>% 
  pivot_wider(
    names_from = test,
    values_from = c(value, abs_change, pct_change),
    values_fn = mean
  ) %>% 
  rename_with(
    .fn = ~ gsub("^abs_change_", "", .), # Remove the prefix "pct_change_"
    .cols = starts_with("abs_change_")  # Select columns that start with "pct_change_"
  )  

# ---- Legg til phv_base ----
phv_round1 <- percent_age2_var %>%
  filter(time != "round3") %>%
  select(id, value_phv) %>%
  rename(phv_round1 = value_phv)

percent_age2_with_phv <- percent_age2_var %>%
  left_join(phv_round1, by = "id") %>%
  filter(time != "round1") %>%
  rename(phv_base = phv_round1)

percent_age2_with_phv <- percent_age2_with_phv %>% filter(id != 601)
percent_age2_with_phv %>% select(id, height) %>% arrange(height)

# Funksjon for C% lage plott med felles tema
create_plot <- function(percent_age2_with_phv, y_var, y_label, title = NULL, show_x_title = FALSE) {
  
  model <- lm(as.formula(paste(y_var, "~ phv_base * sex")), data = percent_age2_with_phv)
  
  model_p_value <- summary(model)$fstatistic
  model_p_value <- pf(model_p_value[1], model_p_value[2], model_p_value[3], lower.tail = FALSE)
  r_squared <- summary(model)$r.squared
  
  p_value_formatted <- formatC(model_p_value, format = "f", digits = 3)
  
  plot <- percent_age2_with_phv %>% 
    ggplot(aes(x = phv_base, y = !!sym(y_var), color = sex, fill = sex)) +
    geom_hline(yintercept = 0, color = "grey", size = 0.5, linetype = "dashed", alpha = 1) +
    # geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf), fill = "grey90", alpha = 0.03, color = NA) +
    geom_point(alpha = 0.4, size = 1.5) +
    scale_color_manual(values = c("boys" = "skyblue", "girls" = "maroon")) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
    stat_poly_eq(
      aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
      formula = y ~ x,
      parse = TRUE,
      size = 4,
      label.x = "right",
      label.y = c(1.15, 0.85)   # litt vertikal separasjon
    ) +
    scale_fill_manual(values = c("boys" = "lightblue", "girls" = "pink")) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.5),
      axis.line.y = element_line(color = "black", size = 0.5),
      axis.text.x = element_text(size = 12, color = "black"),  # Hide x-axis labels by default
      strip.text.x = element_text(size = 13, color = "black"),  # Hide strip text by default
      axis.title.y = element_text(size = 16),
      axis.title.x = if (show_x_title) element_text(size = 12, color = "black") else element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.text = element_text(size = 14),      # p9 Cker tekststC8rrelse pC% legend
      legend.title = element_text(size = 14)  # p9 Cker tittel pC% legend
    ) +
    labs(x = "YPHV (y)", y = y_label, title = title)
  
}

# --- Lag plots med y-titler og bokstav ----
plots_list1 <- list(
  sprint_plot = create_plot(percent_age2_with_phv, "sprint30", "30 m sprint (s)", "A"),
  cmj_plot = create_plot(percent_age2_with_phv, "cmj", "CMJ (cm)", "B"),
  force_plot = create_plot(percent_age2_with_phv, "totforce", "Total force (N)", "C", show_x_title = TRUE),
  cod_plot = create_plot(percent_age2_with_phv, "cod", "CoD (s)", "D", show_x_title = TRUE)
)

plots_list2 <- list(
  bodymass_plot = create_plot(percent_age2_with_phv, "bodymass", "Body mass (kg)", "A", show_x_title = TRUE),
  height_plot = create_plot(percent_age2_with_phv, "height", "Height (cm)", "B", show_x_title = TRUE)
)

# --- Kombiner plots i grid med samlet legend ----
combined_plot_2 <- wrap_plots(plots_list2, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(plot.margin = unit(c(1,1,1,1), "lines"))

# --- Vis plot ----
combined_plot_1
combined_plot_2

# # --- Lagre som SVG ----
# library(svglite)
#  ggsave("regression_none3.svg", plot = combined_plot, device = "svg", width = 8, height = 10)


sprint_plot = create_plot(percent_age2_with_phv, "sprint30", "30 m sprint (s)", "A", show_x_title = TRUE)
cmj_plot = create_plot(percent_age2_with_phv, "cmj", "CMJ (cm)", "B", show_x_title = TRUE)
force_plot = create_plot(percent_age2_with_phv, "totforce", "Total force (N)", "C", show_x_title = TRUE)
height_plot = create_plot(percent_age2_with_phv, "height", "Height (cm)", show_x_title = TRUE)
yyir_plot = create_plot(percent_age2_with_phv, "yyir", "YYIR1 (m)", show_x_title = TRUE)
cod_plot = create_plot(percent_age2_with_phv, "cod", "CoD (s)", show_x_title = TRUE)

rel_force_plot = create_plot(percent_age2_with_phv, "relforce", "Total force (N)", "C", show_x_title = TRUE)


yyir_plot
sprint_plot
cmj_plot
force_plot
cod_plot
bodymass_plot
height_plot

combined_plot
ggsave("MATURATION_sprint_2.svg", plot = sprint_plot, width = 6, height = 5, dpi = 300)
ggsave("MATURATION_force.svg", plot = force_plot, width = 6, height = 5, dpi = 300)
ggsave("MATURATION_smj.svg", plot = cmj_plot, width = 6, height = 5, dpi = 300)

ggsave("combined_plot_final.svg", plot = combined_plot, width = 8, height = 10, dpi = 300)

ggsave("combined_plot_1_yeet.svg", plot = combined_plot_1, width = 8, height = 8, dpi = 300)
ggsave("combined_plot_2_yeet.svg", plot = combined_plot_2, width = 8, height = 6, dpi = 300)


