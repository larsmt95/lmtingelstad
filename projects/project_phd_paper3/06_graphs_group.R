library(readxl)
library(tidyverse)
library(cowplot)
library(dplyr)
library(scales)
library("viridis")
library(patchwork)
library(ggh4x)


## --- DATAFORBEREDELSE ----
# Beregn absolutte og prosentvise endringer mellom runder

mean_group_age_yearly <- data.frame()
unique_tests <- unique(paper3_longer_vol2$test)

# Loop over each `test` category
for (test_name in unique_tests) {
  temp_result <- paper3_longer_vol2 %>%
    filter(test == test_name) %>%
    mutate(time = factor(time, levels = c("round1","round2","round3"))) %>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(
      .t = as.integer(time),
      .t_lag = lag(.t),
      .v_lag = lag(value),
      pct_change = if_else(.t - .t_lag == 1, (value/.v_lag - 1) * 100, NA_real_),
      abs_change = if_else(.t - .t_lag == 1, value - .v_lag, NA_real_)
    ) %>%
    ungroup() %>%
    select(-.t, -.t_lag, -.v_lag)
  
  mean_group_age_yearly <- bind_rows(mean_group_age_yearly, temp_result)
}

# Endring mellom T1 og T3
mean_group_age_graph_t1_3 <- data.frame()
unique_tests <- unique(paper3_longer_vol2$test)

# Loop over each `test` category
for (test_name in unique_tests) {
  # Filter for the current test and IDs with all time points
  
  temp_result <- paper3_longer_vol2 %>%
    filter(test == test_name) %>%
    # group_by(id) %>%
    # filter(all(c("round1", "round2", "round3") %in% time)) %>%
    # ungroup() %>%
    filter(time != "round2") %>%
    group_by(id) %>% 
    arrange(id, time) %>% 
    mutate(pct_change = (value/lag(value) - 1) * 100) %>% 
    mutate(abs_change = value - lag(value)) %>% 
    ungroup()
  
  
  # Save the result to the list
  mean_group_age_graph_t1_3 <- bind_rows(mean_group_age_graph_t1_3, temp_result)
}


# --- GJENNOMSNITT PER GRUPPE/ÅR ----
mean_group_age_yearly <- mean_group_age_yearly %>% 
  #group_by(id) %>%
  #filter(all(c("round1", "round2", "round3") %in% time)) %>%
  #ungroup() %>%
  filter(time != "round1") %>% 
  group_by(group, time, test, id) %>% 
  summarise(mean = mean(abs_change, na.rm = TRUE),
            sd = sd(abs_change, na.rm = TRUE),
            n = n()) %>% 
  print()

# DIFF TID 1-3
mean_group_age_graph_t1_3 <- mean_group_age_graph_t1_3  %>% 
  # group_by(id) %>%
  # filter(all(c("round1", "round2", "round3") %in% time)) %>%
  # ungroup() %>%
  filter(time == "round3") %>% 
  group_by(group, time, test, id) %>% 
  summarise(mean = mean(abs_change, na.rm = TRUE),
            sd = sd(abs_change, na.rm = TRUE),
            n = n()) %>% 
  mutate(time = if_else(time == "round3", "round1", time)) %>% 
  print() 

# Kombiner datasett
mean_group_age_graph_combined <- bind_rows(mean_group_age_yearly, mean_group_age_graph_t1_3)

# Pivot til bredt format
mean_group_age_graph_test <- mean_group_age_graph_combined %>% 
  select(group, time, id, n, test, mean) %>% 
  pivot_wider(names_from = test, 
              values_from = mean, 
              values_fn = mean) %>% 
  left_join(paper3 %>% select(id, time, sex, ageg), by = c("id", "time")) %>% 
  filter(!is.na(group), !is.na(sex), ageg != "U18") %>% 
  rename(Age = ageg) %>% 
  rename_with(~ gsub("^mean_", "", .x))

# Reorder time-faktor
mean_group_age_graph_test %>% select(id, cmj) %>% arrange(desc(cmj))

# Reorder the 'time' factor so T1-3 is last
mean_group_age_graph_test$time <- factor(mean_group_age_graph_test$time, 
                                         levels = c("round2", "round3", "round1", "boys", "girls"))

mean_group_age_graph_test %>% 
  filter(group == "boysU14", time == "round3") %>% 
  select(id, sprint30) %>% 
  arrange(sprint30) %>% 
  print(n = Inf)



## --- FUNKSJON FOR PLOTT ----
perform_analysis_and_plot <- function(mean_group_age_graph_test, dependent_var, custom_colors, custom_theme, 
                                      label_format = "%.1f", ylim_min = 0, ylim_max = NULL, 
                                      show.legend = FALSE, y_axis_title = NULL, plot_title = "", 
                                      legend_position = "bottom", show_x_axis = FALSE, show_strip_text = FALSE) {
  
  # Standard farger og former
  custom_colors <- c("boysU14" = "skyblue2", 
                     "boysU16" = "skyblue4", 
                     "girlsU14" = "#FF6F6F", 
                     "girlsU16" = "maroon")
  
  shape_values <- c(
    "boysU14" = 16,  # sirkel
    "girlsU14" = 16, # sirkel
    "boysU16" = 17,  # trekant
    "girlsU16" = 17  # trekant
  )
  
  # Labels for legend
  group_labels <- c(
    "boysU14" = "Boys U14",
    "boysU16" = "Boys U16",
    "girlsU14" = "Girls U14",
    "girlsU16" = "Girls U16"
  )
  
  labeller_group <- as_labeller(c(
    "boys" = "Boys",
    "girls" = "Girls",
    "boysU14" = "Boys U14",
    "girlsU14" = "Girls U14",
    "boysU16" = "Boys U16",
    "girlsU16" = "Girls U16"
  ))

  
alpha <- ifelse(mean_group_age_graph_test$Age == "U14", 0.5, 0.8)
  
p <- ggplot(mean_group_age_graph_test, aes(time, y = .data[[dependent_var]], color = group, group = sex, shape = group)) +
    geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dashed", alpha = 1) +
    geom_jitter(aes(color = group), width = 0.2, size = 2, alpha = alpha) +
    facet_wrap(~sex, nrow = 1, scales = "free_x", strip.position = "bottom", labeller = labeller_group) +
    scale_shape_manual(values = shape_values, labels = group_labels) +
    scale_color_manual(values = custom_colors, labels = group_labels) +
  scale_x_discrete(labels = c(
    "boys" = "Boys", 
    "girls" = "Girls",
    "round2" = "T1-2",
    "round3" = "T2-3",
    "round1" = "T1-3",
    "group" = "Group",
    "boysU14" = "Boys U14", 
    "girlsU14" = "Girls U14",
    "boysU16" = "Boys U16", 
    "girlsU16" = "Girls U16"
  )) +
  labs(y = y_axis_title, title = plot_title, color = "Group", shape = "Group") +
  theme(
      panel.background = element_blank(),
      strip.background = element_blank(),
      strip.text.x = if (show_strip_text) element_text(size = 13, color = "black") else element_blank(),  # Hide strip text by default
      strip.placement = "outside",
      axis.line.x = element_line(color = "black", size = 0.5),
      axis.line.y = element_line(color = "black", size = 0.5),
      axis.text.x = if (show_x_axis) element_text(size = 12, color = "black") else element_blank(),  # Hide x-axis labels by default
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 16),
      axis.title.x = element_blank(),
      legend.position = "none",  # Remove individual legends
      legend.text = element_text(size = 14)
    )
  
  
  if(!is.null(ylim_max)) {
    p <- p + coord_cartesian(ylim = c(ylim_min, ylim_max))
  } else {
    p <- p + coord_cartesian(ylim = c(ylim_min, Inf))
  }
  
p <- p + geom_text(
    data = mean_group_age_graph_test %>%
      filter(!is.na(.data[[dependent_var]])) %>%  # Bruker den dynamiske dependent_var kolonnen
      group_by(sex, time) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(x = case_when(
        time == "round1" ~ 3,
        time == "round2" ~ 1,
        time == "round3" ~ 2
      )),
    aes(x = x, y = ylim_max, label = paste0("n=", n)),
    inherit.aes = FALSE,
    hjust = 0.5, size = 4, color = "black"
  )
  
  
  return(p)
  
}

# --- Variabler per gruppe ---
vars_hurtighet <- c("cmj", "sprint30", "cod", "yyir")
# vars_hurtighet_2 <- c("cod", "yyir", "totpwr", "relforce", "relpwr")
vars_styrke <- c("totpwr", "relforce", "relpwr")
vars_antropometri <- c("height", "bodymass")
vars_skade <- c("relhams", "meanadd", "reladd", "meanabd", "relabd")
# vars_combined <- c("cmj", "sprint30", "totforce", "meanhams", "height", "bodymass")

# # --- Y-akse titler ---
y_titles_hurtighet <- c("CMJ (cm)", "CoD (s)", "YYIR1 (m)", "30 m sprint (s)")
# y_titles_hurtighet <- c("CoD (s)", "YYIR1 (m)", "Total power (W)", "Relative force (N/kg)", "Relative power (W/kg)")
y_titles_styrke <- c("Total force (N)", "Total power (W)", "Relative force (N/kg)", "Relative power (W/kg)")
y_titles_antropometri <- c("Height (cm)", "Body mass (kg)")
y_titles_skade <- c("Relative hamstring (N/kg)",
                    "Hip adduction (N)", "Relative hip ADD (Nm/kg)",
                    "Hip abduction (N)", "Relative hip ABD (Nm/kg)")
# y_titles_combined <- c("CMJ (cm)", "30 m sprint (s)",
#                        "Total force (N)", "Eccentric hamstring (N)",
#                        "Height (cm)", "Body mass (kg)")
 

# # --- Bokstaver ---
titles_hurtighet <- c("A", "B", "C", "D")
# titles_hurtighet <- c("A", "B", "C", "D", "E")
titles_styrke <- c("A", "B", "C", "D")
titles_antropometri <- c("A", "B")
titles_skade <- c("A", "B", "C", "D", "E")
titles_combined <- c("A", "B", "C", "D", "E", "F")

# --- Funksjon for å lage plots med lister ---
create_plots <- function(vars, y_titles, plot_titles, ylim_list) {
  mapply(function(var, y_title, p_title) {
    perform_analysis_and_plot(
      mean_group_age_graph_test,
      dependent_var = var,
      ylim_min = ylim_list[[var]][1],
      ylim_max = ylim_list[[var]][2],
      y_axis_title = y_title,
      plot_title = p_title,
      show_x_axis = TRUE,
      show_strip_text = TRUE,
      show.legend = TRUE
    )
  }, vars, y_titles, plot_titles, SIMPLIFY = FALSE)
}

ylim_settings <- list(
  cmj = c(-9, 30),
  cod = c(-2.5, 1),
  yyir = c(-525, 1100),
  sprint30 = c(-1.02, 1),
  totforce = c(-695, 1600),
  totpwr = c(-270, 930),
  relforce = c(-15, 22),
  relpwr = c(-6, 11),
  height = c(0, 25),
  bodymass = c(0, 25),
  meanhams = c(-85, 180),
  relhams = c(-2.5, 2.9),
  meanadd = c(-100, 150),
  reladd = c(-1.2, 1.5),
  meanabd = c(-70, 110),
  relabd = c(-0.9, 1.5)
)

library(gridGraphics)


# --- Lag alle plots ---
plots_hurtighet <- create_plots(vars_hurtighet, y_titles_hurtighet, titles_hurtighet, ylim_settings)
plots_styrke <- create_plots(vars_styrke, y_titles_styrke, titles_styrke, ylim_settings)
plots_antropometri <- create_plots(vars_antropometri, y_titles_antropometri, titles_antropometri, ylim_settings)
plots_skade <- create_plots(vars_skade, y_titles_skade, titles_skade, ylim_settings)
# plots_combined <- create_plots(vars_combined, y_titles_combined, titles_combined, ylim_settings)

# --- Kombiner plots i grid ---
hurtighet_grid <- plot_grid(plotlist = plots_hurtighet, nrow = 3, align = "v")
styrke_grid <- plot_grid(plotlist = plots_styrke, nrow = 2, align = "v")
antropometri_grid <- plot_grid(plotlist = plots_antropometri, nrow = 1, align = "v")
skade_grid <- plot_grid(plotlist = plots_skade, nrow = 3, align = "v")
# combined_grid <- plot_grid(plotlist = plots_combined, nrow, 3, align = "v")

# --- Vis plots ---
hurtighet_grid
styrke_grid
antropometri_grid
skade_grid

# Ekstraher legenden
legend_plot <- ggplot() +
  theme_void() +
  guides(color = guide_legend(override.aes = list(alpha = 1)))# Kombiner plott til et grid og samle legendene

combined_plot <- wrap_plots(plots_skade, ncol = 2) +
  plot_layout(guides = 'collect') +  # Samle alle legendene
  theme(legend.position = "right")  # Plasser legenden til venstre

# Juster margene etter behov
combined_plot <- combined_plot +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))



combined_plot

# ggsave("combined_plot.svg", plot = combined_plot, width = 12, height = 5, dpi = 300)


