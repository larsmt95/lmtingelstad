library(tidyverse)
library(dplyr)
library(readxl)
library(janitor) # rydde variabelnavn
library(skimr) # rask datasjekk
library(naniar) # sjekke missing data
library(ggplot2)
library(GGally)

#df_full
#df_main

glimpse(df_main)
summary(df_main)
skim(df_main)
vis_miss(df_main) # sjekke missing
df_main %>% 
  count(id) %>% 
  filter(n > 1)

names(df_full_v2)
sapply(df_full, class)


# sjekke datadistribusjon

## alle 

GGally::ggpairs(
  df_full %>%
    select(cs_pre, hadh_pre, caf_total_pre, csa_total_pre)
)


  ## funksjon for å plotte distribusjoner
plot_distribution <- function(data, vars, title){
  
  data %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "value") %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15, fill = "steelblue", alpha = 0.7) +
    facet_wrap(~variable, scales = "free") +
    theme_minimal() +
    labs(title = title)
}

    # Deskriptive
plot_distribution(df_full, deskriptive, "Descriptive variables")

    # Muskelvariabler
plot_distribution(df_full, muscle, "Muscle variables")

    # Kardiorespiratoriske
plot_distribution(df_full, cardio, "Cardiorespiratory variables")

    # Hjerte
plot_distribution(df_full, heart, "Stress echo variables")

    # Lunge
plot_distribution(df_full, lung, "Pulmonary variables")

    # Blood
plot_distribution(df_full, blood, "Blood variables")

    # Kroppssammensetning
plot_distribution(df_full, bodycomp, "Body composition variables")

  ## qq-plot for alle 
plot_qq <- function(data, vars, title){
  
  data %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(),
                 names_to = "variable",
                 values_to = "value") %>%
    ggplot(aes(sample = value)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~variable, scales = "free") +
    theme_minimal() +
    labs(title = title)
}

plot_qq(df_full_v2, muscle, "Muscle variables")
  ## Mulig behov for log-trans: cs, hahd, csa_total, csa_mhc2
plot_qq(df_full_v2, cardio, "Cardioresiratory variables")



# Se på distribusjonen direkte
caf_ex <- df_full_v2 %>% 
  filter(group == "BCS exercise") %>% 
  pull(caf_total_pre)

# Grunnleggende statistikk
summary(caf_ex)
hist(caf_ex, breaks = 10)

# Er det utliggere?
boxplot(caf_ex)

# Shapiro på begge
shapiro.test(caf_ex)
shapiro.test(df_full_v2 %>% filter(group == "BCS usual care") %>% pull(caf_total_pre))
