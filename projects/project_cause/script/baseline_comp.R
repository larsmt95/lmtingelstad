library(dplyr)
library(gt)
library(gtsummary)
library(tableone)


# sjekke for baseline forskjeller mellom grupper

names(df_full_v2)

df_full_v2 %>%
  group_by(group) %>%
  summarise(
    age_mean = mean(alder_pre, na.rm = TRUE),
    age_sd = sd(alder_pre, na.rm = TRUE),
    bmi_mean = mean(bmi_pre, na.rm = TRUE),
    bmi_sd = sd(bmi_pre, na.rm = TRUE)
  )

anova <- aov(pre_tot_lean ~ factor(group), data = df_full)
summary(anova)

# gjøre group til faktor 
df_full_test <- df_full_v2 %>%
  mutate(
    group = factor(
      group,
      levels = c(0,1,2),
      labels = c(
        "Non-cancer controls",
        "BCS exercise",
        "BCS usual care"
      )
    )
  )


table1 <- df_full_test %>%
  select(
    group,
    alder_pre,
    bmi_pre,
    pre_vo2mlkgmin,
    pre_hfmaks,
    pre_tot_lean,
    pre_legs_lean,
    pre_hb
  ) %>%
  tbl_summary(
    by = group,
    statistic = all_continuous() ~ "{mean} ± {sd}"
  )

# lage tabell 1
library(tableone)

vars <- c(
  "alder_pre",
  "bmi_pre",
  "pre_vo2mlkgmin",
  "pre_hfmaks",
  "pre_tot_lean",
  "pre_legs_lean",
  "pre_hb"
)

deskriptive_table <- c(
  "alder_pre","hoyde_pre","vekt_pre","bmi_pre"
)

table_muscle <- CreateTableOne(
  vars = muscle,
  strata = "group",
  data = df_full_test
)

print(table_muscle,
      test = FALSE,
      quote = FALSE,
      noSpaces = TRUE
      )
