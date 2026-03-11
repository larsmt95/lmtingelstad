library(tidyverse)
library(DBI)
library(RPostgres)

## KOBLE TIL DATABASE I SQL 

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "e-commerce",
  host = "localhost",
  port = 5432,
  user     = "postgres",
  password = "legandary007"
)

## HENTE N??DVENDIG DATA FRA SQL
query <- "
SELECT 
    low_rating_flag,
    delay_flag,
    delay_days,
    total_order_value,
    avg_review_score
FROM public.fact_orders_v5
"

df <- dbGetQuery(con, query)


query_mr <- "
SELECT *
FROM public.montly_revenue
"

df_mr <- dbGetQuery(con, query_mr)

query_drm <- "
SELECT *
FROM public.delay_rate_montly
"

df_drm <- dbGetQuery(con, query_drm)

query_aov <- "
SELECT *
FROM public.monthly_aov
"

aov <- dbGetQuery(con, query_aov)

query_mkpis <- "
SELECT *
FROM public.monthly_kpis
"

monthly_kpis <- dbGetQuery(con, query_mkpis)

## Sesongm??nster

  ## AOV
ggplot(aov, aes(x = year_month, y = aov)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Year-Month",
    y = "Average Order Value"
  ) +
  theme_minimal()
  
  ## Revenue over tid 
ggplot(df_mr, aes(x = year_month, y = total_revenue)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Year-Month",
    y = "Revenue"
  ) +
  theme_minimal()


  ## Delay rate vs season
df_drm %>% 
  filter (delay_rate < 1) %>% 
ggplot(aes(x = year_month, y = delay_rate)) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year-Month",
    y = "Delay Rate"
  ) +
  theme_minimal()

    ## low rating over tid
monthly_kpis %>% 
  filter(low_rating_rate < 1) %>% 
ggplot(aes(x = year_month, y = low_rating_rate)) +
  geom_line(linewidth = 1.2) +
  theme_minimal()

## Trend over tid 

tot <- lm(total_revenue ~ year_month, data = df_mr)
summary(tot)  
