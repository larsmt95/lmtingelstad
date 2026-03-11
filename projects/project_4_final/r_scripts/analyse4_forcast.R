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

query_mkpis <- "
SELECT *
FROM public.monthly_kpis
"

monthly_kpis <- dbGetQuery(con, query_mkpis)


## klargj??re data

monthly_kpis$time_index <- 1:nrow(monthly_kpis)
monthly_kpis$month <- as.factor(format(monthly_kpis$year_month, "%m"))

    # time_index -> trend
    # month -> sesong

## Bygg baseline-modell

  ## Kun line??r trend

model_trend <- lm(total_revenue ~ time_index,
                  data = monthly_kpis) # antar stabil vekst uten sesong


  ## Trend + sesong (anbefalt)

model_trend_season <- lm(total_revenue ~ time_index + month,
                         data = monthly_kpis)

summary(model_trend_season)
    
# line??r vekst
    # faste m??nedseffekter 

## Lag fremtidige m??neder

h <- 12 # forecast horizon

future_data <- data.frame(
  time_index = (max(monthly_kpis$time_index) + 1):
               (max(monthly_kpis$time_index) + 12)
)

future_dates <- seq(max(monthly_kpis$year_month) + 31,
                    by = "month",
                    length.out = h)

future_data$month <- as.factor(format(future_dates, "%m"))
future_data$year_month <- future_dates

## Generer forecast
future_data$forecast <- predict(model_trend_season,
                                newdata = future_data)


?predict 

ggplot() +
  geom_line(data = monthly_kpis,
            aes(x = year_month, y = total_revenue)) +
  geom_line(data = future_data,
            aes(x = year_month, y = forecast),
            linetype = "dashed")+
  theme_minimal()


## Valider modellen

  ## Residualer
plot(model_trend_season$residuals)

  ## R-squared
summary(model_trend_season)

  ## Uten sesong
model_trend <- lm(total_revenue ~ time_index, data = monthly_kpis)
AIC(model_trend, model_trend_season)


## Forecast med intervall

future_pred <- predict(
  model_trend_season,
  newdata = future_data,
  interval = "prediction",
  level = 0.95
)

future_data$forecast <- future_pred[, "fit"]
future_data$lower <- future_pred[, "lwr"]
future_data$upper <- future_pred[, "upr"]

library(ggplot2)

ggplot() +
  geom_line(data = monthly_kpis,
            aes(x = year_month, y = total_revenue)) +
  
  geom_line(data = future_data,
            aes(x = year_month, y = forecast),
            linetype = "dashed", linewidth = 1.2) +
  
  geom_ribbon(data = future_data,
              aes(x = year_month,
                  ymin = lower,
                  ymax = upper),
              alpha = 0.15) +
  
  theme_minimal()



# Baseline forecast indikerer fortsatt stabil m??nedlig vekst p?? omtrent 55 000 per m??ned. Sesongeffekter er svake, med noe indikasjon p?? h??yere aktivitet i november.
# Baseline forecast was constructed using a linear time trend with fixed monthly seasonal effects. More advanced stochastic time-series models were not applied at this stage, as the objective was to establish a transparent reference projection.
