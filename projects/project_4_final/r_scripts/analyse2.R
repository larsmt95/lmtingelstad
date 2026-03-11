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

## HENTE NCDVENDIG DATA FRA SQL
query <- "
SELECT 
    order_purchase_timestamp
    low_rating_flag,
    delay_flag,
    delay_days,
    total_order_value,
    avg_review_score
FROM public.fact_orders_v5
"

df <- dbGetQuery(con, query)


## - Delay severity ??? Rating

    ## gir flere forsinkelsesdager h??yere risiko for d??rlig rating?

model_delay <- glm(low_rating_flag ~ delay_days + total_order_value,
                   data = df,
                   family = binomial())

summary(model_delay)
exp(coef(model_delay))

      # Koeffisient: 0.0735
      # Odds ratio:  1.076
      
      # Det betyr: Hver ekstra forsinkelsesdag ??ker odds for lav rating med 7.6 %.
        # Dette er en dose-respons-effekt.
        # Mye mer troverdig enn OR = 15.

  ## effekt i prosentpoeng

new_data <- data.frame(
  delay_days = c(0, 5, 10),
  total_order_value = mean(df$total_order_value, na.rm = TRUE)
)

predict(model_delay, new_data, type = "response")

      # Each additional day of delay increases the odds of a low rating by 7.6 %. A 10-day delay more than doubles the odds. Holding order value constant, predicted probability increases substantially as delay length grows.

  ## b??yer effekten av?
  
    # legger til kvadratledd (Kvadratleddet lar effekten b??ye seg/kurve i stedet for ?? v??re rett)
    # effekten kan: ??ke raskt og flate ut | ??ke sakte f??rst og s?? eksplodere | ha terskel
model_quad <- glm(
    low_rating_flag ~ delay_days + I(delay_days^2) + total_order_value,
    data = df,
    family = binomial()
)

summary(model_quad)

## Visualisere sannsynlighetskurve

delay_seq <- data.frame(
  delay_days = seq(0, 20, by = 1),
  total_order_value = mean(df$total_order_value, na.rm = TRUE)
)

delay_seq$pred <- predict(model_delay, delay_seq, type = "response")

plot(delay_seq$delay_days,
     delay_seq$pred,
     type = "l",
     xlab = "Delay days",
     ylab = "Predicted probability of low rating")

## Effekten av delay avhenger av ordreverdi

model_interataction <- glm(
  low_rating_flag ~ delay_days * total_order_value,
  data = df,
  family = binomial()
)

summary(model_interataction)
  
  ## visualiser effekten

q_vals <- quantile(df$total_order_value, c(0.25, 0.75), na.rm = TRUE)

new_data_2 <- expand.grid(
  delay_days = c(0, 10),
  total_order_value = q_vals
)

new_data_2$pred <- predict(model_interataction, new_data_2, type = "response")

new_data_2

library(ggplot2)

delay_seq <- expand.grid(
  delay_days = seq(0, 15, by = 1),
  total_order_value = q_vals
)

delay_seq$pred <- predict(model_interataction, delay_seq, type = "response")

delay_seq$order_value_group <- ifelse(
  delay_seq$total_order_value == q_vals[1],
  "Low order value",
  "High order value"
)

ggplot(delay_seq,
       aes(x = delay_days,
           y = pred,
           color = order_value_group)) +
  geom_line(size = 1.2) +
  labs(
    x = "Delay days",
    y = "Predicted probability of low rating",
    color = "Order value group"
  ) +
  theme_minimal()

  ## effekt i prosentpoeng

library(dplyr)

effects <- new_data_2 %>%
  group_by(total_order_value) %>%
  summarise(effect_pp = diff(pred))

effects

    ## Med 95k observasjoner blir selv sm?? effekter signifikante
    ## Statistisk signifikant ??? forretningsmessig relevant

    ## Although the interaction between delay and order value was statistically significant, the practical effect size was negligible. The impact of delivery delay on customer dissatisfaction is largely consistent across order values.