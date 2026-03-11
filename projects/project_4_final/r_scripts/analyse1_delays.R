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

## HENTE NØDVENDIG DATA FRA SQL
query <- "
SELECT 
    low_rating_flag,
    delay_flag,
    total_order_value,
    product_category_name_english
FROM public.fact_orders_v5
"

df <- dbGetQuery(con, query)

df <- df %>% 
    rename(category = product_category_name_english)

df$category <- as.factor(df$category)
df$low_rating_flag <- as.numeric(df$low_rating_flag)
df$delay_flag <- as.numeric(df$delay_flag)

## Sjekke baseline
mean(df$low_rating_flag)

## KJØRE LOGISTISK REGRESJON

model <- glm(
  low_rating_flag ~ delay_flag + total_order_value + category,
  data = df,
  family = binomial()
)

summary(model)

## Konverter til Odds Ratio
exp(coef(model))


## Med sammenslåtte kategorier 

df$category_macro <- case_when(
  
  df$category %in% c("computers","computers_accessories","electronics",
                     "telephony","tablets_printing_image","consoles_games",
                     "audio","fixed_telephony") ~ "electronics_tech",
  
  df$category %in% c("fashion_male_clothing","fashion_female_clothing",
                     "fashion_shoes","fashion_bags_accessories",
                     "fashion_underwear_beach","fashion_childrens_clothes",
                     "watches_gifts") ~ "fashion",
  
  df$category %in% c("furniture_decor","furniture_living_room",
                     "furniture_bedroom","housewares","home_appliances",
                     "home_construction","kitchen_dining_laundry_garden_furniture",
                     "bed_bath_table","garden_tools") ~ "home_furniture",
  
  df$category %in% c("health_beauty","perfumery",
                     "diapers_and_hygiene") ~ "beauty_health",
  
  df$category %in% c("sports_leisure","toys",
                     "cool_stuff","musical_instruments") ~ "sports_leisure",
  
  df$category %in% c("books_general_interest","books_technical",
                     "books_imported","dvds_blu_ray",
                     "cds_dvds_musicals","music") ~ "books_media",
  
  df$category %in% c("food","food_drink","drinks") ~ "food_drinks",
  
  TRUE ~ "other"
)

df$category_macro <- as.factor(df$category_macro)

model_macro <- glm(
  low_rating_flag ~ delay_flag + total_order_value + category_macro,
  data = df,
  family = binomial()
)

summary(model_macro)
exp(coef(model_macro))

## Effekt i prosentpoeng
new_data <- data.frame(
  delay_flag = c(0, 1),
  total_order_value = mean(df$total_order_value, na.rm = TRUE),
  category = levels(df$category)[1]
)

pred <- predict(model, new_data, type = "response")
pred
diff(pred)
