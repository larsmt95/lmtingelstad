library(httr)
library(jsonlite)
library(dplyr)
library(DBI)
library(RPostgres)

## legge til ny person

res <- POST(
  "https://www.strava.com/oauth/token",
  body = list(
    client_id     = Sys.getenv("197542"),
    client_secret = Sys.getenv("b6433d7c6b92ff61402f5118969b37afcbe49906"),
    code          = "KODEN_DU_FIKK",
    grant_type    = "authorization_code"
  ),
  encode = "form"
)

tokens <- content(res, "parsed")


## oppdatere egne data
access_token <- "e5174152cbf7853a3e58459f5c7476230c554948"

get_activities <- function(page) {
  GET(
    "https://www.strava.com/api/v3/athlete/activities",
    query = list(per_page = 200, page = page),
    add_headers(Authorization = paste("Bearer", access_token))
  )
}

all_activities <- list()
page <- 1

repeat {
  res <- get_activities(page)
  data <- fromJSON(content(res, "text"), flatten = TRUE)
  
  if (length(data) == 0) break
  
  all_activities[[page]] <- data
  page <- page + 1
}

activities <- bind_rows(all_activities)

activities


## SQL

library(DBI)
library(RPostgres)

## koble til default-databasen
con <- dbConnect(
  Postgres(),
  dbname   = "postgres",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "legandary007"
)

## opprette database
# dbExecute(con, "CREATE DATABASE strava")

## koble p?? nytt - n?? mot strava 
dbDisconnect(con)

con <- dbConnect(
  Postgres(),
  dbname   = "strava",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "legandary007"
)


## klargj??re data - nested (Strava) til flate kolonner (SQL/powerBI)

library(dplyr)

activities_sql <- dplyr::select(
  activities,
  activity_id = id,
  name,
  type,
  start_date,
  distance,
  moving_time,
  elapsed_time,
  total_elevation_gain,
  average_speed,
  average_heartrate,
  location_city,
  location_state,
  location_country,
  max_speed,
  average_watts,
  max_watts,
  elev_high,
  elev_low,
  has_kudoed,
  average_temp,
  average_heartrate,
  max_heartrate,
  athlete.id
)

names(activities)

activities[id]


## Skrive til Postgres

dbWriteTable(
  con,
  name = Id(schema = "public", table = "activities"),
  value = activities_sql,
  overwrite = TRUE
)


## Verifiser 

dbGetQuery(con, "SELECT COUNT(*) FROM activities")


## Lage indeks

dbGetQuery(con, "
  SELECT COUNT(*)
  FROM public.activities
")



