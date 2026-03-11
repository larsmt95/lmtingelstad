library(httr)
library(jsonlite)
library(dplyr)
library(DBI)
library(RPostgres)

## legge til ny person

res <- POST(
  "https://www.strava.com/oauth/token",
  body = list(
    client_id     = "197542",
    client_secret = "02158c298b1171bcd6a696382d0a2b93ee0da6c8",
    code          = "7059583f24b25744a3dd4052d4fac0a269c75a67", # fra kompis
    grant_type    = "authorization_code"
  ),
  encode = "form"
)

status_code(res)
tokens <- content(res, "parsed")
str(tokens)
tokens$athlete$id



tokens$scope

# LAGRE REFRESH_TOKEN I DATABASEN

dbExecute(
  con,
  "
  INSERT INTO public.strava_tokens (athlete_id, refresh_token, expires_at)
  VALUES ($1,$2,$3)
  ON CONFLICT (athlete_id)
  DO UPDATE SET
    refresh_token = EXCLUDED.refresh_token,
    expires_at    = EXCLUDED.expires_at
  ",
  params = list(
    tokens$athlete$id,
    tokens$refresh_token,
    tokens$expires_at
  )
)

DBI::dbIsValid(con)


update_all_athletes()

  # -----------------------------------------------

res <- GET(
  "https://www.strava.com/api/v3/athlete/activities",
  add_headers(Authorization = paste("Bearer", tokens$access_token))
)

status_code(res)
content(res, "text")


access_token <- tokens$access_token
access_token



tokens <- content(res, "parsed")

tokens$athlete$id
tokens$refresh_token

library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "strava",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "legandary007"
)

dbExecute(
  con,
  "
  INSERT INTO public.strava_tokens (athlete_id, refresh_token, expires_at)
  VALUES ($1, $2, $3)
  ON CONFLICT (athlete_id)
  DO UPDATE SET
    refresh_token = EXCLUDED.refresh_token,
    expires_at    = EXCLUDED.expires_at
  ",
  params = list(
    tokens$athlete$id,
    tokens$refresh_token,
    tokens$expires_at
  )
)

dbDisconnect(con)















## oppdatere egne data
access_token <- "09e4709237503a3d8a38e3557046d870ba03c932"

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

res <- get_activities(1)
status_code(res)
content(res, "text")






