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
    client_secret = "b6433d7c6b92ff61402f5118969b37afcbe49906",
    code          = "fff596723b0aa2dcca9c0e4bd2894f93a62b027c", # fra kompis
    grant_type    = "authorization_code"
  ),
  encode = "form"
)

status_code(res)
tokens <- content(res, "parsed")
tokens$athlete$username

tokens$scope

res <- GET(
  "https://www.strava.com/api/v3/athlete/activities",
  add_headers(Authorization = paste("Bearer", tokens$access_token))
)

status_code(res)
content(res, "text")


access_token <- tokens$access_token
access_token




















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






