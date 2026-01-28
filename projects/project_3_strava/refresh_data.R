library(httr)
library(jsonlite)
library(dplyr)
library(DBI)
library(RPostgres)

nrow(activities_sql)

refresh_access_token <- function(refresh_token) {
  
  res <- httr::POST(
    "https://www.strava.com/oauth/token",
    body = list(
      client_id     = "197542",   # ???? eksakt tallet fra Strava
      client_secret = "b6433d7c6b92ff61402f5118969b37afcbe49906",
      refresh_token = refresh_token,
      grant_type    = "refresh_token"
    ),
    encode = "form"
  )
  
  print(status_code(res))
  print(content(res, "text"))
  
  content(res, "parsed")
}


# ???? hent fra DB
tokens <- refresh_access_token("bfbe37bc4f19e399a0f222a621d8a84ccf025bf8")
tokens
access_token <- tokens$access_token

res_test <- httr::GET(
  "https://www.strava.com/api/v3/athlete/activities?per_page=1",
  httr::add_headers(Authorization = paste("Bearer", access_token))
)

httr::status_code(res_test)

get_activities <- function(page, token) {
  GET(
    "https://www.strava.com/api/v3/athlete/activities",
    query = list(per_page = 200, page = page),
    add_headers(Authorization = paste("Bearer", token))
  )
}

# --- finn siste dato i databasen ---
last_date <- dbGetQuery(
  con,
  "SELECT MAX(start_date) AS last_date FROM public.activities"
)$last_date

# hvis tabellen er tom (f??rste gang)
if (is.na(last_date)) {
  after_ts <- 0
} else {
  after_ts <- as.integer(as.POSIXct(last_date, tz = "UTC"))
}

# --- hent nye aktiviteter ---
all_activities <- list()
page <- 1

repeat {
  
  res <- httr::GET(
    "https://www.strava.com/api/v3/athlete/activities",
    query = list(
      per_page = 200,
      page = page,
      after = after_ts
    ),
    httr::add_headers(Authorization = paste("Bearer", access_token))
  )
  
  status <- httr::status_code(res)
  
  if (status == 429) {
    message("Rate limit n??dd ??? avbryter for i dag")
    break
  }
  
  if (status != 200) {
    stop("Strava API-feil: ", status)
  }
  
  data <- jsonlite::fromJSON(
    httr::content(res, "text"),
    flatten = TRUE
  )
  
  if (length(data) == 0) break
  
  all_activities[[page]] <- data
  page <- page + 1
  
  Sys.sleep(1)   # ???? viktig
}

activities <- dplyr::bind_rows(all_activities)

