library(httr)
library(jsonlite)
library(dplyr)
library(DBI)
library(RPostgres)

update_all_athletes <- function() {
  
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "strava",
    host     = "localhost",
    port     = 5432,
    user     = "postgres",
    password = "legandary007"
  )
  
  athletes <- dbGetQuery(con,
                         "SELECT athlete_id, refresh_token FROM public.strava_tokens"
  )
  
  for (i in seq_len(nrow(athletes))) {
    
    athlete_id    <- athletes$athlete_id[i]
    refresh_token <- athletes$refresh_token[i]
    
    message("Oppdaterer: ", athlete_id)
    
    # --- refresh ---
    res_token <- POST(
      "https://www.strava.com/oauth/token",
      body = list(
        client_id     = "197542",
        client_secret = "02158c298b1171bcd6a696382d0a2b93ee0da6c8",
        refresh_token = refresh_token,
        grant_type    = "refresh_token"
      ),
      encode = "form"
    )
    
    if (status_code(res_token) != 200) next
    
    access_token <- content(res_token, "parsed")$access_token
    
    # --- finn siste dato ---
    last_date <- dbGetQuery(
      con,
      paste0(
        "SELECT MAX(start_date) 
         FROM public.activities 
         WHERE athlete_id = ", athlete_id
      )
    )[[1]]
    
    if (is.na(last_date)) {
      after_ts <- 0
    } else {
      after_ts <- as.integer(as.POSIXct(last_date, tz = "UTC")) - 3600
    }
    
    # --- hent nye ??kter ---
    all_activities <- list()
    page <- 1
    
    repeat {
      
      res <- GET(
        "https://www.strava.com/api/v3/athlete/activities",
        query = list(
          per_page = 200,
          page = page,
          after = after_ts
        ),
        add_headers(Authorization = paste("Bearer", access_token))
      )
      
      if (status_code(res) != 200) break
      
      data <- fromJSON(content(res, "text"), flatten = TRUE)
      
      if (length(data) == 0) break
      
      all_activities[[page]] <- data
      page <- page + 1
      
      Sys.sleep(1)
    }
    
    if (length(all_activities) == 0) next
    
    activities <- bind_rows(all_activities)
    
    activities_sql <- activities %>%
      select(
        activity_id = id,
        name,
        type,
        start_date,
        distance,
        moving_time,
        elapsed_time,
        total_elevation_gain,
        average_speed,
        average_heartrate
      )
    
    activities_sql$athlete_id <- athlete_id
    
    dbWriteTable(
      con,
      name = Id(schema = "public", table = "activities"),
      value = activities_sql,
      append = TRUE
    )
    
    message("La inn ", nrow(activities_sql), " ??kter.")
  }
  
  dbDisconnect(con)
  message("Oppdatering ferdig.")
}

# --- Kj??r slik ---
update_all_athletes()
