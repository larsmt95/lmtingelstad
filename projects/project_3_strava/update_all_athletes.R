library(httr)
library(jsonlite)
library(dplyr)
library(DBI)
library(RPostgres)

update_all_athletes <- function() {
  
  library(httr)
  library(jsonlite)
  library(dplyr)
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
  
  athletes <- dbGetQuery(
    con,
    "SELECT athlete_id, refresh_token FROM public.strava_tokens"
  )
  
  if (nrow(athletes) == 0) {
    message("Ingen registrerte ut??vere.")
    dbDisconnect(con)
    return(invisible(NULL))
  }
  
  for (i in seq_len(nrow(athletes))) {
    
    athlete_id    <- as.numeric(athletes$athlete_id[i])
    refresh_token <- athletes$refresh_token[i]
    
    cat("\n--- Starter bruker:", athlete_id, "---\n")
    
    # ---- Refresh token ----
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
    
    if (status_code(res_token) != 200) {
      message("Refresh feilet for ", athlete_id)
      next
    }
    
    access_token <- content(res_token, "parsed")$access_token
    
    # ---- Finn siste aktivitet ----
    last_date <- dbGetQuery(
      con,
      paste0(
        "SELECT MAX(start_date)
         FROM public.activities
         WHERE athlete_id = ", athlete_id
      )
    )[[1]]
    
    after_ts <- if (is.na(last_date)) {
      0
    } else {
      as.integer(as.POSIXct(last_date, tz = "UTC")) - 3600
    }
    
    # ---- Hent nye aktiviteter ----
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
    
    if (length(all_activities) == 0) {
      message("Ingen nye ??kter for ", athlete_id)
      next
    }
    
    activities <- bind_rows(all_activities)
    
    # ---- Klargj??r data ----
    activities_sql <- activities %>%
      rename(activity_id = id) %>%
      select(any_of(c(
        "activity_id",
        "name",
        "type",
        "start_date",
        "distance",
        "moving_time",
        "elapsed_time",
        "total_elevation_gain",
        "average_speed",
        "average_heartrate",
        "location_city",
        "location_state",
        "location_country",
        "max_speed",
        "average_watts",
        "max_watts",
        "elev_high",
        "elev_low",
        "has_kudoed",
        "max_heartrate"
      )))
      
      required_cols <- c(
        "activity_id","name","type","start_date","distance",
        "moving_time","elapsed_time","total_elevation_gain",
        "average_speed","average_heartrate",
        "location_city","location_state","location_country",
        "max_speed","average_watts","max_watts",
        "elev_high","elev_low","has_kudoed",
        "max_heartrate"
      )
    
    missing_cols <- setdiff(required_cols, names(activities_sql))
    
    if (length(missing_cols) > 0) {
      activities_sql[missing_cols] <- NA
    }
    
    activities_sql <- activities_sql %>%    
      mutate(
        athlete_id = athlete_id,
        distance = as.numeric(distance),
        moving_time = as.numeric(moving_time),
        elapsed_time = as.numeric(elapsed_time),
        total_elevation_gain = as.numeric(total_elevation_gain),
        average_speed = as.numeric(average_speed),
        max_speed = as.numeric(max_speed),
        average_heartrate = as.numeric(average_heartrate),
        max_heartrate = as.numeric(max_heartrate),
        average_watts = as.numeric(average_watts),
        max_watts = as.numeric(max_watts),
        elev_high = as.numeric(elev_high),
        elev_low = as.numeric(elev_low)
      )
    
    # ---- Midlertidig tabell ----
    dbWriteTable(
      con,
      "temp_activities",
      activities_sql,
      temporary = TRUE,
      overwrite = TRUE
    )
    
    # ---- Insert med ON CONFLICT ----
    dbExecute(
      con,
      "
      INSERT INTO public.activities (
        activity_id,
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
        max_heartrate,
        athlete_id
      )
      SELECT
        activity_id,
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
        max_heartrate,
        athlete_id
      FROM temp_activities
      ON CONFLICT (activity_id) DO NOTHING
      "
    )
    
    message("Ferdig med bruker ", athlete_id)
  }
  
  dbDisconnect(con)
  message("Oppdatering ferdig.")
}


update_all_athletes()



