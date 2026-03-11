update_power_zones <- function() {
  
  library(httr)
  library(jsonlite)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  
  con <- dbConnect(
    Postgres(),
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
  
  for (i in seq_len(nrow(athletes))) {
    
    athlete_id    <- athletes$athlete_id[i]
    refresh_token <- athletes$refresh_token[i]
    
    cat("\n--- Starter bruker:", athlete_id, "---\n")
    
    # Refresh token
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
    
    activities <- dbGetQuery(
      con,
      paste0(
        "SELECT activity_id, start_date
         FROM public.activities
         WHERE athlete_id = ", athlete_id,
        " AND type IN ('Ride','VirtualRide')"
      )
    )
    
    for (j in seq_len(nrow(activities))) {
      
      activity_id <- activities$activity_id[j]
      start_date  <- activities$start_date[j]
      activity_year <- format(as.Date(start_date), "%Y")
      
      # Hopp over hvis allerede finnes
      exists_check <- dbGetQuery(
        con,
        paste0(
          "SELECT 1 FROM public.power_zones
           WHERE activity_id = ", activity_id,
          " LIMIT 1"
        )
      )
      
      if (nrow(exists_check) > 0) next
      
      # Hent ??rlig FTP (95% av beste 20 min)
      ftp_row <- dbGetQuery(
        con,
        paste0(
          "SELECT 0.95 * MAX(best_watt) AS ftp
           FROM public.power_curve
           WHERE athlete_id = ", athlete_id,
          " AND duration_sec = 1200
            AND year = ", activity_year
        )
      )
      
      if (nrow(ftp_row) == 0 || is.na(ftp_row$ftp[1])) {
        message("Ingen FTP for ", athlete_id, " ??r ", activity_year)
        next
      }
      
      ftp <- ftp_row$ftp[1]
      
      # Hent watt-stream
      res_stream <- GET(
        paste0("https://www.strava.com/api/v3/activities/",
               activity_id,
               "/streams"),
        query = list(keys = "watts,time", key_by_type = "true"),
        add_headers(Authorization = paste("Bearer", access_token))
      )
      
      if (status_code(res_stream) != 200) next
      
      stream_data <- fromJSON(content(res_stream, "text"))
      
      if (is.null(stream_data$watts$data)) next
      
      watts <- stream_data$watts$data
      time  <- stream_data$time$data
      
      # Robust tidsberegning
      dt <- diff(c(time, max(time)+1))
      
      df <- data.frame(
        watts = watts,
        dt    = dt
      )
      
      df$zone <- cut(
        df$watts,
        breaks = c(
          -Inf,
          0.55 * ftp,
          0.75 * ftp,
          0.90 * ftp,
          1.05 * ftp,
          1.20 * ftp,
          Inf
        ),
        labels = c("Z1","Z2","Z3","Z4","Z5","Z6")
      )
      
      zone_summary <- df %>%
        group_by(zone) %>%
        summarise(seconds = sum(dt), .groups = "drop")
      
      zone_summary$activity_id <- activity_id
      zone_summary$athlete_id  <- athlete_id
      
      dbWriteTable(
        con,
        Id(schema = "public", table = "power_zones"),
        zone_summary,
        append = TRUE,
        row.names = FALSE
      )
      
      Sys.sleep(0.5)
    }
    
    message("Ferdig med bruker ", athlete_id)
  }
  
  dbDisconnect(con)
  message("Power zones oppdatert.")
}


update_power_zones()
