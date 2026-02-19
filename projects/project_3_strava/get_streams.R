# GET STREAMS

head(activities$id)


res <- POST(
  "https://www.strava.com/oauth/token",
  body = list(
    client_id = "197542",
    client_secret = "b6433d7c6b92ff61402f5118969b37afcbe49906",
    code = "05e2978d031a7e02363fbece5cabb81b8aa4da29", # fra kompis 
    grant_type = "authorization_code"
  ),
  encode = "form"
)

content(res)

token_data <- content(res)

access_token <- token_data$access_token
refresh_token <- token_data$refresh_token


res_token <- POST(
  "https://www.strava.com/oauth/token",
  body = list(
    client_id     = "197542",
    client_secret = "b6433d7c6b92ff61402f5118969b37afcbe49906",
    refresh_token = "c79b9a060376a117e15396fb1f51ea02ae1cd4bd",
    grant_type    = "refresh_token"
  ),
  encode = "form"
)

access_token <- content(res_token, "parsed")$access_token

test_id <- 17013202674

res_stream <- GET(
  paste0("https://www.strava.com/api/v3/activities/", test_id, "/streams"),
  query = list(
    keys = "watts,time",
    key_by_type = "true"
  ),
  add_headers(Authorization = paste("Bearer", access_token))
)

status_code(res_stream)

stream_data <- content(res_stream, as = "parsed")

names(stream_data)
head(stream_data$watts$data)
head(stream_data$time$data)

df <- data.frame(
  time  = unlist(stream_data$time$data),
  watts = unlist(stream_data$watts$data)
)

library(zoo)

df$roll_20min <- rollapply(
  df$watts,
  width = 1200,
  FUN = mean,
  align = "left",
  fill = NA
)

max_20min <- max(df$roll_20min, na.rm = TRUE)

max_20min

durations <- c(5, 30, 60, 300, 1200)

power_curve <- sapply(durations, function(d) {
  if (nrow(df) >= d) {
    max(rollapply(df$watts, d, mean, align = "left", fill = NA), na.rm = TRUE)
  } else {
    NA
  }
})

data.frame(
  duration_sec = durations,
  best_watt = power_curve
)


# FULL FUNKSJON - ALLE UT??VERE

library(httr)
library(jsonlite)
library(dplyr)
library(DBI)
library(RPostgres)
library(zoo)

# --- Varigheter vi ??nsker ---
durations <- c(5, 30, 60, 300, 600, 1200, 3600, 7200)

# --- Rolling best funksjon ---
compute_power_curve <- function(watts, durations) {
  
  results <- data.frame()
  
  for (d in durations) {
    
    if (length(watts) >= d) {
      best <- max(rollapply(watts, d, mean, align = "left"), na.rm = TRUE)
    } else {
      best <- NA
    }
    
    results <- rbind(results,
                     data.frame(duration_sec = d,
                                best_watt = best))
  }
  
  return(results)
}

# --- Hovedfunksjon ---
update_power_curve <- function() {
  
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
  
  athletes$athlete_id <- as.numeric(athletes$athlete_id)
  
  for (i in seq_len(nrow(athletes))) {
    
    athlete_id    <- athletes$athlete_id[i]
    refresh_token <- athletes$refresh_token[i]
    
    cat("\nStarter athlete:", athlete_id, "\n")
    
    # --- refresh ---
    res_token <- POST(
      "https://www.strava.com/oauth/token",
      body = list(
        client_id     = "197542",
        client_secret = "b6433d7c6b92ff61402f5118969b37afcbe49906",
        refresh_token = refresh_token,
        grant_type    = "refresh_token"
      ),
      encode = "form"
    )
    
    if (status_code(res_token) != 200) next
    
    access_token <- content(res_token, "parsed")$access_token
    
    # --- hent aktiviteter ---
    activities <- dbGetQuery(
      con,
      paste0("SELECT activity_id, start_date
          FROM public.activities
          WHERE athlete_id = ", athlete_id, "
          AND type IN ('Ride','VirtualRide')")
    )
    
    
    for (j in seq_len(nrow(activities))) {
      
      activity_id <- activities$activity_id[j]
      start_date  <- activities$start_date[j]
      year_val    <- as.numeric(format(as.Date(start_date), "%Y"))
      
      # --- hent watt stream ---
      res_stream <- GET(
        paste0("https://www.strava.com/api/v3/activities/",
               activity_id,
               "/streams"),
        query = list(keys = "watts", key_by_type = "true"),
        add_headers(Authorization = paste("Bearer", access_token))
      )
      
      if (status_code(res_stream) != 200) next
      
      stream_data <- fromJSON(content(res_stream, "text"))
      
      if (is.null(stream_data$watts$data)) next
      
      watts <- stream_data$watts$data
      
      # --- beregn power curve ---
      curve <- compute_power_curve(watts, durations)
      
      for (k in seq_len(nrow(curve))) {
        
        row <- curve[k,]
        
        dbExecute(
          con,
          "
          INSERT INTO public.power_curve
          (activity_id, athlete_id, start_date,
           year, duration_sec, best_watt)
          VALUES ($1,$2,$3,$4,$5,$6)
          ON CONFLICT (activity_id, duration_sec)
          DO UPDATE SET best_watt = EXCLUDED.best_watt
          ",
          params = list(
            activity_id,
            athlete_id,
            start_date,
            year_val,
            row$duration_sec,
            row$best_watt
          )
        )
      }
      
      Sys.sleep(0.5)
    }
  }
  
  dbDisconnect(con)
  cat("Power curve oppdatert.\n")
}



build_all_power_curves(con)

update_power_curve()

