## 2.5 SQL ??? skrive data

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = "strava",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "legandary007"
)

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
  average_temp,
  max_heartrate,
  athlete.id
)

dbWriteTable(
  con,
  name = Id(schema = "public", table = "activities"),
  value = activities_sql,
  append = TRUE
)
