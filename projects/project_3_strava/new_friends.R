library(httr)
library(jsonlite)
library(DBI)
library(RPostgres)

# --- FYLL INN ---
CLIENT_ID     <- "197542"
CLIENT_SECRET <- "b6433d7c6b92ff61402f5118969b37afcbe49906"
CODE_FROM_USER <- "b383e85171b3794fb77e8895e12f303531d444c5"

# --- Bytt code til tokens ---
res <- POST(
  "https://www.strava.com/oauth/token",
  body = list(
    client_id     = CLIENT_ID,
    client_secret = CLIENT_SECRET,
    code          = CODE_FROM_USER,
    grant_type    = "authorization_code"
  ),
  encode = "form"
)

stopifnot(status_code(res) == 200)

tokens <- content(res, "parsed")

# --- Koble til DB ---
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "strava",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "legandary007"
)

# --- Lagre/oppdater token ---
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

message("Ferdig ??? ut??ver lagret.")
