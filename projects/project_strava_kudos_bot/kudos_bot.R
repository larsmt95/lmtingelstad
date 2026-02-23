library(httr)
library(jsonlite)
library(dplyr)

res <- httr::POST(
  "https://www.strava.com/oauth/token",
  body = list(
    client_id = "197542",
    client_secret = "02158c298b1171bcd6a696382d0a2b93ee0da6c8",
    code = "d22d1510c927c4a399a5cc10b4c18b23dfc26ce5",
    grant_type = "authorization_code"
  ),
  encode = "form"
)

tokens <- content(res)
str(tokens)

access_token  <- tokens$access_token
refresh_token <- tokens$refresh_token

# 2?????? Hent egne aktiviteter siste 24t
act_res <- GET(
  "https://www.strava.com/api/v3/athlete/activities",
  add_headers(Authorization = paste("Bearer", access_token)),
  query = list(per_page = 5)
)

print(act_res$status_code)

test_id <- fromJSON(content(act_res, as = "text"))$id[1]

kudos_res <- POST(
  paste0("https://www.strava.com/api/v3/activities/", test_id, "/kudos"),
  add_headers(Authorization = paste("Bearer", access_token))
)

print(kudos_res$status_code)




## N?? kan vi hente aktiviteter riktig
activities <- content(res, as = "text") |>
  fromJSON(flatten = TRUE)

str(activities)


## S?? filtrerer vi siste 24t
activities <- activities |>
  mutate(start_date = as.POSIXct(start_date, tz = "UTC")) |>
  filter(start_date > Sys.time() - 86400)

nrow(activities)

print(activities$id)
print(nrow(activities))

test_id <- activities$id[1]

kudos_res <- POST(
  paste0("https://www.strava.com/api/v3/activities/", test_id, "/kudos"),
  add_headers(Authorization = paste("Bearer", access_token))
)

print(kudos_res$status_code)
print(content(kudos_res, as = "text"))

tokens <- content(res)
print(tokens$scope)



# 3?????? Gi kudos
for (id in activities$id) {
  POST(
    paste0("https://www.strava.com/api/v3/activities/", id, "/kudos"),
    add_headers(Authorization = paste("Bearer", access_token))
  )
  Sys.sleep(1)  # liten delay s?? det ser menneskelig ut
}

cat("Kudos sendt:", nrow(activities), "\n")
