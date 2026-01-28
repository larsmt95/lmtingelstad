# =========================
# ENGANGS: AUTHORIZATION
# =========================

library(httr)
library(jsonlite)

# ???? FYLL INN
CLIENT_ID     <- "197542"
CLIENT_SECRET <- "b6433d7c6b92ff61402f5118969b37afcbe49906"
CODE_FROM_USER <- "425f2f7a54712710a1eb6e2d5f201eeb7fe6fe6d"

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

httr::status_code(res)
httr::content(res, "text")
tokens <- httr::content(res, "parsed")
tokens

# ???? LAGRE DISSE (DB / fil)
tokens$athlete$id
tokens$access_token
tokens$refresh_token
tokens$expires_at
