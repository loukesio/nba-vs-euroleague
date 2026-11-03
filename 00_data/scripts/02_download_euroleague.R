# Download EuroLeague game lists, box scores and shot locations from the official API.
# Raw JSON is cached per game on the data drive, so the script can be stopped and re-run safely.
#
#   games:  api-live.euroleague.net/v2/competitions/E/seasons/E{yyyy}/games
#   box:    api-live.euroleague.net/v3/competitions/E/seasons/E{yyyy}/games/{code}/stats
#   shots:  live.euroleague.net/api/Points?gamecode={code}&seasoncode=E{yyyy}
#
# Box scores are downloaded for SEASONS_LONG (2001/02 on, for the lead-lag model);
# shot locations only for the main window SEASONS (2010/11 on).

source(here::here("R", "config.R"))
library(httr2)
library(jsonlite)

# The shot endpoint (live.euroleague.net/api/Points) is rate-limited (HTTP 429 with a
# Retry-After header); the box-score API is not. So the script runs in two modes:
#   EL_MODE=box   Rscript 02_download_euroleague.R   -> game lists + box scores (fast)
#   EL_MODE=shots Rscript 02_download_euroleague.R   -> shot locations, one slow worker
# With no EL_MODE set, both are downloaded in one pass.
MODE  <- Sys.getenv("EL_MODE", "all")
PAUSE <- if (MODE == "box") 0.2 else 2  # seconds between requests

# Optional: pass a subset of seasons to run several workers in parallel,
# e.g. `Rscript 02_download_euroleague.R 2010 2014 2018 2022`
args <- as.integer(commandArgs(trailingOnly = TRUE))
run_seasons <- if (length(args)) args else SEASONS_LONG

fetch_cached <- function(url, path) {
  if (file.exists(path) && file.size(path) > 0) return(invisible(path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # Network failures (as opposed to HTTP errors) are skipped; a re-run fetches what is missing.
  resp <- tryCatch(request(url) |>
    req_user_agent("NBA_VS_Euroleague research project (R/httr2)") |>
    req_retry(max_tries = 6, backoff = \(i) 5 * i) |>  # honours Retry-After on 429
    req_timeout(60) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform(), error = \(e) { message("  network error: ", url); NULL })
  Sys.sleep(PAUSE)
  if (is.null(resp)) return(invisible(NULL))
  if (resp_status(resp) != 200) {
    message("  HTTP ", resp_status(resp), ": ", url)
    return(invisible(NULL))
  }
  writeLines(resp_body_string(resp), path)
  invisible(path)
}

for (s in run_seasons) {
  sc <- paste0("E", s)
  message("Season ", season_label(s))
  games_path <- file.path(RAW_EL, "games", paste0(sc, ".json"))
  fetch_cached(sprintf("https://api-live.euroleague.net/v2/competitions/E/seasons/%s/games", sc), games_path)

  games <- fromJSON(games_path)$data
  codes <- games$gameCode[games$played]
  message("  ", length(codes), " played games")

  for (g in codes) {
    if (MODE != "shots") fetch_cached(
      sprintf("https://api-live.euroleague.net/v3/competitions/E/seasons/%s/games/%d/stats", sc, g),
      file.path(RAW_EL, "box", sc, paste0(g, ".json"))
    )
    if (MODE != "box" && s %in% SEASONS) fetch_cached(
      sprintf("https://live.euroleague.net/api/Points?gamecode=%d&seasoncode=%s", g, sc),
      file.path(RAW_EL, "shots", sc, paste0(g, ".json"))
    )
  }
}

writeLines(format(Sys.time(), tz = "UTC", usetz = TRUE), file.path(RAW_EL, "DOWNLOADED_AT.txt"))
message("Done.")
