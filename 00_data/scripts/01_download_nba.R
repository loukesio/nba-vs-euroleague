# Download NBA data via hoopR's pre-compiled release files (sportsdataverse) and cache them
# on the data drive. stats.nba.com is not called directly (it blocks scripted access).
#
#   team box (ESPN):    2001/02-2025/26  -> lead-lag model needs the longer series
#   player box (ESPN):  2010/11-2025/26  -> minutes, for the rotation entropy
#   shots (NBA Stats):  2010/11-2025/26  -> x/y shot locations
#
# hoopR naming quirks: the ESPN loaders take the season END year (2011 = 2010/11), while
# load_nba_stats_shots() takes the START year. Files here are always named by START year.

source(here::here("R", "config.R"))
suppressMessages(library(hoopR))

save_season <- function(df, kind, s) {
  dir.create(file.path(RAW_NBA, kind), recursive = TRUE, showWarnings = FALSE)
  path <- file.path(RAW_NBA, kind, paste0(kind, "_", s, ".parquet"))
  arrow::write_parquet(df, path)
  message(sprintf("  %-10s %s: %d rows", kind, season_label(s), nrow(df)))
}

done <- \(kind, s) file.exists(file.path(RAW_NBA, kind, paste0(kind, "_", s, ".parquet")))

for (s in SEASONS_LONG) {
  if (!done("team_box", s)) save_season(load_nba_team_box(seasons = s + 1), "team_box", s)
}
for (s in SEASONS) {
  if (!done("player_box", s)) save_season(load_nba_player_box(seasons = s + 1), "player_box", s)
  if (!done("shots", s))      save_season(load_nba_stats_shots(seasons = s), "shots", s)
}

writeLines(format(Sys.time(), tz = "UTC", usetz = TRUE), file.path(RAW_NBA, "DOWNLOADED_AT.txt"))
message("Done.")
