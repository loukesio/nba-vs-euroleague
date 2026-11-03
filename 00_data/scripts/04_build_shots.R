# Build one shot-level table for both leagues -> processed/shots.parquet
# Coordinates are converted to METRES with the origin at the centre of the hoop:
#   x = sideline-to-sideline (negative/positive = either side), y = towards half court.
#   NBA legacy units are tenths of feet; EuroLeague units are centimetres.

source(here::here("R", "config.R"))
source(here::here("R", "court.R"))
suppressMessages({ library(dplyr); library(purrr); library(arrow); library(jsonlite) })

# ---- NBA ---------------------------------------------------------------------
nba <- map_dfr(SEASONS, function(s) {
  read_parquet(file.path(RAW_NBA, "shots", sprintf("shots_%d.parquet", s))) |>
    filter(season_type_id %in% c("2", "4")) |>
    transmute(
      league = "NBA", season = s,
      phase = if_else(season_type_id == "4", "playoffs", "regular"),
      game_id = as.character(game_id), team = team_tricode,
      player_id = as.character(person_id), player = player_name,  # NBA Stats: surname only
      x = x_legacy / 10 * 0.3048, y = y_legacy / 10 * 0.3048,
      value = shot_value, made = shot_result == "Made"
    )
})

# ---- EuroLeague ----------------------------------------------------------------
EL_MADE   <- c("2FGM", "3FGM", "LAYUPMD", "DUNK")
EL_MISSED <- c("2FGA", "3FGA", "LAYUPATT", "2FGAB", "3FGAB")

el <- map_dfr(SEASONS, function(s) {
  sc <- paste0("E", s)
  games <- fromJSON(file.path(RAW_EL, "games", paste0(sc, ".json")))$data |>
    filter(played, phaseType$code %in% names(EL_PHASE_MAP))
  map_dfr(seq_len(nrow(games)), function(i) {
    path <- file.path(RAW_EL, "shots", sc, paste0(games$gameCode[i], ".json"))
    if (!file.exists(path)) return(NULL)
    rows <- fromJSON(path)$Rows
    if (!length(rows) || !nrow(rows)) return(NULL)
    rows |>
      mutate(action = trimws(ID_ACTION)) |>
      filter(action %in% c(EL_MADE, EL_MISSED)) |>
      transmute(
        league = "EuroLeague", season = s,
        phase = unname(EL_PHASE_MAP[games$phaseType$code[i]]),
        game_id = paste0(sc, "_", games$gameCode[i]), team = trimws(TEAM),
        # shot feed IDs are "P" + the box-score person code
        player_id = sub("^P", "", trimws(ID_PLAYER)), player = trimws(PLAYER),
        x = COORD_X / 100, y = COORD_Y / 100,
        value = if_else(substr(action, 1, 1) == "3", 3L, 2L),
        made = action %in% EL_MADE
      )
  })
})

shots <- bind_rows(nba, el) |>
  mutate(dist = sqrt(x^2 + y^2)) |>
  # drop impossible coordinates (data-entry errors); keep heaves inside the court
  filter(abs(x) <= 8, y >= -1.7, y <= 27) |>
  mutate(zone = shot_zone(league, x, y, dist, value),
         pts = value * made)

write_parquet(shots, file.path(PROCESSED, "shots.parquet"))

shots |>
  group_by(league, season) |>
  summarise(n = n(), p3_share = mean(value == 3), fg = mean(made), .groups = "drop") |>
  print(n = 40)
