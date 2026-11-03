# Build one tidy team-game table for both leagues -> processed/team_games.parquet
#
# Consistency choices (documented in README):
#   * Rebounds and turnovers are player-credited sums in both leagues (team rebounds /
#     team turnovers are recorded differently by ESPN and the EuroLeague, so they are dropped).
#   * Fouls received = fouls committed by the opponent.
#   * Game length (minutes) is derived from total player minutes, which identifies overtimes.

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
suppressMessages({ library(dplyr); library(purrr); library(arrow); library(jsonlite) })

# ---- NBA ---------------------------------------------------------------------
read_nba <- function(s) {
  tb <- read_parquet(file.path(RAW_NBA, "team_box", sprintf("team_box_%d.parquet", s)))
  pb_path <- file.path(RAW_NBA, "player_box", sprintf("player_box_%d.parquet", s))

  team <- tb |>
    filter(season_type %in% c(2, 3)) |>
    transmute(
      league = "NBA", season = s,
      phase = if_else(season_type == 3, "playoffs", "regular"),
      game_id = as.character(game_id), date = as.Date(game_date),
      team = team_abbreviation, opp = opponent_team_abbreviation, team_id,
      home = team_home_away == "home", win = as.logical(team_winner),
      pts = team_score, opp_pts = opponent_team_score,
      p3m = three_point_field_goals_made, p3a = three_point_field_goals_attempted,
      p2m = field_goals_made - p3m, p2a = field_goals_attempted - p3a,
      ftm = free_throws_made, fta = free_throws_attempted,
      ast = assists, stl = steals, blk = blocks, pf = fouls,
      orb = offensive_rebounds, drb = defensive_rebounds
    )

  # Player box: turnovers (player-credited) and minutes. Only for seasons we downloaded.
  if (!file.exists(pb_path)) {
    return(team |> left_join(
      tb |> transmute(game_id = as.character(game_id), team_id, tov = turnovers),
      by = c("game_id", "team_id")) |>
      mutate(entropy_min = NA_real_, game_min = NA_real_) |> select(-team_id))
  }
  pb <- read_parquet(pb_path) |>
    mutate(minutes = coalesce(as.numeric(minutes), 0)) |>
    group_by(game_id = as.character(game_id), team_id) |>
    summarise(tov = sum(turnovers, na.rm = TRUE),
              entropy_min = minutes_entropy(minutes),
              team_min = sum(minutes), .groups = "drop") |>
    # 240 player-minutes in regulation, +25 per overtime (minutes are rounded in ESPN data)
    mutate(n_ot = pmax(0, round((team_min - 240) / 25)), game_min = 48 + 5 * n_ot)
  team |> inner_join(pb |> select(game_id, team_id, tov, entropy_min, game_min),
                     by = c("game_id", "team_id")) |> select(-team_id)
}

# ---- EuroLeague ----------------------------------------------------------------
read_el_season <- function(s) {
  sc <- paste0("E", s)
  games <- fromJSON(file.path(RAW_EL, "games", paste0(sc, ".json")))$data |>
    filter(played, phaseType$code %in% names(EL_PHASE_MAP))
  map_dfr(seq_len(nrow(games)), function(i) {
    g <- games[i, ]
    path <- file.path(RAW_EL, "box", sc, paste0(g$gameCode, ".json"))
    if (!file.exists(path)) return(NULL)
    box <- fromJSON(path)
    map_dfr(c("local", "road"), function(side) {
      other <- setdiff(c("local", "road"), side)
      pl <- box[[side]]$players$stats
      tibble(
        league = "EuroLeague", season = s,
        phase = unname(EL_PHASE_MAP[g$phaseType$code]),
        game_id = paste0(sc, "_", g$gameCode), date = as.Date(substr(g$date, 1, 10)),
        team = g[[side]]$club$code, opp = g[[other]]$club$code,
        home = side == "local" & !isTRUE(g$isNeutralVenue),
        pts = g[[side]]$score, opp_pts = g[[other]]$score,
        win = pts > opp_pts,
        p2m = sum(pl$fieldGoalsMade2), p2a = sum(pl$fieldGoalsAttempted2),
        p3m = sum(pl$fieldGoalsMade3), p3a = sum(pl$fieldGoalsAttempted3),
        ftm = sum(pl$freeThrowsMade), fta = sum(pl$freeThrowsAttempted),
        ast = sum(pl$assistances), stl = sum(pl$steals), blk = sum(pl$blocksFavour),
        pf = sum(pl$foulsCommited),
        orb = sum(pl$offensiveRebounds), drb = sum(pl$defensiveRebounds),
        tov = sum(pl$turnovers),
        entropy_min = minutes_entropy(pl$timePlayed),
        # 200 player-minutes in regulation, +25 per overtime
        game_min = 40 + 5 * pmax(0, round((sum(pl$timePlayed) / 60 - 200) / 25))
      )
    })
  })
}

nba <- map_dfr(SEASONS_LONG, read_nba)
el  <- map_dfr(SEASONS_LONG, read_el_season)

team_games <- bind_rows(nba, el) |>
  # Drop non-competitive games: All-Star games (ESPN codes them as regular season; their
  # "teams" play <= 3 games a season) and forfeits recorded with no statistics (0 attempts).
  group_by(league, season, team) |> filter(n() >= 8) |> ungroup() |>
  group_by(league, game_id) |> filter(n() == 2, all(p2a + p3a > 0)) |> ungroup() |>
  add_opponent() |>
  compute_metrics() |>
  mutate(
    # NEW: possessions per 40 minutes, a game-length-adjusted pace
    pace40 = np * 40 / game_min,
    team_season = paste(league, season, team, sep = "_"),
    opp_season  = paste(league, season, opp, sep = "_")
  )

dir.create(PROCESSED, showWarnings = FALSE)
write_parquet(team_games, file.path(PROCESSED, "team_games.parquet"))

team_games |>
  count(league, season, phase) |>
  tidyr::pivot_wider(names_from = phase, values_from = n) |>
  print(n = 50)
