# Loaders for the processed tables (built by 00_data/scripts/03_* and 04_*).

load_team_games <- function(seasons = SEASONS, leagues = c("NBA", "EuroLeague")) {
  arrow::read_parquet(file.path(PROCESSED, "team_games.parquet")) |>
    dplyr::filter(season %in% seasons, league %in% leagues) |>
    dplyr::mutate(league = factor(league, levels = c("NBA", "EuroLeague")),
                  phase  = factor(phase, levels = c("regular", "playoffs")))
}

load_shots <- function(seasons = SEASONS) {
  arrow::read_parquet(file.path(PROCESSED, "shots.parquet")) |>
    dplyr::filter(season %in% seasons) |>
    dplyr::mutate(league = factor(league, levels = c("NBA", "EuroLeague")),
                  phase  = factor(phase, levels = c("regular", "playoffs")))
}

# Season means (and their standard errors) of one metric, per league.
season_means <- function(tg, metric) {
  tg |>
    dplyr::filter(is.finite(.data[[metric]])) |>
    dplyr::group_by(league, season) |>
    dplyr::summarise(n = dplyr::n(), mean = mean(.data[[metric]]),
                     se = stats::sd(.data[[metric]]) / sqrt(n), .groups = "drop")
}
