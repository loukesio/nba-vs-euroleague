# RQ2 step 3b — "The EuroLeague today looks like the NBA of season X."
# For each posterior draw of the smooth NBA curve (RW2, 2001/02-2025/26), find the season in
# which the NBA first reached the EuroLeague's 2025/26 level. Only for metrics defined the same
# way in both leagues. Descriptive, like the shift model in 03a.

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
source(here::here("R", "bayes.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(tidyr); library(purrr) })

rq_dir <- here::here("RQ2_trends_leadlag")
K <- 5
tg <- load_team_games(SEASONS_LONG) |> filter(phase == "regular") |>
  # NBA player minutes (hence overtime detection) exist only from 2010/11, so for this long
  # series pace is put on a per-40-minute basis using REGULATION length (48 NBA, 40 EuroLeague)
  # in every season. Overtime games (~5%) inflate it slightly, equally in all seasons.
  mutate(pace40 = np * 40 / if_else(league == "NBA", 48, 40))

# Metrics comparable across leagues: np is per game (game lengths differ) and minutes entropy
# is unavailable for the NBA before 2010/11, so both are left out.
LAG_METRICS <- setdiff(METRICS$metric, c("np", "entropy_min", "tor"))

# ---- (b) time gap for today --------------------------------------------------------------
# For each posterior draw of the NBA curve, find the (interpolated) season in which the NBA
# curve first reached the EuroLeague's 2025/26 level. NA if the NBA never reached it.
crossing_season <- function(f_nba, seasons, target) {
  above <- f_nba >= target
  if (all(above) || !any(above)) {
    # the NBA was on one side of the target for the whole period
    return(NA_real_)
  }
  # direction-agnostic: first index where the curve crosses the target
  s <- sign(f_nba - target)
  i <- which(diff(s) != 0)[1]
  seasons[i] + (target - f_nba[i]) / (f_nba[i + 1] - f_nba[i])
}

time_gap <- map_dfr(LAG_METRICS, function(metric) {
  sm <- season_means(tg, metric)
  fN <- fit_season_curve(sm |> filter(league == "NBA"))
  fE <- fit_season_curve(sm |> filter(league == "EuroLeague"))
  today <- as.numeric(fE[, ncol(fE)])
  # as.numeric(): one row of a draws matrix is still a 1 x T matrix, and diff() would misbehave
  cross <- map_dbl(seq_len(nrow(fN)), \(i) crossing_season(as.numeric(fN[i, ]), SEASONS_LONG, today[i]))
  tibble(metric = metric,
         el_today = mean(today),
         nba_today = mean(fN[, ncol(fN)]),
         p_reached = mean(!is.na(cross)),
         nba_season_equiv = median(cross, na.rm = TRUE),
         lo = quantile(cross, 0.05, na.rm = TRUE), hi = quantile(cross, 0.95, na.rm = TRUE))
}) |>
  mutate(gap_years = 2025 - nba_season_equiv) |>
  left_join(METRICS, by = "metric")

write.csv(time_gap, file.path(rq_dir, "results", "tables", "time_gap_today.csv"), row.names = FALSE)

print(time_gap |> select(label, el_today, nba_today, p_reached, nba_season_equiv, gap_years), n = 30)
