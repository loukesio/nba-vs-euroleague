# RQ2 step 3a — Does one league follow the other? (the time gap for today is in 03b)
#
# (a) Shift model (models/lead_lag.stan): the EuroLeague's season means are modelled as the
#     NBA's smooth trend shifted by k seasons (k = -K..K), with free level/scale (a, b).
#     Output: posterior probability of each lag k. Uses regular-season box scores 2001/02-2025/26.
# (b) "Time gap" for today: in which season did the NBA reach the EuroLeague's 2025/26 level?
#     Computed from the smooth NBA curve (RW2) draws. Only for metrics defined identically in
#     both leagues (per-possession or per-40-minute; not raw per-game counts).
#
# Both are descriptive: with ~25 seasons a lag pattern is a correlation in time, not proof
# that one league copies the other.

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

# ---- (a) shift model -------------------------------------------------------------------
fit_lag <- function(metric) {
  sm <- season_means(tg, metric)
  nba <- sm |> filter(league == "NBA") |> arrange(season)
  el  <- sm |> filter(league == "EuroLeague") |> arrange(season)
  stopifnot(identical(nba$season, SEASONS_LONG))
  # EuroLeague seasons usable for every lag: t - k inside the NBA range for all |k| <= K
  el <- el |> mutate(idx = match(season, nba$season)) |>
    filter(idx > K, idx <= nrow(nba) - K)
  zN <- \(x) (x - mean(nba$mean)) / sd(nba$mean)
  zE <- \(x) (x - mean(el$mean)) / sd(el$mean)
  fit <- stan_model_cached("lead_lag")$sample(
    data = list(T = nrow(nba), yN = zN(nba$mean), seN = nba$se / sd(nba$mean),
                K = K, TE = nrow(el), e_idx = el$idx, yE = zE(el$mean), seE = el$se / sd(el$mean)),
    chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000,
    seed = 2026, refresh = 0, show_messages = FALSE, adapt_delta = 0.95
  )
  pk <- colMeans(posterior::as_draws_matrix(fit$draws("p_k")))
  b <- as.numeric(fit$draws("b"))
  tibble(metric = metric, k = -K:K, p = pk) |>
    mutate(b_mean = mean(b), b_lo = quantile(b, 0.025),
           el_seasons = sprintf("%d-%d", min(el$season), max(el$season)),
           max_rhat = max(fit$summary(c("a", "b", "omega", "tauN", "tauE", "p_k"))$rhat, na.rm = TRUE),
           divergences = sum(fit$diagnostic_summary(quiet = TRUE)$num_divergent))
}

lags <- map_dfr(LAG_METRICS, fit_lag) |> left_join(METRICS, by = "metric")
lag_summary <- lags |>
  group_by(metric, label, group, b_mean, b_lo, el_seasons, max_rhat, divergences) |>
  summarise(k_mode = k[which.max(p)], p_mode = max(p),
            p_el_follows = sum(p[k > 0]), p_nba_follows = sum(p[k < 0]), p_sync = p[k == 0],
            .groups = "drop")

write.csv(lags, file.path(rq_dir, "results", "tables", "lead_lag_posterior.csv"), row.names = FALSE)
write.csv(lag_summary, file.path(rq_dir, "results", "tables", "lead_lag_summary.csv"), row.names = FALSE)

print(lag_summary |> select(label, k_mode, p_mode, p_el_follows, p_nba_follows, b_mean), n = 30)
