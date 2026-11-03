# RQ1 step 1 — How do the leagues differ today? Box-score metrics, 2023/24-2025/26, regular season.
#
# One Bayesian mixed model per metric (brms), on the standardised metric z:
#   z ~ 0 + league + (1 | team_season) + (1 | opp_season),  sigma ~ 0 + league
#   * league               -> the average team in each league
#   * (1 | team_season)    -> teams differ in style/quality; games of one team-season are correlated
#   * (1 | opp_season)     -> who you play against matters too
#   * sigma ~ 0 + league   -> game-to-game variability may differ between leagues
# Priors (on the z scale): league means ~ N(0, 1.5), SDs ~ half-N(0, 1).
# The model compiles once; other metrics reuse it via update(), which only swaps the data.

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
source(here::here("R", "bayes.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(tidyr); library(purrr) })

rq_dir <- here::here("RQ1_differences_today")
model_dir <- file.path(rq_dir, "data", "models")

tg <- load_team_games(TODAY_SEASONS) |> filter(phase == "regular")

priors <- c(prior(normal(0, 1.5), class = "b"),
            prior(normal(0, 1), class = "sd"),
            prior(normal(0, 1), class = "b", dpar = "sigma"))

prep <- function(metric) {
  d <- tg |> filter(is.finite(.data[[metric]]))
  m <- mean(d[[metric]]); s <- sd(d[[metric]])
  list(data = d |> mutate(z = (.data[[metric]] - m) / s) |>
         select(z, league, team_season, opp_season),
       m = m, s = s)
}

form <- bf(z ~ 0 + league + (1 | team_season) + (1 | opp_season), sigma ~ 0 + league)

first <- prep(METRICS$metric[1])
base_fit <- fit_cached(form, first$data, prior = priors,
                       file = file.path(model_dir, paste0("box_", METRICS$metric[1])))

results <- map_dfr(METRICS$metric, function(metric) {
  p <- prep(metric)
  path <- file.path(model_dir, paste0("box_", metric))
  # reuse a saved fit if there is one (delete the .rds to force a refit)
  fit <- if (file.exists(paste0(path, ".rds"))) readRDS(paste0(path, ".rds")) else
    refit(base_fit, p$data, "box", file = path)
  dr <- as_draws_df(fit)
  # posterior draws (named d_* so tibble() below cannot overwrite them with their means)
  d_nba <- p$m + p$s * dr$b_leagueNBA
  d_el  <- p$m + p$s * dr$b_leagueEuroLeague
  d_diff <- d_nba - d_el
  # within-league game-to-game SD (pooled) to express the gap as a standardised effect size
  sd_within <- p$s * sqrt((exp(dr$b_sigma_leagueNBA)^2 + exp(dr$b_sigma_leagueEuroLeague)^2) / 2)
  d_es <- d_diff / sd_within
  tibble(metric = metric,
         nba = mean(d_nba), nba_lo = quantile(d_nba, .025), nba_hi = quantile(d_nba, .975),
         el = mean(d_el), el_lo = quantile(d_el, .025), el_hi = quantile(d_el, .975),
         diff = mean(d_diff), diff_lo = quantile(d_diff, .025), diff_hi = quantile(d_diff, .975),
         p_nba_higher = mean(d_diff > 0),
         effect_size = mean(d_es), es_lo = quantile(d_es, .025), es_hi = quantile(d_es, .975)) |>
    bind_cols(diagnostics(fit))
}) |>
  left_join(METRICS, by = "metric")

write.csv(results, file.path(rq_dir, "results", "tables", "box_differences_today.csv"), row.names = FALSE)
print(results |> select(label, nba, el, diff, p_nba_higher, effect_size, max_rhat, divergences), n = 30)
