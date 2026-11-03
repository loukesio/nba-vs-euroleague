# RQ3 step 1 — How does play change in the playoffs? 2010/11-2025/26.
#
# Matched design: only team-seasons that reached the playoffs, using their regular-season AND
# playoff games. Each team is compared with itself, so "playoff teams are better" cannot
# explain the difference. One brms model per metric, on the standardised metric z:
#
#   z ~ 0 + league + league:playoff + (1 | team_season) + (1 | opp_season),  sigma ~ 0 + league
#
#   * league:playoff   -> the playoff shift in each league (THE quantity of interest)
#   * (1 | team_season) -> each team-season's own baseline
#   * (1 | opp_season)  -> opponent strength (playoff opponents are better than average)
#
# "Does the NBA become more European in the playoffs?" (the 2019 paper's claim):
#   toward_EL = NBA playoff shift / (EuroLeague RS level - NBA RS level)
#   > 0 means the shift points towards the EuroLeague; 1 would close the whole gap.

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
source(here::here("R", "bayes.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(tidyr); library(purrr) })

rq_dir <- here::here("RQ3_playoffs")
model_dir <- file.path(rq_dir, "data", "models")

tg <- load_team_games(SEASONS)
po_teams <- tg |> filter(phase == "playoffs") |> distinct(team_season)
matched <- tg |> semi_join(po_teams, by = "team_season") |>
  mutate(playoff = as.integer(phase == "playoffs"))

write.csv(matched |> count(league, phase, name = "team_games"),
          file.path(rq_dir, "results", "tables", "matched_sample_size.csv"), row.names = FALSE)

priors <- c(prior(normal(0, 1.5), class = "b"),
            prior(normal(0, 1), class = "sd"),
            prior(normal(0, 1), class = "b", dpar = "sigma"))
form <- bf(z ~ 0 + league + league:playoff + (1 | team_season) + (1 | opp_season),
           sigma ~ 0 + league)

# Turnover rate (tor) is turnovers per 100 possessions / 100, the same quantity: fit it once.
RQ3_METRICS <- setdiff(METRICS$metric, "tor")

prep <- function(metric) {
  d <- matched |> filter(is.finite(.data[[metric]]))
  m <- mean(d[[metric]]); s <- sd(d[[metric]])
  list(data = d |> mutate(z = (.data[[metric]] - m) / s) |>
         select(z, league, playoff, team_season, opp_season),
       m = m, s = s)
}

first <- prep(RQ3_METRICS[1])
base_fit <- fit_cached(form, first$data, prior = priors,
                       file = file.path(model_dir, paste0("po_", RQ3_METRICS[1])))

results <- map_dfr(RQ3_METRICS, function(metric) {
  p <- prep(metric)
  path <- file.path(model_dir, paste0("po_", metric))
  # reuse a saved fit if there is one (delete the .rds to force a refit)
  long <- paste0(path, "_long")
  fit <- if (file.exists(paste0(long, ".rds"))) readRDS(paste0(long, ".rds")) else
    if (file.exists(paste0(path, ".rds"))) readRDS(paste0(path, ".rds")) else
    refit(base_fit, p$data, "po", file = path)
  # Weak convergence (divergences, R-hat > 1.03 or tail ESS < 200): refit with longer chains
  # and smaller steps. FT%, 3PA share, 2PA and 3PA per 100 needed this in the first run.
  if (with(diagnostics(fit), divergences > 0 || max_rhat > 1.03 || min_ess_tail < 200)) {
    message("refitting ", metric, " with longer chains")
    fit <- refit(base_fit, p$data, "po_long", file = long,
                 iter = 4000, warmup = 1500, control = list(adapt_delta = 0.99))
  }
  dr <- as_draws_df(fit)
  rs_nba <- p$m + p$s * dr$b_leagueNBA
  rs_el  <- p$m + p$s * dr$b_leagueEuroLeague
  po_nba <- p$s * dr$`b_leagueNBA:playoff`
  po_el  <- p$s * dr$`b_leagueEuroLeague:playoff`
  toward <- po_nba / (rs_el - rs_nba)
  tibble(metric = metric,
         rs_nba = mean(rs_nba), rs_el = mean(rs_el),
         po_shift_nba = mean(po_nba), nba_lo = quantile(po_nba, .025), nba_hi = quantile(po_nba, .975),
         p_nba_up = mean(po_nba > 0),
         po_shift_el = mean(po_el), el_lo = quantile(po_el, .025), el_hi = quantile(po_el, .975),
         p_el_up = mean(po_el > 0),
         # standardised shifts (in SDs of the metric) for a common-scale forest plot
         es_nba = mean(dr$`b_leagueNBA:playoff`), es_nba_lo = quantile(dr$`b_leagueNBA:playoff`, .025),
         es_nba_hi = quantile(dr$`b_leagueNBA:playoff`, .975),
         es_el = mean(dr$`b_leagueEuroLeague:playoff`), es_el_lo = quantile(dr$`b_leagueEuroLeague:playoff`, .025),
         es_el_hi = quantile(dr$`b_leagueEuroLeague:playoff`, .975),
         p_nba_toward_el = mean(toward > 0),
         toward_el_median = median(toward)) |>
    bind_cols(diagnostics(fit))
}) |>
  left_join(METRICS, by = "metric")

write.csv(results, file.path(rq_dir, "results", "tables", "playoff_shifts.csv"), row.names = FALSE)
print(results |> select(label, po_shift_nba, p_nba_up, po_shift_el, p_el_up, p_nba_toward_el, max_rhat),
      n = 30)
