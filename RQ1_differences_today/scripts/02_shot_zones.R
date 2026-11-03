# RQ1 step 2 — Shot profiles by zone, 2023/24-2025/26, regular season.
#
# Data are aggregated to one row per team-season x zone, then two Bayesian binomial models
# per zone (brms):
#   share:     n_zone | trials(n_total) ~ 0 + league + (1 | team_season)
#   accuracy:  made   | trials(n_zone)  ~ 0 + league + (1 | team_season)
# The random effect lets teams differ (over-dispersion); the league coefficient on the logit
# scale is the "typical team". Points per shot (PPS) = shot value x accuracy.
# A descriptive distance profile (share and FG% by 25 cm band) is saved for plotting.

source(here::here("R", "config.R"))
source(here::here("R", "court.R"))
source(here::here("R", "bayes.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(tidyr); library(purrr) })

rq_dir <- here::here("RQ1_differences_today")
model_dir <- file.path(rq_dir, "data", "models")

shots <- load_shots(TODAY_SEASONS) |> filter(phase == "regular") |>
  mutate(team_season = paste(league, season, team, sep = "_"))

zone_counts <- shots |>
  count(league, team_season, zone, name = "n_zone") |>
  complete(nesting(league, team_season), zone, fill = list(n_zone = 0)) |>
  left_join(shots |> group_by(team_season, zone) |> summarise(made = sum(made), .groups = "drop"),
            by = c("team_season", "zone")) |>
  mutate(made = coalesce(made, 0L)) |>
  group_by(team_season) |> mutate(n_total = sum(n_zone)) |> ungroup()
write.csv(zone_counts, file.path(rq_dir, "data", "zone_counts_today.csv"), row.names = FALSE)

priors <- c(prior(normal(0, 2), class = "b"), prior(normal(0, 1), class = "sd"))
inv_logit <- plogis

share_base <- NULL; acc_base <- NULL
zone_results <- map_dfr(ZONES, function(z) {
  d <- zone_counts |> filter(zone == z)
  if (is.null(share_base)) {
    share_base <<- fit_cached(bf(n_zone | trials(n_total) ~ 0 + league + (1 | team_season)),
                              d, family = binomial(), prior = priors,
                              file = file.path(model_dir, "zone_share_base"))
    acc_base <<- fit_cached(bf(made | trials(n_zone) ~ 0 + league + (1 | team_season)),
                            d, family = binomial(), prior = priors,
                            file = file.path(model_dir, "zone_acc_base"))
  }
  fs <- refit(share_base, d, "zone_share", iter = 4000, warmup = 1000)
  fa <- refit(acc_base, d |> filter(n_zone > 0), "zone_acc", iter = 4000, warmup = 1000)
  ds <- as_draws_df(fs); da <- as_draws_df(fa)
  value <- if (grepl("3", z)) 3 else 2
  out <- map_dfr(c("NBA", "EuroLeague"), function(lg) {
    sh <- inv_logit(ds[[paste0("b_league", lg)]])
    ac <- inv_logit(da[[paste0("b_league", lg)]])
    tibble(zone = z, league = lg,
           share = mean(sh), share_lo = quantile(sh, .025), share_hi = quantile(sh, .975),
           fg = mean(ac), fg_lo = quantile(ac, .025), fg_hi = quantile(ac, .975),
           pps = mean(value * ac), pps_lo = quantile(value * ac, .025), pps_hi = quantile(value * ac, .975))
  })
  diff_share <- inv_logit(ds$b_leagueNBA) - inv_logit(ds$b_leagueEuroLeague)
  diff_pps <- value * (inv_logit(da$b_leagueNBA) - inv_logit(da$b_leagueEuroLeague))
  out |> mutate(p_nba_share_higher = mean(diff_share > 0), p_nba_pps_higher = mean(diff_pps > 0),
                max_rhat = max(diagnostics(fs)$max_rhat, diagnostics(fa)$max_rhat),
                divergences = diagnostics(fs)$divergences + diagnostics(fa)$divergences)
})

write.csv(zone_results, file.path(rq_dir, "results", "tables", "shot_zones_today.csv"), row.names = FALSE)

# Descriptive distance profile (counts are large, so plain binomial CIs are adequate here)
dist_profile <- shots |>
  filter(dist < 9) |>
  mutate(band = floor(dist / 0.25) * 0.25 + 0.125) |>
  group_by(league, band) |>
  summarise(n = n(), made = sum(made), three_share = mean(value == 3), .groups = "drop") |>
  group_by(league) |>
  mutate(share = n / sum(n), fg = made / n, fg_se = sqrt(fg * (1 - fg) / n)) |>
  ungroup()
write.csv(dist_profile, file.path(rq_dir, "results", "tables", "distance_profile_today.csv"), row.names = FALSE)

print(zone_results |> select(zone, league, share, fg, pps, p_nba_share_higher, max_rhat), n = 20)
