# RQ3 step 2 — Do shot locations change in the playoffs? Matched team-seasons, 2010/11-2025/26.
#
# Counts per team-season x phase x zone; one binomial model per zone (brms):
#   n_zone | trials(n_total) ~ 0 + league + league:playoff + (1 | team_season)
# The playoff shift is reported in percentage points of shot share for a typical team.

source(here::here("R", "config.R"))
source(here::here("R", "court.R"))
source(here::here("R", "bayes.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(tidyr); library(purrr) })

rq_dir <- here::here("RQ3_playoffs")
model_dir <- file.path(rq_dir, "data", "models")

shots <- load_shots(SEASONS) |> mutate(team_season = paste(league, season, team, sep = "_"))
po_teams <- shots |> filter(phase == "playoffs") |> distinct(team_season)

counts <- shots |>
  semi_join(po_teams, by = "team_season") |>
  count(league, team_season, phase, zone, name = "n_zone") |>
  complete(nesting(league, team_season, phase), zone, fill = list(n_zone = 0)) |>
  group_by(team_season, phase) |> mutate(n_total = sum(n_zone)) |> ungroup() |>
  mutate(playoff = as.integer(phase == "playoffs"))
write.csv(counts, file.path(rq_dir, "data", "zone_counts_matched.csv"), row.names = FALSE)

base <- NULL
res <- map_dfr(ZONES, function(z) {
  d <- counts |> filter(zone == z)
  if (is.null(base)) base <<- fit_cached(
    bf(n_zone | trials(n_total) ~ 0 + league + league:playoff + (1 | team_season)),
    d, family = binomial(),
    prior = c(prior(normal(0, 2), class = "b"), prior(normal(0, 1), class = "sd")),
    file = file.path(model_dir, "po_zone_base"))
  fit <- refit(base, d, "po_zone", iter = 4000, warmup = 1000)
  dr <- as_draws_df(fit)
  map_dfr(c("NBA", "EuroLeague"), function(lg) {
    rs <- plogis(dr[[paste0("b_league", lg)]])
    po <- plogis(dr[[paste0("b_league", lg)]] + dr[[paste0("b_league", lg, ":playoff")]])
    tibble(zone = z, league = lg, rs_share = mean(rs), po_share = mean(po),
           shift_pp = mean(100 * (po - rs)), lo = quantile(100 * (po - rs), .025),
           hi = quantile(100 * (po - rs), .975), p_up = mean(po > rs))
  }) |> bind_cols(diagnostics(fit))
})

write.csv(res, file.path(rq_dir, "results", "tables", "playoff_zone_shifts.csv"), row.names = FALSE)
print(res, n = 20)
