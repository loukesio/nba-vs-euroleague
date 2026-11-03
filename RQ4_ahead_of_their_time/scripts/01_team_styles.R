# RQ4 step 1 — Teams: who embodied each season, who played differently, and who was ahead of their time?
#
# Each team-season (regular season, 2010/11-2025/26) gets a STYLE PROFILE of 14 numbers:
#   pace, shot mix (5 zones), FTA, assists, turnovers, offensive rebound rate, blocks, steals,
#   fouls, and the spread of minutes (rotation).
# Profiles are posteriors, not raw averages (see R/style.R): each team is shrunk a little towards its
# league-season average, and 400 posterior draws carry the uncertainty into every result below.
# All 14 numbers are put on one scale: units of "how much teams differ within a season".
#
#   Typical:   distance from the team to its league-season average. P(most typical) = share of
#              draws in which the team is the closest of all teams that season.
#   Different: P(most different) = share of draws in which the team is the furthest away.
#   Ahead:     the league moves through style space over the years (its "path"). A team's TIME
#              POSITION is the season on that path it is closest to. Played 2014/15, closest to the
#              league of 2019/20 -> 5 seasons ahead. The path ends in 2025/26, so recent teams cannot
#              be far ahead (results are capped there).
#   Cross-league: the same, but placing EuroLeague teams on the NBA's path and vice versa, using the
#              shot mix only (rim, mid-range, three), where the leagues are directly comparable.

source(here::here("R", "config.R"))
source(here::here("R", "court.R"))
source(here::here("R", "data.R"))
source(here::here("R", "style.R"))
suppressMessages({ library(tidyr); library(purrr) })

set.seed(2026)
S <- 400
rq_dir <- here::here("RQ4_ahead_of_their_time")
tab_dir <- file.path(rq_dir, "results", "tables")

GAME_FEATURES <- c(pace40 = "Pace", fta_100 = "FTA", ast_100 = "Assists", tov_100 = "Turnovers",
                   orb_rate = "Off. rebounds", blk_100 = "Blocks", stl_100 = "Steals",
                   fcm_100 = "Fouls", entropy_min = "Rotation depth")
ZONE_FEATURES <- c(rim = "Rim", short_mid = "Short mid", long_mid = "Long mid",
                   corner3 = "Corner 3", above3 = "Above-break 3")
FEATURES <- c(GAME_FEATURES, ZONE_FEATURES)

# ---- data ----------------------------------------------------------------------------
tg <- load_team_games(SEASONS) |> filter(phase == "regular")
game_means <- tg |>
  group_by(league, season, team) |>
  summarise(games = n(), wins = sum(win),
            across(all_of(names(GAME_FEATURES)), list(m = \(x) mean(x, na.rm = TRUE),
                                                      se = \(x) sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x))))),
            .groups = "drop")

zone_key <- setNames(names(ZONE_FEATURES), ZONES)
zones <- load_shots(SEASONS) |>
  filter(phase == "regular") |>
  mutate(team = if_else(league == "NBA" & team %in% names(NBA_TEAM_MAP), NBA_TEAM_MAP[team], team),
         zone = zone_key[as.character(zone)]) |>
  count(league, season, team, zone) |>
  pivot_wider(names_from = zone, values_from = n, values_fill = 0)

teams <- game_means |>
  inner_join(zones, by = c("league", "season", "team")) |>
  mutate(league = as.character(league), group = paste(league, season)) |>
  arrange(league, season, team)
message(nrow(teams), " team-seasons (", nrow(game_means) - nrow(teams), " without shot data dropped)")

# ---- posterior draws: X[S, n, F] on the natural scale ----------------------------------
n <- nrow(teams)
X <- array(NA_real_, c(S, n, length(FEATURES)), dimnames = list(NULL, NULL, names(FEATURES)))
for (f in names(GAME_FEATURES)) {
  X[, , f] <- rshrunk_normal(S, teams[[paste0(f, "_m")]], teams[[paste0(f, "_se")]], teams$group)
}
counts <- as.matrix(teams[, names(ZONE_FEATURES)])
prior_m <- teams |>
  group_by(group) |>
  mutate(across(all_of(names(ZONE_FEATURES)), sum)) |>
  ungroup() |>
  select(all_of(names(ZONE_FEATURES))) |>
  as.matrix()
prior_m <- prior_m / rowSums(prior_m)
alpha <- sapply(split(seq_len(n), teams$league), \(i) dm_alpha(counts[i, ], prior_m[i, ]))
message("Dirichlet prior strength (in shots): ", paste(names(alpha), round(alpha), collapse = ", "))
X[, , names(ZONE_FEATURES)] <- rdirichlet_rows(S, alpha[teams$league] * prior_m + counts)

# ---- one common scale per league: the typical spread between teams within a season ------
Xbar <- apply(X, c(2, 3), mean)
scale_tbl <- as_tibble(Xbar) |>
  mutate(league = teams$league, group = teams$group) |>
  pivot_longer(all_of(names(FEATURES)), names_to = "feature") |>
  group_by(league, feature) |>
  summarise(centre = mean(value),
            # pooled within-season SD
            scale = sqrt(sum((value - ave(value, group))^2) / (n() - n_distinct(group))),
            .groups = "drop")
standardise <- function(X, leagues, sc = scale_tbl) {
  Z <- X
  for (f in dimnames(X)[[3]]) {
    s <- sc |> filter(feature == f)
    ctr <- s$centre[match(leagues, s$league)]; scl <- s$scale[match(leagues, s$league)]
    Z[, , f] <- (X[, , f] - rep(ctr, each = dim(X)[1])) / rep(scl, each = dim(X)[1])
  }
  Z
}
Z <- standardise(X, teams$league)
Zbar <- apply(Z, c(2, 3), mean)

# ---- typical and different ---------------------------------------------------------------
centres <- as_tibble(Zbar) |> mutate(group = teams$group) |>
  group_by(group) |> summarise(across(everything(), mean))
C <- as.matrix(centres[match(teams$group, centres$group), names(FEATURES)])
D <- dist_to_centre(Z, C)

top_features <- function(dev, k = 3) {
  apply(dev, 1, function(r) {
    o <- order(-abs(r))[1:k]
    paste0(FEATURES[names(r)[o]], ifelse(r[o] > 0, " +", " -"), collapse = ", ")
  })
}
typicality <- teams |>
  select(league, season, team, games, wins) |>
  mutate(distance = colMeans(D), dist_lo = apply(D, 2, quantile, .05),
         dist_hi = apply(D, 2, quantile, .95),
         stands_out_for = top_features(Zbar - C)) |>
  bind_cols(extreme_probs(D, teams$group)) |>
  group_by(league, season) |>
  mutate(rank_typical = rank(distance)) |>
  ungroup()
write.csv(typicality, file.path(tab_dir, "team_typicality.csv"), row.names = FALSE)

# deviations per feature (posterior mean, standardised) for the profile figures
write.csv(bind_cols(teams |> select(league, season, team), as_tibble(Zbar - C)),
          file.path(tab_dir, "team_profile_deviations.csv"), row.names = FALSE)

# ---- the league's path, and each team's time position ---------------------------------------
paths <- list()
time_pos <- map_dfr(c("NBA", "EuroLeague"), function(lg) {
  idx <- which(teams$league == lg)
  means <- centres |> filter(startsWith(group, lg)) |>
    mutate(season = as.integer(sub(".* ", "", group)))
  P <- league_path(means, names(FEATURES))
  paths[[lg]] <<- P
  map_dfr(idx, function(i) {
    tp <- time_position(Z[, i, ], P, teams$season[i])
    ahead <- tp$tau - teams$season[i]
    tibble(league = lg, season = teams$season[i], team = teams$team[i],
           tau = median(tp$tau), tau_lo = quantile(tp$tau, .05), tau_hi = quantile(tp$tau, .95),
           years_ahead = median(ahead), p_ahead_2 = mean(ahead >= 2), p_behind_2 = mean(ahead <= -2),
           off_path = median(tp$off), gain = median(tp$gain))
  })
})
write.csv(time_pos, file.path(tab_dir, "team_time_position.csv"), row.names = FALSE)

path_tbl <- imap_dfr(paths, \(P, lg) as_tibble(P) |> mutate(league = lg, season = attr(P, "season")))
write.csv(path_tbl, file.path(tab_dir, "league_paths_teams.csv"), row.names = FALSE)

# ---- cross-league: shot mix only ------------------------------------------------------------
# rim / mid-range / three shares, on the target league's scale
mix <- function(X) {
  out <- array(NA_real_, c(dim(X)[1], dim(X)[2], 3), dimnames = list(NULL, NULL, c("rim", "mid", "three")))
  out[, , "rim"] <- X[, , "rim"]
  out[, , "mid"] <- X[, , "short_mid"] + X[, , "long_mid"]
  out[, , "three"] <- X[, , "corner3"] + X[, , "above3"]
  out
}
M <- mix(X)
cross <- map_dfr(c("NBA", "EuroLeague"), function(target) {
  tgt <- which(teams$league == target)
  Mbar <- apply(M[, tgt, ], c(2, 3), mean)
  ctr <- colMeans(Mbar)
  scl <- apply(Mbar, 2, \(v) sqrt(sum((v - ave(v, teams$group[tgt]))^2) / (length(v) - n_distinct(teams$group[tgt]))))
  sz <- \(A) sweep(sweep(A, 2, ctr), 2, scl, "/")
  means <- as_tibble(Mbar) |> mutate(season = teams$season[tgt]) |>
    group_by(season) |> summarise(across(everything(), mean))
  means[, 2:4] <- sz(as.matrix(means[, 2:4]))
  P <- league_path(means, c("rim", "mid", "three"))
  src <- which(teams$league != target)
  map_dfr(src, function(i) {
    tp <- time_position(sz(M[, i, ]), P, teams$season[i])
    tibble(league = teams$league[i], on_path_of = target, season = teams$season[i], team = teams$team[i],
           tau = median(tp$tau), tau_lo = quantile(tp$tau, .05), tau_hi = quantile(tp$tau, .95),
           off_path = median(tp$off), gain = median(tp$gain))
  })
})
write.csv(cross, file.path(tab_dir, "team_cross_league_time.csv"), row.names = FALSE)

# ---- print the headlines -------------------------------------------------------------------
cat("\nMost typical team per season\n")
typicality |> group_by(league, season) |> slice_max(p_most_typical, n = 1, with_ties = FALSE) |>
  select(league, season, team, p_most_typical, distance) |> print(n = 40)
cat("\nMost different team per season\n")
typicality |> group_by(league, season) |> slice_max(p_most_different, n = 1, with_ties = FALSE) |>
  select(league, season, team, p_most_different, distance, stands_out_for) |> print(n = 40)
cat("\nFurthest ahead of their time\n")
time_pos |> group_by(league) |> slice_max(years_ahead, n = 8, with_ties = FALSE) |>
  select(league, season, team, tau, tau_lo, tau_hi, years_ahead, p_ahead_2, gain) |> print(n = 20)
cat("\nFurthest behind their time\n")
time_pos |> group_by(league) |> slice_min(years_ahead, n = 5, with_ties = FALSE) |>
  select(league, season, team, tau, tau_lo, tau_hi, years_ahead, p_behind_2, gain) |> print(n = 20)
cat("\nCross-league, median gap (tau - season)\n")
cross |> group_by(league, on_path_of) |> summarise(median_gap = median(tau - season), .groups = "drop") |> print()
