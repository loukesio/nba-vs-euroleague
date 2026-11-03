# RQ4 step 2 — Players: the league in one player, the most unusual players, and who was ahead of their time.
#
# Rotation players only (NBA >= 800 minutes and >= 150 located shots; EuroLeague >= 200 minutes and
# >= 50 located shots, since its seasons are much shorter). Each player-season gets a STYLE PROFILE:
#   shot mix (5 zones) + per 100 possessions: assists, turnovers, off./def. rebounds, blocks, steals,
#   fouls + free-throw attempts per field-goal attempt.
# Small samples are shrunk towards the league-season average for the player's position
# (Dirichlet-multinomial for the shot mix, gamma-Poisson for the rates; see R/style.R).
#
# A centre and a guard can never look alike, so everything is relative to POSITION (G / F / C):
#   The league in one player: closest to the league's overall average profile that season.
#   Most typical per position: closest to the average guard / forward / centre that season.
#   Most unusual: furthest from the average player of their position.
#   Ahead of their time: time position on the path of their own position (e.g. a 2014 centre who
#     plays like the average centre of 2021 is 7 seasons ahead).

source(here::here("R", "config.R"))
source(here::here("R", "style.R"))
suppressMessages({ library(tidyr); library(purrr); library(arrow) })

set.seed(2026)
S <- 300
rq_dir <- here::here("RQ4_ahead_of_their_time")
tab_dir <- file.path(rq_dir, "results", "tables")

ZONE_FEATURES <- c(n_rim = "Rim", n_short_mid = "Short mid", n_long_mid = "Long mid",
                   n_corner3 = "Corner 3", n_above3 = "Above-break 3")
# rate features: count column, exposure column
RATE_FEATURES <- tribble(
  ~feature, ~label,           ~count, ~exposure,
  "ast",    "Assists",        "ast",  "poss100",
  "tov",    "Turnovers",      "tov",  "poss100",
  "orb",    "Off. rebounds",  "orb",  "poss100",
  "drb",    "Def. rebounds",  "drb",  "poss100",
  "blk",    "Blocks",         "blk",  "poss100",
  "stl",    "Steals",         "stl",  "poss100",
  "pf",     "Fouls",          "pf",   "poss100",
  "ftr",    "FTA per FGA",    "fta",  "fga"
)
FEATURES <- c(ZONE_FEATURES, setNames(RATE_FEATURES$label, RATE_FEATURES$feature))
MIN_MINUTES <- c(NBA = 800, EuroLeague = 200)
MIN_SHOTS   <- c(NBA = 150, EuroLeague = 50)

players_all <- read_parquet(file.path(PROCESSED, "player_seasons.parquet")) |>
  mutate(poss100 = poss / 100)
players <- players_all |>
  filter(!is.na(position), minutes >= MIN_MINUTES[league], shots >= MIN_SHOTS[league]) |>
  mutate(group = paste(league, season, position)) |>
  arrange(league, season, position, player)
message(nrow(players), " rotation player-seasons")

# ---- one league at a time ---------------------------------------------------------------
analyse_league <- function(lg) {
  p <- players |> filter(league == lg)
  n <- nrow(p)
  X <- array(NA_real_, c(S, n, length(FEATURES)), dimnames = list(NULL, NULL, names(FEATURES)))

  # shot mix: Dirichlet-multinomial, prior centred on the league-season-position shot mix
  counts <- as.matrix(p[, names(ZONE_FEATURES)])
  m <- p |> group_by(group) |> mutate(across(all_of(names(ZONE_FEATURES)), sum)) |> ungroup() |>
    select(all_of(names(ZONE_FEATURES))) |> as.matrix()
  m <- m / rowSums(m)
  alpha <- sapply(split(seq_len(n), p$position), \(i) dm_alpha(counts[i, ], m[i, ]))
  X[, , names(ZONE_FEATURES)] <- rdirichlet_rows(S, alpha[p$position] * m + counts)

  # rates: gamma-Poisson, prior centred on the league-season-position rate
  kappas <- list()
  for (r in seq_len(nrow(RATE_FEATURES))) {
    f <- RATE_FEATURES[r, ]
    y <- p[[f$count]]; E <- p[[f$exposure]]
    mu <- ave(y, p$group, FUN = sum) / ave(E, p$group, FUN = sum)
    kap <- sapply(split(seq_len(n), p$position), \(i) nb_kappa(y[i], E[i], mu[i]))
    kappas[[f$feature]] <- kap
    k <- kap[p$position]
    X[, , f$feature] <- rgamma_cols(S, k + y, k / mu + E)
  }

  # common scale: pooled within league-season-position SD of the posterior means
  Xbar <- apply(X, c(2, 3), mean)
  w <- p$poss
  ctr <- colSums(Xbar * w) / sum(w)
  scl <- apply(Xbar, 2, \(v) sqrt(sum((v - ave(v, p$group))^2) / (n - n_distinct(p$group))))
  Z <- X
  for (f in seq_along(FEATURES)) Z[, , f] <- (X[, , f] - ctr[f]) / scl[f]
  Zbar <- apply(Z, c(2, 3), mean)

  # centres: possession-weighted averages of the posterior means
  wmean <- function(g) {
    as_tibble(Zbar) |> mutate(g = g, w = w) |> group_by(g) |>
      summarise(across(all_of(names(FEATURES)), \(v) sum(v * w) / sum(w)))
  }
  pos_c <- wmean(p$group)
  lg_c  <- wmean(paste(p$league, p$season))
  C_pos <- as.matrix(pos_c[match(p$group, pos_c$g), names(FEATURES)])
  C_lg  <- as.matrix(lg_c[match(paste(p$league, p$season), lg_c$g), names(FEATURES)])

  D_pos <- dist_to_centre(Z, C_pos)
  D_lg  <- dist_to_centre(Z, C_lg)
  top_features <- function(dev, k = 3) apply(dev, 1, function(r) {
    o <- order(-abs(r))[1:k]
    paste0(FEATURES[names(r)[o]], ifelse(r[o] > 0, " +", " -"), collapse = ", ")
  })
  typ <- p |>
    select(league, season, player_id, player, position, team, teams, minutes, pts, shots) |>
    mutate(dist_league = colMeans(D_lg), dist_position = colMeans(D_pos),
           stands_out_for = top_features(Zbar - C_pos)) |>
    bind_cols(extreme_probs(D_lg, paste(p$league, p$season)) |>
                rename(p_league_in_one = p_most_typical) |> select(p_league_in_one),
              extreme_probs(D_pos, p$group) |>
                rename(p_typical_position = p_most_typical, p_most_unusual = p_most_different))

  # paths per position, and time positions
  paths <- list()
  tp <- map_dfr(c("G", "F", "C"), function(pos) {
    means <- pos_c |> filter(endsWith(g, paste0(" ", pos))) |>
      mutate(season = as.integer(sub("^\\S+ (\\d+) .*$", "\\1", g)))
    P <- league_path(means, names(FEATURES))
    paths[[pos]] <<- P
    map_dfr(which(p$position == pos), function(i) {
      t <- time_position(Z[, i, ], P, p$season[i])
      ahead <- t$tau - p$season[i]
      tibble(row = i, tau = median(t$tau), tau_lo = quantile(t$tau, .05), tau_hi = quantile(t$tau, .95),
             years_ahead = median(ahead), p_ahead_2 = mean(ahead >= 2), p_behind_2 = mean(ahead <= -2),
             off_path = median(t$off), gain = median(t$gain))
    })
  }) |> arrange(row)

  path_tbl <- imap_dfr(paths, \(P, pos) {
    out <- as_tibble(P)
    # back to the natural scale for plotting
    for (f in names(FEATURES)) out[[f]] <- out[[f]] * scl[f] + ctr[f]
    out |> mutate(league = lg, position = pos, season = attr(P, "season"))
  })

  # draws of shot-mix shares kept for the shot-chart figure: posterior mean is enough
  list(players = bind_cols(typ, tp |> select(-row)),
       deviations = bind_cols(p |> select(league, season, player_id, player, position),
                              as_tibble(Zbar - C_pos)),
       # posterior SD of each standardised feature (for the player-fit uncertainty in step 04)
       sds = bind_cols(p |> select(league, season, player_id), as_tibble(apply(Z, c(2, 3), sd))),
       paths = path_tbl,
       priors = tibble(league = lg, position = names(alpha), dirichlet_alpha = alpha) |>
         left_join(as_tibble(kappas) |> mutate(position = names(alpha)), by = "position"))
}

res <- map(c("NBA", "EuroLeague"), analyse_league)
players_out <- map_dfr(res, "players")
write.csv(players_out, file.path(tab_dir, "player_styles.csv"), row.names = FALSE)
write.csv(map_dfr(res, "deviations"), file.path(tab_dir, "player_profile_deviations.csv"), row.names = FALSE)
write.csv(map_dfr(res, "sds"), file.path(tab_dir, "player_profile_sd.csv"), row.names = FALSE)
write.csv(map_dfr(res, "paths"), file.path(tab_dir, "league_paths_players.csv"), row.names = FALSE)
write.csv(map_dfr(res, "priors"), file.path(tab_dir, "player_prior_strength.csv"), row.names = FALSE)

# ---- headlines -------------------------------------------------------------------------------
cat("\nThe league in one player\n")
players_out |> group_by(league, season) |> slice_max(p_league_in_one, n = 1, with_ties = FALSE) |>
  select(league, season, player, position, team, p_league_in_one) |> print(n = 40)
cat("\nMost unusual player each season\n")
players_out |> group_by(league, season) |> slice_max(dist_position, n = 1, with_ties = FALSE) |>
  select(league, season, player, position, team, dist_position, stands_out_for) |> print(n = 40)
cat("\nAhead of their time (played by 2020/21, so 5+ seasons of future to compare with)\n")
players_out |> filter(season <= 2020, gain > 0.5) |> group_by(league) |>
  slice_max(years_ahead, n = 15, with_ties = FALSE) |>
  select(league, season, player, position, team, tau, tau_lo, tau_hi, years_ahead, p_ahead_2, gain) |>
  print(n = 40)
cat("\nBehind their time (2016/17 or later)\n")
players_out |> filter(season >= 2016, gain > 0.5) |> group_by(league) |>
  slice_min(years_ahead, n = 10, with_ties = FALSE) |>
  select(league, season, player, position, team, tau, years_ahead, p_behind_2, gain) |> print(n = 30)
