# Team-game metrics, following Mandić et al. (2019, PLOS ONE) definitions exactly,
# plus a few additions (marked NEW).
#
# Input: one row per team per game with columns
#   league, season, phase, game_id, team, opp, home, win, pts, opp_pts,
#   p2m, p2a, p3m, p3a, ftm, fta, orb, drb, ast, stl, blk, tov, pf, entropy_min
# Opponent columns (o_*) are added by add_opponent().

library(dplyr)

add_opponent <- function(tg) {
  opp <- tg |>
    select(league, game_id, team, p2m, p2a, p3m, p3a, ftm, fta, orb, drb, tov, pf) |>
    rename_with(\(x) paste0("o_", x), -c(league, game_id, team)) |>
    rename(opp = team)
  inner_join(tg, opp, by = c("league", "game_id", "opp"))
}

compute_metrics <- function(tg) {
  tg |>
    mutate(
      # Possessions proxy used by the paper: F.NP = P2A + P3A + 0.5 FTA + TOV
      np   = p2a + p3a + 0.5 * fta + tov,
      o_np = o_p2a + o_p3a + 0.5 * o_fta + o_tov,
      frv  = o_pf,  # fouls received = fouls committed by the opponent

      # Pace-adjusted counts (per 100 possessions)
      fta_100 = 100 * fta / np,
      p2a_100 = 100 * p2a / np,
      p3a_100 = 100 * p3a / np,
      ast_100 = 100 * ast / np,
      blk_100 = 100 * blk / np,
      stl_100 = 100 * stl / np,
      tov_100 = 100 * tov / np,
      fcm_100 = 100 * pf / np,
      trb_100 = 100 * (orb + drb) / np,

      # Shooting percentages
      ft_pct = ftm / fta,
      p2_pct = p2m / p2a,
      p3_pct = p3m / p3a,

      # Fouls and free throws
      ft_per_foul = fta / frv,

      # Oliver's four factors (team and opponent)
      efg  = (p2m + 1.5 * p3m) / (p2a + p3a),
      tor  = tov / np,
      ftr  = ftm / (p2a + p3a),
      orb_rate = orb / (orb + o_drb),
      drb_rate = drb / (drb + o_orb),
      o_efg = (o_p2m + 1.5 * o_p3m) / (o_p2a + o_p3a),
      o_tor = o_tov / o_np,
      o_ftr = o_ftm / (o_p2a + o_p3a),

      # NEW: offensive efficiency and shot mix
      ortg      = 100 * pts / np,
      p3a_share = p3a / (p2a + p3a)
    )
}

# Metric catalogue used by RQ2/RQ3: name, label, family for the Bayesian models.
#   "gaussian" = analysed on the original scale (as in the paper);
#   pct metrics are modelled on the logit scale in the extended models.
METRICS <- tibble::tribble(
  ~metric,       ~label,                         ~group,
  "np",          "Possessions per game",         "Pace",
  "pace40",      "Possessions per 40 min",       "Pace",
  "entropy_min", "Minutes entropy (bits)",       "Rotation",
  "ortg",        "Points per 100 poss.",         "Efficiency",
  "efg",         "Effective FG%",                "Four factors",
  "tor",         "Turnover rate",                "Four factors",
  "ftr",         "Free-throw rate (FTM/FGA)",    "Four factors",
  "orb_rate",    "Offensive rebound rate",       "Four factors",
  "drb_rate",    "Defensive rebound rate",       "Four factors",
  "p3a_share",   "3PA share of FGA",             "Shooting",
  "p2_pct",      "2P%",                          "Shooting",
  "p3_pct",      "3P%",                          "Shooting",
  "ft_pct",      "FT%",                          "Shooting",
  "fta_100",     "FTA per 100 poss.",            "Shooting",
  "p2a_100",     "2PA per 100 poss.",            "Shooting",
  "p3a_100",     "3PA per 100 poss.",            "Shooting",
  "ast_100",     "Assists per 100 poss.",        "Other",
  "blk_100",     "Blocks per 100 poss.",         "Other",
  "stl_100",     "Steals per 100 poss.",         "Other",
  "tov_100",     "Turnovers per 100 poss.",      "Other",
  "fcm_100",     "Fouls per 100 poss.",          "Fouls",
  "ft_per_foul", "FTA per foul received",        "Fouls",
  "trb_100",     "Rebounds per 100 poss.",       "Rebounding"
)

# Shannon entropy (bits) of playing-time shares across a team's players in one game.
minutes_entropy <- function(minutes) {
  p <- minutes[minutes > 0] / sum(minutes[minutes > 0])
  -sum(p * log2(p))
}
