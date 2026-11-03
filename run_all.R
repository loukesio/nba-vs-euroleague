# Re-run the whole project from raw data to figures.
# Downloads are cached on the data drive, so steps 01-02 only fetch what is missing.
# Bayesian fits are cached too (brms `file =`), so re-runs are fast unless data change.

steps <- c(
  "00_data/scripts/01_download_nba.R",
  "00_data/scripts/02_download_euroleague.R",
  "00_data/scripts/03_build_team_games.R",
  "00_data/scripts/04_build_shots.R",
  "00_data/scripts/05_build_player_seasons.R",

  "RQ1_differences_today/scripts/01_box_differences.R",
  "RQ1_differences_today/scripts/02_shot_zones.R",
  "RQ1_differences_today/scripts/03_spatial_model.R",
  "RQ1_differences_today/scripts/04_plot_rq1.R",

  "RQ2_trends_leadlag/scripts/01a_replicate_paper.R",
  "RQ2_trends_leadlag/scripts/01b_paper_model_2010_2025.R",
  "RQ2_trends_leadlag/scripts/02_smooth_trends.R",
  "RQ2_trends_leadlag/scripts/03a_lead_lag.R",
  "RQ2_trends_leadlag/scripts/03b_time_gap.R",
  "RQ2_trends_leadlag/scripts/04_plot_rq2.R",

  "RQ3_playoffs/scripts/01_playoff_models.R",
  "RQ3_playoffs/scripts/02_playoff_shot_zones.R",
  "RQ3_playoffs/scripts/03_plot_rq3.R",

  "RQ4_ahead_of_their_time/scripts/01_team_styles.R",
  "RQ4_ahead_of_their_time/scripts/02_player_styles.R",
  "RQ4_ahead_of_their_time/scripts/03_plot_rq4.R"
)

for (s in steps) {
  message("\n==> ", s)
  t0 <- Sys.time()
  callr::rscript(here::here(s), show = TRUE)
  message(sprintf("    done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
