# RQ2 step 2 — Smooth (non-linear) trends, 2010/11-2025/26, regular season.
#
# The 2019 model forces a straight line through the season means. Here each league gets a
# smooth latent curve (second-order random walk, models/season_curve.stan) that can bend,
# e.g. when the NBA's three-point growth accelerated. We save posterior summaries of the
# curve for every metric x league, which the plotting script and the report use.

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
source(here::here("R", "bayes.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(tidyr); library(purrr) })

rq_dir <- here::here("RQ2_trends_leadlag")
tg <- load_team_games(SEASONS) |> filter(phase == "regular")

summ_cols <- function(f) tibble(fit = colMeans(f),
                                lo = apply(f, 2, quantile, 0.025),
                                hi = apply(f, 2, quantile, 0.975))

res <- map(METRICS$metric, function(metric) {
  fits <- map(set_names(c("NBA", "EuroLeague")), function(lg) {
    sm <- season_means(tg |> filter(league == lg), metric)
    list(sm = sm, f = fit_season_curve(sm))
  })
  curve <- imap_dfr(fits, \(x, lg) tibble(metric = metric, league = lg, season = x$sm$season,
                                          obs_mean = x$sm$mean, obs_se = x$sm$se) |>
                      bind_cols(summ_cols(x$f)))
  # Gap NBA - EuroLeague per season: the two fits are independent, so pairing draws is valid
  gap_draws <- fits$NBA$f - fits$EuroLeague$f
  first <- gap_draws[, 1]; last <- gap_draws[, ncol(gap_draws)]
  gap <- tibble(metric = metric, season = fits$NBA$sm$season) |> bind_cols(summ_cols(gap_draws))
  # Convergence: did the absolute gap shrink between 2010/11 and 2025/26?
  conv <- tibble(metric = metric,
                 gap_first = mean(first), gap_last = mean(last),
                 p_gap_shrank = mean(abs(last) < abs(first)),
                 change_NBA = mean(fits$NBA$f[, ncol(fits$NBA$f)] - fits$NBA$f[, 1]),
                 change_EL  = mean(fits$EuroLeague$f[, ncol(fits$EuroLeague$f)] - fits$EuroLeague$f[, 1]),
                 max_rhat = max(attr(fits$NBA$f, "diag")$max_rhat, attr(fits$EuroLeague$f, "diag")$max_rhat),
                 divergences = attr(fits$NBA$f, "diag")$divergences + attr(fits$EuroLeague$f, "diag")$divergences)
  list(curve = curve, gap = gap, conv = conv)
})

curves <- map_dfr(res, "curve")
gaps   <- map_dfr(res, "gap")
conv   <- map_dfr(res, "conv") |> left_join(METRICS, by = "metric")

write.csv(gaps, file.path(rq_dir, "results", "tables", "smooth_trend_gap_curves.csv"), row.names = FALSE)
write.csv(conv, file.path(rq_dir, "results", "tables", "convergence_2010_2025.csv"), row.names = FALSE)
write.csv(curves,  file.path(rq_dir, "results", "tables", "smooth_trend_curves.csv"), row.names = FALSE)
print(conv |> select(label, gap_first, gap_last, p_gap_shrank, change_NBA, change_EL, max_rhat), n = 30)
