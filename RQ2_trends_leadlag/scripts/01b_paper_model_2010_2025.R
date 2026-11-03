# RQ2 step 1b — The 2019 paper's model (validated in 01a) applied to 2010/11-2025/26.
# Output mirrors their Table 1: per league, the mean level (mu, at 2017/18) and linear trend (s),
# plus the NBA - EuroLeague differences with posterior probabilities.

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
source(here::here("R", "bayes.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(tidyr); library(purrr) })

rq_dir <- here::here("RQ2_trends_leadlag")
CENTRE <- 2017

tg <- load_team_games(SEASONS)   # regular season + playoffs combined, as in Table 1

# Report percentages and rates x100, as the paper did
scale_of <- \(m) if (m %in% c("np", "pace40", "entropy_min", "ortg") | grepl("_100$", m)) 1 else 100

draws <- METRICS$metric |>
  set_names() |>
  map(function(m) {
    map(set_names(c("NBA", "EuroLeague")), function(lg) {
      d <- tg |> filter(league == lg)
      fit_paper_trend(scale_of(m) * d[[m]], d$season, centre = CENTRE)
    })
  })

tab <- imap_dfr(draws, function(dl, m) {
  nba <- dl$NBA; el <- dl$EuroLeague
  tibble(
    metric = m,
    mu_EL = mean(el$mu), mu_NBA = mean(nba$mu),
    d_mu = mean(nba$mu - el$mu), d_mu_sd = sd(nba$mu - el$mu), P_d_mu_gt0 = mean(nba$mu > el$mu),
    s_EL = mean(el$s), s_NBA = mean(nba$s),
    d_s = mean(nba$s - el$s), d_s_sd = sd(nba$s - el$s), P_d_s_gt0 = mean(nba$s > el$s),
    max_rhat = max(nba$max_rhat, el$max_rhat), divergences = nba$divergences + el$divergences
  )
}) |>
  left_join(METRICS, by = "metric")

write.csv(tab, file.path(rq_dir, "results", "tables", "table1_updated_2010_2025.csv"), row.names = FALSE)
print(tab |> select(label, mu_EL, mu_NBA, P_d_mu_gt0, s_EL, s_NBA, P_d_s_gt0, max_rhat), n = 30)
