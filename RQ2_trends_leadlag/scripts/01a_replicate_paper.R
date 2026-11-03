# RQ2 step 1a — Replicate Mandić et al. (2019) Table 1 from the authors' own data (S1 Data).
#
# Their model, for one league and one variable y (observation i in season j):
#   y_ij ~ Normal(mu_j, sigma_j)                 # each season has its own mean and spread
#   mu_j ~ Normal(mu + s * (t_j - 2008), sigma)  # season means scatter around a linear trend
# In brms this would be  bf(y ~ 1 + t_c + (1 | season), sigma ~ 0 + season), but on 40k+ games
# that takes >10 min per fit. We fit the identical model on per-season sufficient statistics
# with a small hand-written Stan program instead (models/paper_trend.stan): ~1 s per fit.
# mu = the mean at 2008/09, s = the change per season.
#
# If our estimates match their published Table 1, we know we understood the method,
# and we can apply the same model to the new data with confidence.

source(here::here("R", "config.R"))
source(here::here("R", "bayes.R"))
source(here::here("R", "theme.R"))
suppressMessages({ library(tidyr); library(purrr); library(ggplot2) })

rq_dir <- here::here("RQ2_trends_leadlag")
s1 <- readRDS(file.path(DATA_ROOT, "raw", "paper_S1", "unz", "S1_Data.rds"))

# Published Table 1 (RS + playoffs combined): mu_Euro, mu_NBA, s_Euro, s_NBA
published <- tribble(
  ~var,                ~mu_Euroleague, ~mu_NBA, ~s_Euroleague, ~s_NBA,
  "F.NP",               83.00, 107.41, -0.15,  0.16,
  "Entropy.MIN",         3.15,   3.15,  0.01,  0.01,
  "F.P3pct",            35.59,  35.16, -0.00,  0.03,
  "F.P2pct",            51.84,  48.12, -0.06,  0.22,
  "F.FTpct",            73.33,  75.61,  0.17,  0.06,
  "PaceAdj.Tot.FTA",    24.72,  22.61, -0.50, -0.17,
  "PaceAdj.Tot.P2A",    46.90,  58.67, -0.01, -0.46,
  "PaceAdj.Tot.P3A",    24.55,  17.17,  0.32,  0.60,
  "PaceAdj.Tot.STL",     9.77,   7.03, -0.35, -0.01,
  "PaceAdj.Tot.AST",    16.79,  20.05,  0.47,  0.02,
  "PaceAdj.Tot.BLK",     3.12,   4.58,  0.04, -0.02,
  "PaceAdj.Tot.TOV",    16.17,  12.85, -0.06, -0.05,
  "F.FTperF",           92.79, 113.81, -1.14,  0.13,
  "PaceAdj.Tot.FCM",    26.38,  19.75, -0.19, -0.17,
  "F.DRB",              70.00,  73.52,  0.03,  0.30,
  "F.ORB",              30.01,  26.48, -0.03, -0.30,
  "PaceAdj.Tot.TRB",    39.54,  39.22,  0.28,  0.01
) |>
  pivot_longer(-var, names_to = c(".value", "league"), names_sep = "_")

d <- s1 |>
  mutate(season = as.integer(substr(Season, 1, 4))) |>
  select(League, season, all_of(unique(published$var)))

# Everything except possessions and entropy is reported x100 in the paper
scale100 <- setdiff(unique(published$var), c("F.NP", "Entropy.MIN"))
d <- d |> mutate(across(all_of(scale100), \(x) 100 * x))

fit_one <- function(var, league) {
  dd <- d |> filter(League == league)
  f <- fit_paper_trend(dd[[var]], dd$season, centre = 2008)
  bind_rows(summ(f$mu) |> mutate(param = "mu"), summ(f$s) |> mutate(param = "s")) |>
    mutate(var = var, league = league, max_rhat = f$max_rhat, divergences = f$divergences)
}

est <- expand_grid(var = unique(published$var), league = c("Euroleague", "NBA")) |>
  pmap_dfr(fit_one)

comparison <- est |>
  select(var, league, param, mean, sd, max_rhat, divergences) |>
  left_join(published |> pivot_longer(c(mu, s), names_to = "param", values_to = "published"),
            by = c("var", "league", "param"))

write.csv(comparison, file.path(rq_dir, "results", "tables", "replication_vs_published.csv"),
          row.names = FALSE)

p <- ggplot(comparison, aes(published, mean, colour = league)) +
  geom_abline(linetype = 2, colour = "grey60") +
  geom_pointrange(aes(ymin = mean - 2 * sd, ymax = mean + 2 * sd), size = 0.25) +
  facet_wrap(~ param, scales = "free",
             labeller = as_labeller(c(mu = "Season-average level (mu)", s = "Trend per season (s)"))) +
  scale_colour_manual(values = LEAGUE_COLOURS) +
  labs(x = "Published estimate (Mandić et al. 2019, Table 1)",
       y = "Our brms estimate (± 2 SD)", colour = NULL,
       title = "Replicating the 2019 model on the authors' own data",
       subtitle = "Points on the dashed line = identical estimates. 17 variables x 2 leagues.") +
  theme_project()
ggsave(file.path(rq_dir, "results", "figures", "rq2_replication_check.png"), p,
       width = 8, height = 4.2, dpi = 300)

print(comparison |> mutate(diff = mean - published) |> arrange(desc(abs(diff))), n = 70)
