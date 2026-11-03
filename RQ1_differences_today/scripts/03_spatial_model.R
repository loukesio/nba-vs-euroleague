# RQ1 step 3 — Where on the court are shots made? A Bayesian spatial model of shooting accuracy.
#
# Shots (2023/24-2025/26, regular season, inside 9 m) are counted on a 0.5 m grid per league.
# Model (brms, binomial):
#   made | trials(n) ~ league + t2(x_abs, y, by = league)
#   * t2() is a 2-D smooth surface (tensor-product spline): accuracy varies smoothly in space
#   * by = league gives each league its own surface
#   * x_abs = |x| uses left/right symmetry, doubling the data per cell
# Expected points per shot in a cell = (share of 3-point attempts there x 3 + rest x 2) x P(make).
# Note: the 3-point lines differ (6.75 m vs 7.24 m), so the PPS surfaces jump at different places.

source(here::here("R", "config.R"))
source(here::here("R", "court.R"))
source(here::here("R", "bayes.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(tidyr) })

rq_dir <- here::here("RQ1_differences_today")
CELL <- 0.5

grid <- load_shots(TODAY_SEASONS) |>
  filter(phase == "regular", dist < 9, y < 8.5) |>
  mutate(x_abs = (floor(abs(x) / CELL) + 0.5) * CELL,
         y = (floor(y / CELL) + 0.5) * CELL) |>
  group_by(league, x_abs, y) |>
  summarise(n = n(), made = sum(made), value = mean(value), .groups = "drop") |>
  filter(n >= 20)

fit <- fit_cached(
  bf(made | trials(n) ~ league + t2(x_abs, y, by = league, k = 8)),
  grid, family = binomial(),
  prior = c(prior(normal(0, 2), class = "b"), prior(normal(0, 1), class = "sds")),
  file = file.path(rq_dir, "data", "models", "spatial_accuracy"),
  control = list(adapt_delta = 0.95)
)

# Predict on the FULL grid (also cells with < 20 shots): the smooth surface fills the gaps.
pred <- tidyr::expand_grid(league = unique(grid$league),
                           x_abs = sort(unique(grid$x_abs)), y = sort(unique(grid$y))) |>
  filter(sqrt(x_abs^2 + y^2) < 9) |>
  left_join(grid, by = c("league", "x_abs", "y")) |>
  mutate(value = coalesce(value, if_else(
    sqrt(x_abs^2 + y^2) > if_else(league == "NBA", 7.24, 6.75), 3, 2)))
ep <- posterior_epred(fit, newdata = pred |> mutate(n = 1))
pred <- pred |>
  mutate(p_make = colMeans(ep),
         p_lo = apply(ep, 2, quantile, .05), p_hi = apply(ep, 2, quantile, .95),
         pps = value * p_make)

write.csv(pred, file.path(rq_dir, "results", "tables", "spatial_accuracy_grid.csv"), row.names = FALSE)
write.csv(diagnostics(fit), file.path(rq_dir, "results", "tables", "spatial_model_diagnostics.csv"),
          row.names = FALSE)
print(diagnostics(fit))
