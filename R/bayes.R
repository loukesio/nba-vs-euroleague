# Shared helpers for the Bayesian models (brms + cmdstanr).

suppressMessages({ library(brms); library(posterior); library(dplyr) })

options(brms.backend = "cmdstanr", mc.cores = 4,
        # keep compiled Stan programs between R sessions (default is a temp dir that vanishes)
        cmdstanr_write_stan_file_dir = here::here(".stan_cache"))
dir.create(here::here(".stan_cache"), showWarnings = FALSE)
BRMS_ARGS <- list(chains = 4, iter = 1500, warmup = 750, seed = 2026, refresh = 0)

# Fit a brms model once and cache it on the data drive. `file_refit = "on_change"`
# refits only if the data or model changed, so re-running scripts is cheap.
fit_cached <- function(formula, data, file, ...) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  do.call(brm, c(list(formula = formula, data = data, file = file,
                      file_refit = "on_change"), BRMS_ARGS, list(...)))
}

# Refit a cached brms model to new data without recompiling. A fit loaded from disk may point
# to a compiled program that no longer exists; then recompile once and reuse that fit as base.
refit <- local({
  fresh <- list()
  function(base, newdata, key, ...) {
    b <- fresh[[key]] %||% base
    out <- tryCatch(update(b, newdata = newdata, recompile = FALSE, refresh = 0, seed = 2026, ...),
                    error = function(e) NULL)
    if (is.null(out)) {
      out <- update(b, newdata = newdata, recompile = TRUE, refresh = 0, seed = 2026, ...)
      fresh[[key]] <<- out
    }
    out
  }
})

# Convergence summary to report alongside every model: max R-hat, min bulk/tail ESS,
# and number of divergent transitions.
diagnostics <- function(fit) {
  s <- summarise_draws(as_draws_df(fit), "rhat", "ess_bulk", "ess_tail")
  np <- nuts_params(fit)
  tibble(
    max_rhat = max(s$rhat, na.rm = TRUE),
    min_ess_bulk = min(s$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(s$ess_tail, na.rm = TRUE),
    divergences = sum(np$Value[np$Parameter == "divergent__"])
  )
}

# ---- The 2019 paper's trend model (hand-written Stan, sufficient statistics) ----------
# Compiled once per session; see RQ2_trends_leadlag/models/paper_trend.stan
paper_trend_model <- local({
  mod <- NULL
  function() {
    if (is.null(mod)) mod <<- cmdstanr::cmdstan_model(
      here::here("RQ2_trends_leadlag", "models", "paper_trend.stan"))
    mod
  }
})

# y: game-level values; season: integer season; centre: season where `mu` is reported.
# Returns posterior draws of mu (level at `centre`) and s (trend per season).
fit_paper_trend <- function(y, season, centre) {
  ok <- is.finite(y)
  st <- tibble(y = y[ok], season = season[ok]) |>
    group_by(season) |>
    summarise(n = n(), ybar = mean(y), s2 = var(y), .groups = "drop")
  fit <- paper_trend_model()$sample(
    data = list(J = nrow(st), t = st$season - centre, n = st$n, ybar = st$ybar, s2 = st$s2),
    chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 2500,
    seed = 2026, refresh = 0, show_messages = FALSE, adapt_delta = 0.99
  )
  dr <- fit$draws(c("mu", "s"), format = "df")
  diag <- fit$diagnostic_summary(quiet = TRUE)
  list(mu = dr$mu, s = dr$s,
       max_rhat = max(fit$summary(c("mu", "s", "sigma"))$rhat),
       divergences = sum(diag$num_divergent))
}

# ---- Smooth season curve (RW2) -------------------------------------------------------
stan_model_cached <- local({
  cache <- list()
  function(name) {
    if (is.null(cache[[name]])) cache[[name]] <<- cmdstanr::cmdstan_model(
      here::here("RQ2_trends_leadlag", "models", paste0(name, ".stan")))
    cache[[name]]
  }
})

# sm: output of season_means() for ONE league (columns season, mean, se).
# Returns a draws matrix of the latent curve f on the ORIGINAL scale (draws x seasons).
fit_season_curve <- function(sm) {
  m <- mean(sm$mean); s <- sd(sm$mean)
  fit <- stan_model_cached("season_curve")$sample(
    data = list(T = nrow(sm), y = (sm$mean - m) / s, se = sm$se / s),
    chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000,
    seed = 2026, refresh = 0, show_messages = FALSE, adapt_delta = 0.95
  )
  f <- m + s * posterior::as_draws_matrix(fit$draws("f"))
  colnames(f) <- sm$season
  attr(f, "diag") <- tibble(max_rhat = max(fit$summary(c("omega", "tau", "f"))$rhat),
                            divergences = sum(fit$diagnostic_summary(quiet = TRUE)$num_divergent))
  f
}

# Summarise a vector of posterior draws the way the paper's tables do:
# mean, sd ("standard error"), 95% interval, and P(draw > 0).
summ <- function(x) {
  tibble(mean = mean(x), sd = sd(x), q2.5 = quantile(x, 0.025), q97.5 = quantile(x, 0.975),
         p_gt0 = mean(x > 0))
}
