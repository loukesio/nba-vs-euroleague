# Style profiles for RQ4: shrinkage (empirical Bayes) posteriors, typicality, and "time position".
#
# Every team-season or player-season gets a posterior over a style profile (shot mix + box rates).
# Posteriors are conjugate, so draws come straight from closed-form distributions (no MCMC):
#   * shot mix:   counts ~ Multinomial(p),  p ~ Dirichlet(alpha * m)   -> Dirichlet(alpha * m + counts)
#   * box rates:  y ~ Poisson(lambda * E), lambda ~ Gamma(kappa, kappa / mu) -> Gamma(kappa + y, kappa / mu + E)
#   * game means: ybar ~ N(theta, se^2),   theta ~ N(mu, tau^2)          -> normal, shrunk towards mu
# The prior centre (m, mu) is the league-season average; the prior strength (alpha, kappa, tau) is
# estimated from all units by maximising the marginal likelihood ("empirical Bayes").

suppressMessages({ library(dplyr) })

# ---- prior strength ------------------------------------------------------------------
# Dirichlet-multinomial: counts (n x K), m (n x K, prior mean per row). Returns alpha.
dm_alpha <- function(counts, m) {
  N <- rowSums(counts)
  nll <- function(la) {
    a <- exp(la) * m
    -sum(lgamma(exp(la)) - lgamma(N + exp(la)) + rowSums(lgamma(counts + a) - lgamma(a)))
  }
  exp(optimize(nll, c(log(1), log(1e5)))$minimum)
}

# Gamma-Poisson (negative binomial): y counts, E exposure, mu prior mean rate. Returns kappa.
nb_kappa <- function(y, E, mu) {
  nll <- function(lk) -sum(dnbinom(y, size = exp(lk), mu = mu * E, log = TRUE))
  exp(optimize(nll, c(log(0.1), log(1e4)))$minimum)
}

# ---- posterior draws -------------------------------------------------------------------
# Dirichlet draws for every row of `a` (n x K): returns array S x n x K
rdirichlet_rows <- function(S, a) {
  n <- nrow(a); K <- ncol(a)
  g <- array(rgamma(S * n * K, shape = rep(a, each = S)), c(S, n, K))
  g / as.vector(apply(g, c(1, 2), sum))
}

# Gamma draws: shape, rate vectors (length n) -> S x n matrix
rgamma_cols <- function(S, shape, rate) {
  matrix(rgamma(S * length(shape), shape = rep(shape, each = S), rate = rep(rate, each = S)), S)
}

# Normal-normal shrinkage within groups: ybar, se, group -> S x n matrix of draws
rshrunk_normal <- function(S, ybar, se, group) {
  d <- tibble(ybar, se, group) |>
    group_by(group) |>
    mutate(mu = mean(ybar), tau2 = max(var(ybar) - mean(se^2), 1e-12),
           B = tau2 / (tau2 + se^2), post_mean = mu + B * (ybar - mu), post_sd = sqrt(B) * se) |>
    ungroup()
  matrix(rnorm(S * nrow(d), rep(d$post_mean, each = S), rep(d$post_sd, each = S)), S)
}

# ---- geometry -------------------------------------------------------------------------
# Z: S x n x F draws on the standardised scale; C: n x F centre for each unit's group.
# Returns S x n matrix of Euclidean distances.
dist_to_centre <- function(Z, C) {
  d2 <- 0
  for (f in seq_len(dim(Z)[3])) d2 <- d2 + (Z[, , f] - rep(C[, f], each = dim(Z)[1]))^2
  sqrt(d2)
}

# For each group (rows of D sharing `group`), the posterior probability that each unit is the
# closest to its group's centre (most typical) and the furthest (most different).
extreme_probs <- function(D, group) {
  p_min <- p_max <- numeric(ncol(D))
  for (g in unique(group)) {
    idx <- which(group == g)
    Dg <- D[, idx, drop = FALSE]
    p_min[idx] <- tabulate(max.col(-Dg, ties.method = "first"), length(idx)) / nrow(D)
    p_max[idx] <- tabulate(max.col(Dg, ties.method = "first"), length(idx)) / nrow(D)
  }
  tibble(p_most_typical = p_min, p_most_different = p_max)
}

# The league's path through style space: per feature, a natural spline (df = 4) over the season
# means, evaluated on a fine grid. means: tibble(season, f1..fF). Returns grid matrix with
# attribute "season".
league_path <- function(means, features, step = 0.05, df = 4) {
  g <- seq(min(means$season), max(means$season), by = step)
  B <- splines::ns(means$season, df = df)
  Bg <- cbind(1, predict(B, g))
  P <- sapply(features, function(f) Bg %*% coef(lm(means[[f]] ~ B)))
  attr(P, "season") <- g
  P
}

# Time position: for one unit's draws Zi (S x F), the point on the path closest to each draw.
# Returns per draw: tau (season), off (distance to the path there) and gain (how much closer that
# point is than the league of the unit's own season; ~0 means "ahead" is not worth much).
time_position <- function(Zi, P, own_season) {
  d2 <- pmax(outer(rowSums(Zi^2), rowSums(P^2), "+") - 2 * Zi %*% t(P), 0)
  k <- max.col(-d2, ties.method = "first")
  own <- which.min(abs(attr(P, "season") - own_season))
  off <- sqrt(d2[cbind(seq_along(k), k)])
  list(tau = attr(P, "season")[k], off = off, gain = sqrt(d2[, own]) - off)
}
