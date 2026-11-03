// The Mandić et al. (2019) trend model, written on per-season sufficient statistics.
//
// Original (game-level) model for one league and one variable:
//   y_ij ~ Normal(mu_j, sigma_j)                 game i in season j
//   mu_j ~ Normal(mu + s * t_j, sigma)           season means scatter around a line
//
// Because games are i.i.d. Normal within a season, the season's game count n_j,
// mean ybar_j and sample variance s2_j carry ALL the information in the data:
//   ybar_j               ~ Normal(mu_j, sigma_j / sqrt(n_j))
//   (n_j - 1) s2_j / sigma_j^2 ~ ChiSquare(n_j - 1)
// so this model gives exactly the same posterior as the game-level one, ~1000x faster.
// Priors are flat (Stan's default), as in the paper.
//
// Parameterisation: CENTRED (mu_j sampled directly). Each season mean is pinned down by
// hundreds-to-thousands of games, and in that strong-data regime the centred form samples
// well. (We tried the non-centred form; it mixed worse: R-hat up to 1.22.) The remaining
// few divergences, when sigma -> 0 under the flat prior, are handled with adapt_delta = 0.99.

data {
  int<lower=1> J;              // number of seasons
  vector[J] t;                 // centred season (e.g. season - 2008)
  array[J] int<lower=2> n;     // games in each season
  vector[J] ybar;              // season mean
  vector<lower=0>[J] s2;       // season sample variance
}
parameters {
  real mu;                     // overall mean at t = 0
  real s;                      // linear trend per season
  real<lower=0> sigma;         // between-season variability of season means
  vector[J] mu_j;              // season means
  vector<lower=0>[J] sigma_j;  // within-season (game-to-game) SD
}
model {
  mu_j ~ normal(mu + s * t, sigma);
  ybar ~ normal(mu_j, sigma_j ./ sqrt(to_vector(n)));
  for (j in 1:J) {
    // density of the sample variance, including the Jacobian of s2 -> chi-square scale
    target += chi_square_lpdf((n[j] - 1) * s2[j] / square(sigma_j[j]) | n[j] - 1)
              + log(n[j] - 1) - 2 * log(sigma_j[j]);
  }
}
