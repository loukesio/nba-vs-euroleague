// Does one league follow the other? A shift model with a DISCRETE lag k (in seasons).
//
//   NBA:        yN_t ~ Normal(f_t, sqrt(seN_t^2 + tauN^2))           f = smooth NBA trend (RW2)
//   EuroLeague: yE_t ~ Normal(a + b * f_{t-k}, sqrt(seE_t^2 + tauE^2))
//
// k > 0  -> the EuroLeague today looks like the NBA k seasons ago (EuroLeague follows).
// k < 0  -> the EuroLeague anticipates the NBA (NBA follows).
// a, b   -> allow different levels/scales (rules, game length); b > 0 keeps the same direction.
//
// Two tricks:
// 1. The discrete k is MARGINALISED OUT:  p(data) = sum_k p(k) p(data | k)  (log_sum_exp),
//    and P(k | data) is recovered in generated quantities.
// 2. The NBA curve f is INTEGRATED OUT analytically (as in season_curve.stan): for each k the
//    NBA and EuroLeague season means are jointly multivariate normal,
//      Cov(yN)     = K + RN
//      Cov(yE)     = b^2 K[idx_k, idx_k] + RE
//      Cov(yN, yE) = b K[, idx_k]
//    with K the RW2 prior covariance and idx_k = e_idx - k. Only 7 continuous parameters remain.
//
// The same EuroLeague seasons are used for every k (t - k inside the NBA range for all k), so
// the likelihoods for different k are comparable.

data {
  int<lower=3> T;              // NBA seasons (t = 1..T)
  vector[T] yN;
  vector<lower=0>[T] seN;
  int<lower=0> K;              // lags considered: -K..K
  int<lower=1> TE;             // EuroLeague seasons used
  array[TE] int<lower=K + 1, upper=T - K> e_idx;  // their positions on the NBA time axis
  vector[TE] yE;
  vector<lower=0>[TE] seE;
}
transformed data {
  int NK = 2 * K + 1;
  int N = T + TE;
  vector[NK] log_prior_k = rep_vector(-log(NK), NK);  // uniform prior over lags
  matrix[T, T] M = rep_matrix(0, T, T);
  for (t in 1:T) {
    M[t, 1] = 1;
    M[t, 2] = t - 1;
    for (j in 3:t) M[t, j] = t - j + 1;
  }
  vector[N] y = append_row(yN, yE);
}
parameters {
  real<lower=0> omega;
  real<lower=0> tauN;
  real a;
  real<lower=0> b;
  real<lower=0> tauE;
}
transformed parameters {
  vector[NK] lp;
  {
    vector[T] d = append_row([4, 1]', rep_vector(square(omega), T - 2));
    matrix[T, T] Kf = M * diag_matrix(d) * M';
    vector[N] mu = append_row(rep_vector(0, T), rep_vector(a, TE));
    for (i in 1:NK) {
      int k = i - K - 1;
      array[TE] int idx;
      for (e in 1:TE) idx[e] = e_idx[e] - k;
      matrix[N, N] S;
      S[1:T, 1:T] = add_diag(Kf, square(seN) + square(tauN));
      S[(T + 1):N, (T + 1):N] = add_diag(square(b) * Kf[idx, idx], square(seE) + square(tauE));
      S[1:T, (T + 1):N] = b * Kf[, idx];
      S[(T + 1):N, 1:T] = S[1:T, (T + 1):N]';
      lp[i] = log_prior_k[i] + multi_normal_lpdf(y | mu, S);
    }
  }
}
model {
  omega ~ normal(0, 0.3);
  tauN ~ normal(0, 0.3);
  tauE ~ normal(0, 0.3);
  a ~ normal(0, 1);
  b ~ normal(1, 1);
  target += log_sum_exp(lp);
}
generated quantities {
  vector[NK] p_k = softmax(lp);  // P(k | data, other parameters); averaged over draws in R
}
