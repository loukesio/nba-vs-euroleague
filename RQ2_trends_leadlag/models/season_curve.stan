// Smooth latent trend through season means: a second-order random walk (RW2).
//
//   ybar_t ~ Normal(f_t, sqrt(se_t^2 + tau^2))      observed season mean (se from games)
//   f_t    = 2 f_{t-1} - f_{t-2} + omega * eps_t     the trend keeps its slope unless pushed
//   eps_t  ~ Normal(0, 1),  f_1 ~ Normal(0, 2),  slope f_2 - f_1 ~ Normal(0, 1)
//
// omega = wiggliness (omega -> 0 gives a straight line, i.e. the 2019 paper's model);
// tau   = season-specific shocks that are not part of the trend (e.g. the 2019/20 bubble).
// y is standardised before fitting, so the priors are on a unit scale.
//
// HOW IT IS FITTED. Sampling f directly fails: when omega is small the f_t are tightly
// coupled (a "funnel"), giving divergences and R-hat up to 1.3 in both the centred and the
// non-centred form. But everything here is Gaussian, so f can be INTEGRATED OUT exactly:
//   f = M u,  u = (f_1, slope, eps_3..eps_T) ~ Normal(0, diag(4, 1, omega^2, ...))
//   => y ~ MultiNormal(0, K + R),  K = M diag(.) M',  R = diag(se^2 + tau^2)
// Stan then samples only omega and tau. Draws of the curve f are generated afterwards
// from its exact conditional distribution f | y, omega, tau (Gaussian conditioning).

data {
  int<lower=3> T;
  vector[T] y;           // standardised season means
  vector<lower=0>[T] se; // their standard errors (same scale)
}
transformed data {
  matrix[T, T] M = rep_matrix(0, T, T);   // maps (f1, slope, eps_3..T) to f
  for (t in 1:T) {
    M[t, 1] = 1;
    M[t, 2] = t - 1;
    for (j in 3:t) M[t, j] = t - j + 1;
  }
}
parameters {
  real<lower=0> omega;
  real<lower=0> tau;
}
transformed parameters {
  matrix[T, T] K;        // prior covariance of the curve f
  matrix[T, T] Sigma;    // covariance of the observed season means
  {
    vector[T] d = append_row([4, 1]', rep_vector(square(omega), T - 2));
    K = M * diag_matrix(d) * M';
  }
  Sigma = add_diag(K, square(se) + square(tau));
}
model {
  omega ~ normal(0, 0.3);  // half-normal: favours smooth curves
  tau ~ normal(0, 0.3);
  y ~ multi_normal(rep_vector(0, T), Sigma);
}
generated quantities {
  vector[T] f;             // the trend, drawn from f | y, omega, tau
  vector[T] slope;         // year-on-year change of the trend
  {
    matrix[T, T] KSi = mdivide_right_spd(K, Sigma);           // K Sigma^-1
    vector[T] m = KSi * y;
    matrix[T, T] S = K - KSi * K;
    S = 0.5 * (S + S') + diag_matrix(rep_vector(1e-9, T));
    f = multi_normal_rng(m, S);
  }
  slope[1] = 0;
  for (t in 2:T) slope[t] = f[t] - f[t - 1];
}
