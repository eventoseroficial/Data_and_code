functions {
  real sparse_leroux_lpdf(vector phi, real tau, real rho, int n, matrix W) {
      matrix[n, n] W_calc;

      W_calc = tau*(rho*W + (1-rho)*diag_matrix(rep_vector(1, n)));

      return 0.5 * (log_determinant(W_calc) - quad_form(W_calc, phi));
    }
}

data {
   int<lower=0> N; // número de áreas
   matrix[N, N] W; // matriz de vizinhança
   int<lower=0> Nd; // número de observações
   int d[Nd];// vetor de mortes observadas 
   int<lower=0> nc; // número de covariáveis
   int<lower=0> t; // número de intervalos analisados (cada um tem o próprio intercepto)
   matrix[Nd, nc] X; // matriz de covariáveis
   vector<lower=0>[Nd] dstar; // número de mortes esperadas
   vector<lower=0>[Nd] y; // tempo em risco dos indivíduos
   int RiskYear[Nd]; // identificação do ano de risco
   int area[Nd]; // identificação única para área
}

parameters {
  vector[nc] beta;
  vector[N] phi_unscaled;
  real<lower = 0> tau;
  real<lower = 0, upper = 0.95> rho;
  vector[t] alpha;
}

transformed parameters {
    vector[N] phi;
    phi = phi_unscaled - mean(phi_unscaled);
}

model {
  phi_unscaled ~ sparse_leroux(tau, rho, N, W);
  beta ~ normal(0, 100);
  alpha ~ normal(0, 100);

  tau ~ gamma(1, 1);
  rho ~ beta(1, 1);

  for (i in 1:Nd) {
    d[i] ~ poisson(dstar[i] + y[i]*exp(alpha[RiskYear[i]] + X[i, ]*beta + phi[area[i]]));
  }
}

generated quantities {
   vector[Nd] log_lik;

   for(i in 1:Nd) {
      log_lik[i] = poisson_lpmf(d[i] | dstar[i] + y[i]*exp(alpha[RiskYear[i]] + X[i, ]*beta + phi[area[i]]));
   }
}
