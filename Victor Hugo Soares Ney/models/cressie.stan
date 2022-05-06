functions {
  real sparse_car_lpdf(vector phi, real tau, real alpha, 
    int[,] W_sparse, vector D_sparse, vector lambda, int n, int W_n) {
      row_vector[n] phit_D; // phi' * D
      row_vector[n] phit_W; // phi' * W
      vector[n] ldet_terms;
    
      phit_D = (phi .* D_sparse)';
      phit_W = rep_row_vector(0, n);
      for (i in 1:W_n) {
        phit_W[W_sparse[i, 1]] = phit_W[W_sparse[i, 1]] + phi[W_sparse[i, 2]];
        phit_W[W_sparse[i, 2]] = phit_W[W_sparse[i, 2]] + phi[W_sparse[i, 1]];
      }
    
      for (i in 1:n) ldet_terms[i] = log1m(alpha * lambda[i]);
      return 0.5 * (n * log(tau)
                    + sum(ldet_terms)
                    - tau * (phit_D * phi - alpha * (phit_W * phi)));
  }
}

data {
   int<lower=0> N; // número de áreas
   int<lower=0> Nd; // número de observações
   int d[Nd];// vetor de mortes observadas 
   int<lower=0> nc; // número de covariáveis
   int<lower=0> t; // número de intervalos analisados (cada um tem o próprio intercepto)
   matrix[Nd, nc] X; // matriz de covariáveis
   vector<lower=0>[Nd] dstar; // número de mortes esperadas
   vector<lower=0>[Nd] y; // tempo em risco dos indivíduos
   int RiskYear[Nd]; // identificação do ano de risco
   int area[Nd]; // identificação única para área
   matrix<lower = 0, upper = 1>[N,N] W; // adjacency matrix
  int W_n;                // number of adjacent region pairs
}

transformed data {
   int W_sparse[W_n, 2];   // adjacency pairs
  vector[N] D_sparse;     // diagonal of D (number of neigbors for each site)
  vector[N] lambda;       // eigenvalues of invsqrtD * W * invsqrtD

  { // generate sparse representation for W
  int counter;
  counter = 1;
  // loop over upper triangular part of W to identify neighbor pairs
    for (i in 1:(N - 1)) {
      for (j in (i + 1):N) {
        if (W[i, j] == 1) {
          W_sparse[counter, 1] = i;
          W_sparse[counter, 2] = j;
          counter = counter + 1;
        }
      }
    }
  }
  for (i in 1:N) D_sparse[i] = sum(W[i]);
  {
    vector[N] invsqrtD;  
    for (i in 1:N) {
      invsqrtD[i] = 1 / sqrt(D_sparse[i]);
    }
    lambda = eigenvalues_sym(quad_form(W, diag_matrix(invsqrtD)));
  }
}
parameters {
  vector[nc] beta;
  vector[t] alpha; // número de intervalos de follow-up
  vector[N] phi_unscaled;
  real<lower = 0> tau;
  real<lower = 0, upper = 0.95> rho;
}
transformed parameters {
    vector[N] phi;
    phi = phi_unscaled - mean(phi_unscaled);
}
model {
  phi_unscaled ~ sparse_car(tau, rho, W_sparse, D_sparse, lambda, N, W_n);
  rho ~ beta(1, 1);
  beta ~ normal(0, 100);
  tau ~ gamma(1, 1);
  alpha ~ normal(0, 100);
  
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
