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
}

parameters {
   vector[t] alpha; // número de intervalos de follow-up
   vector[nc] beta; // vetor de parâmetros
   real<lower=0> tau; // precisão dos erros
   vector[N] phi;
}

model {
   // priori
   tau ~ gamma(1, 1);
   beta ~ normal(0, 100);
   alpha ~ normal(0, 100);
   phi ~ normal(0, sqrt(1/tau));

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
