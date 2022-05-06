functions {
    real icar_normal_lpdf(vector phi, int[] node1, int[] node2) {
        return -0.5 * dot_self(phi[node1] - phi[node2]);
   }
}


data {
   int<lower=0> N; // número de áreas
   int<lower=0> N_edges;
   int<lower=1, upper=N> node1[N_edges];  // node1[i] adjacent to node2[i]
   int<lower=1, upper=N> node2[N_edges];  // and node1[i] < node2[i]
   int<lower=0> Nd; // número de observações
   int d[Nd];// vetor de mortes observadas 
   int<lower=0> nc; // número de covariáveis
   int<lower=0> t; // número de intervalos analisados (cada um tem o próprio intercepto)
   matrix[Nd, nc] X; // matriz de covariáveis
   vector<lower=0>[Nd] dstar; // número de mortes esperadas
   vector<lower=0>[Nd] y; // tempo em risco dos indivíduos
   int RiskYear[Nd]; // identificação do ano de risco
   int area[Nd]; // identificação única para área
   real<lower=0> scaling_factor; // scales the variance of the spatial effects
}

parameters {
    vector[nc] beta;
    vector[t] alpha;

    real<lower=0> tau; // precisão geral
    real<lower=0, upper=.95> rho;

    vector[N] phi; // efeitos espaciais
    vector[N] theta; // efeitos heterogêneos
}

transformed parameters {
   real<lower=0> sigma = inv(sqrt(tau));

   vector[N] convolved_re;

   // variance of each component should be approximately equal to 1
   convolved_re =  sqrt(1 - rho) * theta + sqrt(rho / scaling_factor) * phi;
}

model {
    alpha ~ normal(0, 100);
    beta ~ normal(0, 100);
    
    tau ~ gamma(1, 1);
    rho ~ beta(0.5, 0.5);

    phi ~ icar_normal_lpdf(node1, node2);
    sum(phi) ~ normal(0, 0.001 * N);
    theta ~ normal(0, 1);

    for (i in 1:Nd) {
        d[i] ~ poisson(dstar[i] + y[i]*exp(alpha[RiskYear[i]] + X[i, ]*beta + sigma*convolved_re[area[i]]));
    }
}

generated quantities {
   vector[Nd] log_lik;

   for(i in 1:Nd) {
      log_lik[i] = poisson_lpmf(d[i] | dstar[i] + y[i]*exp(alpha[RiskYear[i]] + X[i, ]*beta + sigma*convolved_re[area[i]]));
   }
}
