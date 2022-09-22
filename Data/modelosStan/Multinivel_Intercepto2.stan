data {
  int <lower = 0>N;                       // Número de observaciones
  int <lower = 0>k;                       // Número de covariables 
  vector[N] y;                          // Variables respuesta
  matrix [N,k] X;                        // Variables regresoras
  // efecto aleatorio 
  int <lower = 0>kz; 
  matrix [N,kz] Z;                        // Variables regresoras
}

parameters {
  vector[k] beta;         // coeficientes del modelo
  vector[kz] u;         // coeficientes del modelo
  real <lower = 0> sigma2;
  
}

transformed parameters{
 vector[N] mu;
 real<lower=0> sigma;
 sigma = sqrt(sigma2);
 mu =  X*beta + Z * u;
}

model {
  beta ~ normal(0,1000);
  u ~ normal(0,1000);
  sigma2 ~ inv_gamma(0.0001, 0.0001);
  y ~ normal(mu , sigma);
}

generated quantities {
    real ypred[N];                    // vector de longitud n
    vector [N]lp; 
    lp =  X*beta + Z * u;
    ypred =  normal_rng(lp , sigma);
}
