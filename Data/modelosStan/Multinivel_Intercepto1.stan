data {
  int <lower = 0>N;                       // Número de observaciones
  int <lower = 0>k;                       // Número de covariables 
  int  Nv1;                               // Número de grupos
  int  Grupo[N];                         // Ids de grupos
  vector[N] y;                          // Variables respuesta
  matrix [N,k] X;                        // Variables regresoras
}

parameters {
  vector[Nv1] beta0;       // coeficientes del modelo
  vector[k] beta1;         // coeficientes del modelo
  real <lower = 0> sigma2;
}

transformed parameters{
 vector[N] mu;
 real<lower=0> sigma;
 sigma = sqrt(sigma2);
 mu =  beta0[Grupo] + X*beta1;
}

model {
  beta0 ~ normal(0,100);
  beta1 ~ normal(0,100);
  sigma2 ~ inv_gamma(0.0001, 0.0001);
  y ~ normal(mu , sigma);
  
}
generated quantities {
    real ypred[N];                    // vector de longitud n
    ypred =  normal_rng(mu , sigma);
}

