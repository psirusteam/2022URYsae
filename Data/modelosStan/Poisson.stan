data {
  int<lower=0> n;      // Número de áreas geograficas 
  int<lower=0> y[n];   // Conteos por area
}
parameters {
  real<lower=0> theta;
}
model {
  y ~ poisson(theta);
  theta ~ gamma(38, 9);
}
generated quantities {
    real ypred[n];                    // vector de longitud n
    for(ii in 1:n){
    ypred[ii] = poisson_rng(theta);
    }
}
