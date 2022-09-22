data {
  int<lower=0> n;     // Número de observaciones
  real y[n];          // LogIngreso 
  real <lower=0> Sigma;  // Desviación estándar   
}
parameters {
  real theta;
}
model {
  y ~ normal(theta, Sigma);
  theta ~ normal(0, 1000); // distribución previa
}
generated quantities {
    real ypred[n];                    // vector de longitud D
    for(kk in 1:n){
    ypred[kk] = normal_rng(theta,Sigma);
}
}
