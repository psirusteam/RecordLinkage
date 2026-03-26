############################################################
# 1. Setup
############################################################

rm(list = ls())
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(123)

############################################################
# 2. Parámetros verdaderos
############################################################

N <- 400
K <- 2

pi_true <- 0.3

mu_true <- c(0, 1)
sigma2_true <- c(1, 1.5)
tau2_true   <- c(3, 2)

############################################################
# 3. Simular Z
############################################################

Z <- rbinom(N, 1, pi_true)

############################################################
# 4. Simular datos continuos
############################################################

XA <- matrix(0, N, K)
XB <- matrix(0, N, K)

for(i in 1:N){
  for(k in 1:K){
    
    if(Z[i] == 1){
      # MATCH
      mu0 <- rnorm(1, mu_true[k], sqrt(tau2_true[k]))
      XA[i,k] <- rnorm(1, mu0, sqrt(sigma2_true[k]))
      XB[i,k] <- rnorm(1, mu0, sqrt(sigma2_true[k]))
    } else {
      # NO MATCH
      muA <- rnorm(1, mu_true[k], sqrt(tau2_true[k]))
      muB <- rnorm(1, mu_true[k], sqrt(tau2_true[k]))
      XA[i,k] <- rnorm(1, muA, sqrt(sigma2_true[k]))
      XB[i,k] <- rnorm(1, muB, sqrt(sigma2_true[k]))
    }
    
  }
}

############################################################
# 5. Modelo STAN (Copas–Hilton continuo)
############################################################

stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N,K] XA;
  matrix[N,K] XB;
}

parameters {
  real<lower=0,upper=1> pi;
  vector[K] mu;
  vector<lower=0>[K] sigma2;
  vector<lower=0>[K] tau2;
}

model {

  // Priors
  pi ~ beta(1,5);
  mu ~ normal(0,5);
  sigma2 ~ inv_gamma(2,2);
  tau2 ~ inv_gamma(2,2);

  // Likelihood
  for(i in 1:N){

    real log_m = log(pi);
    real log_u = log1m(pi);

    for(k in 1:K){

      // =========================
      // MATCH (bivariada)
      // =========================
      {
        real var_k = sigma2[k] + tau2[k];
        real cov_k = tau2[k];

        matrix[2,2] Sigma;
        Sigma[1,1] = var_k;
        Sigma[2,2] = var_k;
        Sigma[1,2] = cov_k;
        Sigma[2,1] = cov_k;

        vector[2] x;
        vector[2] mu_vec;

        x[1] = XA[i,k];
        x[2] = XB[i,k];

        mu_vec[1] = mu[k];
        mu_vec[2] = mu[k];

        log_m += multi_normal_lpdf(x | mu_vec, Sigma);
      }

      // =========================
      // NO MATCH (independientes)
      // =========================
      {
        real var_k = sigma2[k] + tau2[k];

        log_u += normal_lpdf(XA[i,k] | mu[k], sqrt(var_k));
        log_u += normal_lpdf(XB[i,k] | mu[k], sqrt(var_k));
      }

    }

    target += log_sum_exp(log_m, log_u);
  }
}

generated quantities {

  vector[N] p_match;

  for(i in 1:N){

    real log_m = log(pi);
    real log_u = log1m(pi);

    for(k in 1:K){

      // MATCH
      {
        real var_k = sigma2[k] + tau2[k];
        real cov_k = tau2[k];

        matrix[2,2] Sigma;
        Sigma[1,1] = var_k;
        Sigma[2,2] = var_k;
        Sigma[1,2] = cov_k;
        Sigma[2,1] = cov_k;

        vector[2] x;
        vector[2] mu_vec;

        x[1] = XA[i,k];
        x[2] = XB[i,k];

        mu_vec[1] = mu[k];
        mu_vec[2] = mu[k];

        log_m += multi_normal_lpdf(x | mu_vec, Sigma);
      }

      // NO MATCH
      {
        real var_k = sigma2[k] + tau2[k];

        log_u += normal_lpdf(XA[i,k] | mu[k], sqrt(var_k));
        log_u += normal_lpdf(XB[i,k] | mu[k], sqrt(var_k));
      }

    }

    p_match[i] = inv_logit(log_m - log_u);
  }

}
"

############################################################
# 6. Ajustar modelo
############################################################

fit <- stan(
  model_code = stan_code,
  data = list(N=N, K=K, XA=XA, XB=XB),
  iter = 2000,
  chains = 4,
  seed = 123
)

############################################################
# 7. Resultados
############################################################

print(fit, pars=c("pi","mu","sigma2","tau2"))

post <- rstan::extract(fit)

pi_est <- mean(post$pi)
mu_est <- colMeans(post$mu)
sigma2_est <- colMeans(post$sigma2)
tau2_est <- colMeans(post$tau2)

cat("\n--- COMPARACIÓN ---\n")

cat("\npi:\n")
print(c(real=pi_true, estimado=pi_est))

cat("\nmu:\n")
print(cbind(real=mu_true, estimado=mu_est))

cat("\nsigma2:\n")
print(cbind(real=sigma2_true, estimado=sigma2_est))

cat("\ntau2:\n")
print(cbind(real=tau2_true, estimado=tau2_est))
