############################################################
# 1. Setup
############################################################

rm(list = ls())
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(123)

############################################################
# 2. Tamaños
############################################################

nA <- 50
nB <- 60
K  <- 3

n_match_true <- 30   # matches reales

############################################################
# 3. Parámetros verdaderos
############################################################

pi_true <- n_match_true / (nA * nB)

mu_true     <- c(0, 2, -1)
sigma2_true <- c(1, 1.5, 0.8)
tau2_true   <- c(2, 3, 1.5)

############################################################
# 4. Simular lista A
############################################################

A <- matrix(0, nA, K)

for(i in 1:nA){
  for(k in 1:K){
    mu0 <- rnorm(1, mu_true[k], sqrt(tau2_true[k]))
    A[i,k] <- rnorm(1, mu0, sqrt(sigma2_true[k]))
  }
}

############################################################
# 5. Simular lista B
############################################################

B <- matrix(0, nB, K)

# Matches verdaderos
for(i in 1:n_match_true){
  for(k in 1:K){
    mu0 <- rnorm(1, mu_true[k], sqrt(tau2_true[k]))
    A[i,k] <- rnorm(1, mu0, sqrt(sigma2_true[k]))
    B[i,k] <- rnorm(1, mu0, sqrt(sigma2_true[k]))
  }
}

# No matches
for(i in (n_match_true+1):nB){
  for(k in 1:K){
    mu0 <- rnorm(1, mu_true[k], sqrt(tau2_true[k]))
    B[i,k] <- rnorm(1, mu0, sqrt(sigma2_true[k]))
  }
}

############################################################
# 6. Crear TODOS los pares
############################################################

pairs <- expand.grid(idA = 1:nA, idB = 1:nB)

N <- nrow(pairs)  # 3000

XA <- matrix(0, N, K)
XB <- matrix(0, N, K)

for(i in 1:N){
  XA[i,] <- A[pairs$idA[i], ]
  XB[i,] <- B[pairs$idB[i], ]
}

############################################################
# 7. Verdad
############################################################

pairs$true_match <- ifelse(
  pairs$idA == pairs$idB & pairs$idA <= n_match_true,
  1, 0
)

############################################################
# 8. STAN MODEL (Copas–Hilton continuo)
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

  pi ~ beta(1,10);
  mu ~ normal(0,5);
  sigma2 ~ inv_gamma(2,2);
  tau2 ~ inv_gamma(2,2);

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

    target += log_sum_exp(log_m, log_u);
  }
}

generated quantities {

  vector[N] p_match;

  for(i in 1:N){

    real log_m = log(pi);
    real log_u = log1m(pi);

    for(k in 1:K){

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
# 9. Ajustar modelo
############################################################

fit <- stan(
  model_code = stan_code,
  data = list(N=N, K=K, XA=XA, XB=XB),
  iter = 1500,
  chains = 2,
  seed = 123
)

############################################################
# 10. Clasificación
############################################################

post <- rstan::extract(fit)

p_match <- colMeans(post$p_match)

pairs$p_match <- p_match
pairs$pred_match <- ifelse(p_match > 0.5, 1, 0)

############################################################
# 11. Evaluación
############################################################

cat("\n--- MATRIZ DE CONFUSIÓN ---\n")
print(table(Predicho = pairs$pred_match,
            Verdadero = pairs$true_match))

accuracy <- mean(pairs$pred_match == pairs$true_match)
cat("\nAccuracy:", accuracy, "\n")

############################################################
# 12. Visualización
############################################################

hist(pairs$p_match[pairs$true_match==1],
     col=rgb(0,1,0,0.5), main="p_match", xlab="")

hist(pairs$p_match[pairs$true_match==0],
     col=rgb(1,0,0,0.5), add=TRUE)