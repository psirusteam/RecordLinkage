############################################################
# 1. Librerías
############################################################

rm(list = ls())

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(123)

############################################################
# 2. Crear listas A y B
############################################################

nA <- 50
nB <- 60

nombres <- c("Juan","Maria","Pedro","Ana","Luis","Carlos",
             "Lucia","Jorge","Sofia","Miguel","Laura")

apellidos <- c("Gomez","Perez","Rodriguez","Lopez","Garcia")

sexos <- c("M","F")

A <- data.frame(
  idA = 1:nA,
  nombre = sample(nombres,nA,replace=TRUE),
  apellido = sample(apellidos,nA,replace=TRUE),
  edad = sample(20:60,nA,replace=TRUE),
  sexo = sample(sexos,nA,replace=TRUE)
)

n_match_true <- 30

B_match <- data.frame(
  idB = 1:n_match_true,
  nombre = A$nombre[1:n_match_true],
  apellido = A$apellido[1:n_match_true],
  edad = A$edad[1:n_match_true] + sample(c(-1,0,1),n_match_true,replace=TRUE),
  sexo = A$sexo[1:n_match_true]
)

B_extra <- data.frame(
  idB = (n_match_true+1):nB,
  nombre = sample(nombres,nB-n_match_true,replace=TRUE),
  apellido = sample(apellidos,nB-n_match_true,replace=TRUE),
  edad = sample(20:60,nB-n_match_true,replace=TRUE),
  sexo = sample(sexos,nB-n_match_true,replace=TRUE)
)

B <- rbind(B_match,B_extra)

############################################################
# 3. Crear pares
############################################################

pairs <- merge(A,B,by=NULL)
N <- nrow(pairs)

############################################################
# 4. Variables de comparación
############################################################

pairs$nombre_match <- ifelse(pairs$nombre.x == pairs$nombre.y,1,0)
pairs$apellido_match <- ifelse(pairs$apellido.x == pairs$apellido.y,1,0)
pairs$edad_match <- ifelse(abs(pairs$edad.x - pairs$edad.y)<=1,1,0)
pairs$sexo_match <- ifelse(pairs$sexo.x == pairs$sexo.y,1,0)

X <- as.matrix(pairs[,c(
  "nombre_match",
  "apellido_match",
  "edad_match",
  "sexo_match"
)])

K <- ncol(X)

############################################################
# 5. Construir conteos para f_k(y)
############################################################

build_counts <- function(vecA, vecB){
  tab <- table(c(vecA, vecB))
  list(
    values = names(tab),
    counts = as.integer(tab)
  )
}

c1 <- build_counts(A$nombre, B$nombre)
c2 <- build_counts(A$apellido, B$apellido)
c3 <- build_counts(A$edad, B$edad)
c4 <- build_counts(A$sexo, B$sexo)

counts_list <- list(c1, c2, c3, c4)

# Número de categorías distintas en cada variable
C <- sapply(counts_list, function(x) length(x$counts))
# Número máximo de categorías distintas en cada variable
C_max <- max(C)

counts_mat <- matrix(0, K, C_max)

for(k in 1:K){
  counts_mat[k,1:C[k]] <- counts_list[[k]]$counts
}

############################################################
# 6. Verdad
############################################################

pairs$true_match <- ifelse(
  pairs$idA <= n_match_true &
    pairs$idB == pairs$idA,
  1,0)

############################################################
# 7. Stan FULL Bayes
############################################################

stan_code <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  int<lower=1> C[K];
  int<lower=1> C_max;
  int<lower=0> counts[K, C_max];
  int<lower=0,upper=1> X[N,K];
}

parameters {
  vector<lower=0,upper=1>[K] alpha;
  simplex[C_max] f[K];
  real<lower=0,upper=1> pi;
}

transformed parameters {
  vector[K] u;
  vector[K] m;

  for (k in 1:K) {

    // u_k = sum f_k(y)^2 (solo categorias reales)
    // dot_self producto punto sum_k f_k^2
    u[k] = dot_self(f[k][1:C[k]]);

    // m_k segun Copas-Hilton
    m[k] = square(alpha[k]) + square(1 - alpha[k]) * u[k];
  }
}

model {

  // Priors
  alpha ~ beta(8,2);
  pi ~ beta(1,50);

  // f_k con Dirichlet + datos (conjugado)
  for (k in 1:K) {

    vector[C_max] alpha_dir;

    // evitar masa en categorias inexistentes
    alpha_dir = rep_vector(1e-10, C_max);

    for (c in 1:C[k]) {
      alpha_dir[c] = 1 + counts[k,c];
    }

    f[k] ~ dirichlet(alpha_dir);
  }

  // Likelihood de linkage
  for (i in 1:N) {

    real log_m = log(pi);
    real log_u = log1m(pi);

    for (k in 1:K) {
      log_m += bernoulli_lpmf(X[i,k] | m[k]);
      log_u += bernoulli_lpmf(X[i,k] | u[k]);
    }

    target += log_sum_exp(log_m, log_u);
  }
}

generated quantities {
  vector[N] p_match;

  for (i in 1:N) {

    real log_m = log(pi);
    real log_u = log1m(pi);

    for (k in 1:K) {
      log_m += bernoulli_lpmf(X[i,k] | m[k]);
      log_u += bernoulli_lpmf(X[i,k] | u[k]);
    }

    p_match[i] = inv_logit(log_m - log_u);
  }
}
"

############################################################
# 8. Ajustar modelo
############################################################

fit <- stan(
  model_code = stan_code,
  data = list(
    N=N,
    K=K,
    C=C,
    C_max=C_max,
    counts=counts_mat,
    X=X
  ),
  iter = 1000,
  chains = 1,
  seed = 123
)

print(fit, pars=c("alpha","pi","u","m"))

############################################################
# 9. Resultados
############################################################

post <- rstan::extract(fit)
pairs$p_match <- colMeans(post$p_match)
pairs$bayes_match <- ifelse(pairs$p_match > 0.8,1,0)

table(pairs$bayes_match, pairs$true_match)
