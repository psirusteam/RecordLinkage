############################################################
# 1. Librerías
############################################################

rm(list = ls())

library(rstan)
library(fastLink)

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
# 3. Crear todos los pares
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
# 5. Verdad simulada
############################################################

pairs$true_match <- ifelse(
  pairs$idA <= n_match_true &
    pairs$idB == pairs$idA,
  1,0)

############################################################
# 6. fastLink
############################################################

A$edad <- as.numeric(A$edad)
B$edad <- as.numeric(B$edad)

A$nombre <- as.character(A$nombre)
B$nombre <- as.character(B$nombre)

A$apellido <- as.character(A$apellido)
B$apellido <- as.character(B$apellido)

A$sexo <- as.character(A$sexo)
B$sexo <- as.character(B$sexo)

fl <- fastLink(
  dfA = A,
  dfB = B,
  varnames = c("nombre","apellido","edad","sexo"),
  stringdist.match = c("nombre","apellido"),
  numeric.match = c("edad")
)

matches_fastlink <- data.frame(
  idA = fl$matches[,1],
  idB = fl$matches[,2]
)

############################################################
# 7. Modelo Fellegi-Sunter bayesiano en Stan
############################################################

stan_code <- "
data {

  int<lower=1> N;               // numero de pares
  int<lower=1> K;               // numero de variables de comparacion
  int<lower=0,upper=1> X[N,K];  // matriz de acuerdos

}

parameters {

  vector<lower=0,upper=1>[K] m;   // probabilidad de acuerdo dado match
  vector<lower=0,upper=1>[K] u;   // probabilidad de acuerdo dado no-match
  real<lower=0,upper=1> pi;       // proporcion de matches

}

model {

  real log_m;
  real log_u;

  m ~ beta(8,2);
  u ~ beta(2,8);
  pi ~ beta(1,5);

  for(i in 1:N){

    log_m = 0;
    log_u = 0;

    for(k in 1:K){

      log_m += bernoulli_lpmf(X[i,k] | m[k]);
      log_u += bernoulli_lpmf(X[i,k] | u[k]);

    }

    target += log_sum_exp(
                 log(pi) + log_m,
                 log1m(pi) + log_u
               );

  }

}

"


############################################################
# 8. Ajustar modelo
############################################################

data_list <- list(
  N = N,
  K = K,
  X = X
)

fit <- stan(
  model_code = stan_code,
  data = data_list,
  iter = 2000,
  chains = 4,
  seed = 123
)

############################################################
# 9. Probabilidad posterior de match
############################################################

post <- rstan::extract(fit)

m_hat <- colMeans(post$m)
u_hat <- colMeans(post$u)
pi_hat <- mean(post$pi)

p_match <- rep(0,N)

for(i in 1:N){
  
  log_m <- log(pi_hat)
  log_u <- log(1-pi_hat)
  
  for(k in 1:K){
    
    log_m <- log_m + dbinom(X[i,k],1,m_hat[k],log=TRUE)
    log_u <- log_u + dbinom(X[i,k],1,u_hat[k],log=TRUE)
    
  }
  
  p_match[i] <- exp(log_m)/(exp(log_m)+exp(log_u))
  
}

pairs$p_match <- p_match
pairs$bayes_match <- ifelse(pairs$p_match > 0.5,1,0)

matches_bayes <- pairs[pairs$bayes_match==1,c("idA","idB")]

############################################################
# 10. Comparar métodos
############################################################

matches_fastlink$fastlink <- 1
matches_bayes$bayes <- 1

comparison <- merge(
  matches_fastlink,
  matches_bayes,
  by=c("idA","idB"),
  all=TRUE
)

comparison$fastlink[is.na(comparison$fastlink)] <- 0
comparison$bayes[is.na(comparison$bayes)] <- 0

table(
  FastLink = comparison$fastlink,
  Bayes = comparison$bayes
)

############################################################
# 11. Comparación con verdad
############################################################

pairs$fastlink_match <- 0

pairs$fastlink_match[
  paste(pairs$idA,pairs$idB) %in%
    paste(matches_fastlink$idA,matches_fastlink$idB)
] <- 1

table(FastLink = pairs$fastlink_match,
      Verdadero = pairs$true_match)

table(Bayes = pairs$bayes_match,
      Verdadero = pairs$true_match)
