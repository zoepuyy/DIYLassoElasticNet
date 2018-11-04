# Simulate Data
#### "true" dgp:
#### y = XB + e, e ~ N(0, sig)
#### Let β = (3, 1.5, 0, 0, 2, 0, 0, 0)T and σ = 3. The pairwise correlation between xi and xj
# was set to be corr(i, j) = 0.5^|i−j|

library(mvtnorm) # multivariate normal distribution

# Betas
betas <- c(3, 1.5, 0, 0, 2, 0, 0, 0)

# Variance-Covariance of X
Var_X <- diag(1, nrow = 8, ncol = 8)
for (i in 1:8){
  for (j in 1:8){
    Var_X[i, j] <- .5^(abs(i - j))
  }
}

# X
X <- mvtnorm::rmvnorm(240, sigma = Var_X)

# y
y <- X %*% betas + rnorm(240, 0, 3)



source("R/coordinate_descent_lasso.R")
coordinate_descent(y, X, lambda = .5)
