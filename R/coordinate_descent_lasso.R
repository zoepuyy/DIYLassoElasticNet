#### In a function: simple version that works
coordinate_descent <- function(y, X, lambda = .01, epsilon = .01){

  # get p
  p <- ncol(X)

  # Init all betas = 0
  betas <- numeric(p)

  hasConverged <- FALSE
  run <- 1
  while(!hasConverged){

    #print(run)

    betas_before <- betas # to keep track of convergence

    for (j in 1:p){       # update all betas

      # force Bj = 0
      betas[j] = 0
      # get vector of partial residuals
      r_j <- (y - X %*% betas)

      # get OLS estimate of beta_j*
      beta_star = cov(X[, j], r_j) # we can use cov because y and X are all standardized

      # Update beta_j with soft_thresholding
      betas[j] <- sign(beta_star) * max(c((abs(beta_star) - lambda), 0))

    }
    # check convergence
    # sum(all_False) = 0 so (sum(all_False) == 0) = TRUE !
    hasConverged <- (sum(abs(betas_before - betas) > epsilon) == 0)

    #run <- run+1

  }
  return(betas)
}
