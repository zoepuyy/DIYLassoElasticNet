# In a function: simple version that works
# Assumes that X and y have already been properly standardized.
elasticNet.solve <- function(y, X, lambda_l1 = .01, lambda_l2 = .01, epsilon = .01){

  # rescale data
  X <- scale(X)
  y <- y - mean(y)

  # get p
  p <- ncol(X)

  # Init all betas = 0
  betas <- numeric(p)

  # Set hasConverged to False for now
  hasConverged <- FALSE

  #Keep track of the runs (useful for debugging)
  run <- 1

  while(!hasConverged){

    #print(run)

    betas_before <- betas # to keep track of convergence

    # update all betas until they converge

    for (j in 1:p){

      # force Bj = 0
      betas[j] = 0

      # get vector of partial residuals
      r_j <- (y - X %*% betas)

      # get OLS estimate of beta_j*
      # we can use cov because y and X are all standardized
      beta_star = cov(X[, j], r_j)

      # Update beta_j with soft_thresholding
      betas[j] <- sign(beta_star) * max(c((abs(beta_star) - lambda_l1), 0)) / (1 + lambda_l2)

    }

    # check convergence (no beta moved more than epsilon)
    # NB: sum(c(FALSE, FALSE, FALSE)) => 0
    hasConverged <- (sum(abs(betas_before - betas) > epsilon) == 0)

    #run <- run+1

  }
  return(betas)
}
