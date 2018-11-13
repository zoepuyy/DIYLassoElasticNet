library(mvtnorm) # multivariate normal distribution

# p <- 30
#
# beta_sparsity <- .3 # % of non_zero coefficients
#
# n_train <- 20
# n_validation <- n_train
# n_test <- 200
# num_datasets <- 50
# n_total_per_dataset <- (n_train + n_validation + n_test)
# n_total <- n_total_per_dataset * num_datasets
#
#
#
# sigma_error <- 3

createDataSet <- function(p, beta_sparsity, n_train, n_validation, n_test, num_datasets){

  n_total_per_dataset <- (n_train + n_validation + n_test)
  n_total <- n_total_per_dataset * num_datasets

  ## Betas
  # Init all betas = 0
  betas <- numeric(p)
  # List potential values for beta:
  potential_betas <- seq(-10, 10, by = .5)[-21] # remove zero !

  # Pick nonzero betas (with prob = beta_sparsity)
  non_zero_betas <- sample(c(TRUE, FALSE), size = p, replace = TRUE, prob = c(beta_sparsity, 1- beta_sparsity))

  # Assign random value to non-zero betas
  betas[non_zero_betas] <- sample(potential_betas, size = sum(non_zero_betas), replace = TRUE)



  ## Variance-Covariance of X
  Var_X <- diag(1, nrow = p, ncol = p)
  for (i in 1:p){
    for (j in 1:p){
      Var_X[i, j] <- .5^(abs(i - j)) # we could change this too!
    }
  }

  # X
  X <- mvtnorm::rmvnorm(n_total, sigma = Var_X)

  # y
  y <- X %*% betas + rnorm(n_total, 0, sigma_error)

  # rescale y
  y <- y / sd(y)

  # Save and export data:
  # Put it in dataframe and do train/validate/test split
  df <- data.frame(cbind(y, X))
  colnames(df) <- c("y", paste0("X", 1:p))

  df$dataset_number <- rep(1:num_datasets, each = n_total_per_dataset)

  df$type_data <- rep(rep(c("training", "validation", "test"), times = c(n_train, n_validation, n_test)),
                      times = num_datasets)

  # Give it a meaningful name

  dataset_name <- sprintf("data/data-p_%i_sparsity_%.2f_sigm_%.2f_ntrain_%i.csv",
                          p, beta_sparsity, sigma_error, n_train)

  write.csv(df, file = dataset_name, row.names = F)
}
