# Clean Global Environment
library(ggplot2)
library(magrittr)
rm(list = ls())

# Get Data
source("R/simulation_data.R")
# Get Lasso Solver
source("R/coordinate_descent_lasso.R")
# Get Predict Function
source("R/predict.R")

# Pick Lambda using cross-validation

cv.lasso <- function(lambda_max = 5,
                     step_lambda = .1,
                     n_folds = 10,
                     y,
                     X,
                     one_stderr_rule = TRUE){

  if (nrow(X) %% n_folds != 0){
    while (nrow(X) %% n_folds != 0){ # not divisible by nfolds
      n_folds <- n_folds - 1
    }
    warning(paste0("The data cannot be split into the required numbers of folds. Using ", n_folds, " folds..."))
  }


  folds <- sample(rep(1:n_folds, nrow(X) / n_folds), size = length(y))

  cv.one_pass <- function(lambda){
    errors <- numeric(n_folds)
    for (i in 1:n_folds){

      Xtest = X[folds == i, ]
      ytest = y[folds == i]
      Xtrain = X[folds != i, ]
      ytrain = y[folds != i]

      beta.lasso <- lasso.solve(ytrain, Xtrain, lambda = lambda)
      ypred <- predict(beta.lasso, Xtest)

      errors[i] <- mean((ytest - ypred)^2)
    }
    return(list(error = mean(errors), std = sd(errors)))
  }

  lambdas <- seq(0, lambda_max, step_lambda)

  df <- data.frame(lambda = lambdas,
                   estimated_error = numeric(length(lambdas)),
                   std = numeric(length(lambdas)))

  for (l in 1:length(df$lambda)){
    df$estimated_error[l] <- cv.one_pass(lambda = df$lambda[l])$error
    df$std[l] <- cv.one_pass(lambda = df$lambda[l])$std
  }

  df$ci_up <- df$estimated_error + df$std
  df$ci_down <- df$estimated_error - df$std


  if (!one_stderr_rule){

    best_lambda <- df$lambda[which.min(df$estimated_error)]

  } else {

    thresh <- df$ci_up[which.min(df$estimated_error)]

    best_lambda <- max(df$lambda[df$estimated_error <= thresh])

  }

  df$is_best <- df$lambda == best_lambda

  cat("Best lambda is", best_lambda)

  return(list(errors = df, best_lambda = best_lambda))

}

plot_cv_lasso <- function(cv_results){

  dta <- cv_results$errors

  dta %>%
    ggplot(aes(x = lambda)) +
    geom_point(aes(y = estimated_error, color = is_best), size = 1, pch = 3) +
    geom_point(aes(y = ci_up), color = "red", size = 1, pch = 3) +
    geom_point(aes(y = ci_down), color = "red", size = 1, pch = 3) +
    geom_hline(yintercept = dta[which.min(dta$estimated_error), "ci_up"],
               color = "grey", size = .2) +
    scale_color_manual(name = "Best Lambda",
                       values = c("TRUE" = "green", "FALSE" = "black")) +
    geom_segment(aes(x=cv_results$best_lambda, xend=cv_results$best_lambda,
                     y=0, yend = dta[dta$is_best, "estimated_error"]),
                 lty = "dashed", size = .25, color = "green") +
    ylab("10-fold average cross-validation error")

}

cv.lasso(lambda_max = 3, step_lambda  = .1, n_folds = 10, y = y, X = X, TRUE) %>%
  plot_cv_lasso()
