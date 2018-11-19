library(ggplot2)
library(tidyr)

# Clean Global Environment
rm(list = ls())

# Get Data
source("R/simulation_data.R")

# Get Lasso Solver
source("R/coordinate_descent_lasso.R")

betas.lasso <- t(mapply(lasso.solve, lambda = seq(0, 1, .05), MoreArgs = list(y = y, X = X)))
betas.lasso <- data.frame(betas.lasso)
betas.lasso$lambda <- seq(0, 1, .05)

betas.lasso <- betas.lasso %>%
  tidyr::gather(key = "Betas", value = "value", 1:8)

# Plot
betas.lasso %>%
  ggplot(aes(x = lambda)) +
  geom_line(aes(y = value, color = Betas)) +
  ggtitle("Coefficient Profiles - Lasso", subtitle = "Simulated data.")
