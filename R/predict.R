# Predict Function
predict <- function(betas, new_data){
  return(new_data%*%betas)
}
