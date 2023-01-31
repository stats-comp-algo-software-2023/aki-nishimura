calc_linear_loglik <- function(reg_coef, design, outcome, noise_var = 1) {
  predicted_val <- design %*% reg_coef
  loglik <- - 0.5 * sum((outcome - predicted_val)^2) / noise_var
  return(loglik)
}

calc_linear_grad <- function(reg_coef, design, outcome, noise_var = 1) {
  predicted_val <- design %*% reg_coef
  grad <- t(design) %*% (outcome - predicted_val)
  grad <- as.vector(grad)
  return(grad)
}