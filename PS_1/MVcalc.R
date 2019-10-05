MVcalc <- function(mu_star, mu, Sigma){
  # mustar is the required return for which we find weights that min. variance
  # mu is a vector of asset returns
  # Sigma is the variance covariance matrix.

  
  # Define some intermediary scalars that we use for optimising the portfolio
  ones <- rep(1, n)
  A <- t(mu) %*% solve(Sigma) %*% mu
  B <- t(mu) %*% solve(Sigma) %*% ones
  C <- ones %*% solve(Sigma) %*% ones
  
  A <- as.numeric(A)
  B <- as.numeric(B)
  C <- as.numeric(C)
  
  lambda <- (C * mu_star - B) /(A*C - B^2)
  delta <- (A - B*mu_star) / (A*C - B^2)
  
  w <- solve(Sigma) %*% (mu * lambda + ones * delta)
  StdRp = sqrt(t(w)%*%Sigma%*%w)
  
  # Check that the w sums to (almost bc aproximating) one
  if(sum(w) - 1 < 1e-10){
    return(w) 
  }
  else {
    return("error")
  }
}