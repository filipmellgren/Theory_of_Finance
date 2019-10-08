MVcalc <- function(mu_star, mu, Sigma){
  # mustar is the required return for which we find weights that min. variance
  # mu is a vector of asset returns
  # Sigma is the variance covariance matrix.

  n <- length(mu)
  # Define some intermediary scalars that we use for optimising the portfolio
  ones <- rep(1, n)
  A <- t(mu) %*% solve(Sigma) %*% mu
  B <- t(mu) %*% solve(Sigma) %*% ones
  C <- t(ones) %*% solve(Sigma) %*% ones

  A <- as.numeric(A)
  B <- as.numeric(B)
  C <- as.numeric(C)
  
  lambda <- (C * mu_star - B) /(A*C - B^2)
  delta <- (A - B*mu_star) / (A*C - B^2)
  
  w <- rep(0, length(mu_star))
  
  for (ix in 1:length(mu_star)) {
    # Use formula given by professor
    # Index one of the values, don't know atm why 2 works but 1 doesn't
    # 1 gave the wrong slope of the mean var frontier with corr = 1
    w[ix] <- (solve(Sigma) %*% (mu * lambda[ix] + ones * delta[ix]))[2] 
  }
    return(w) 
  }