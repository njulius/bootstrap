# A function to find the variance of the matching estimator for the ATET
# conditional on X and W in A&I's baby DGP.

# Under this DGP, the conditional variance takes a simple form:
# Var(tauhat|X,W) = (N1)^-2 * (sum over i of Ki^2) where Ki is a deterministic
# function of the matches made.

condVar <- function(matches) {
  # The number of rows in the matching matrix is the number of control units
  n0 <- length(matches[,1])
  
  # The number of columns in the matching matrix is the number of treated units
  n1 <- length(matches[1,])
  
  # For each control unit, calculate Ki and store
  # Note: I'm ignoring ties here because I don't think I'll ever use this on
  # a bootstrapped dataset (on purpose, at least). Dealing with ties is trivial
  # if I nest loops, but that will drastically slow down the function.
  
  K <- apply(matches, 1, sum, na.rm=TRUE)
  
  condVar <- (n1)^(-2) * sum(K^2)
  
  return(condVar)
  
}
