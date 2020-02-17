dgpFun <- function(nTreated,nControl,trueTau) {
  
  n <- nTreated + nControl
  a <- nTreated/nControl
  
  # Generate Xi for all observations
  
  X <- runif(n, min = 0, max = 1)
  
  # Generate Wi appropriately.
  
  W <- rep(0, times = n)
  
  indices <- seq(from = 1, to = n, by = 1)
  
  treatedIndices <- sample(indices, size = nTreated, replace = FALSE)
  
  W[treatedIndices] <- 1
  
  # Generate Yi appropriately
  
  Y <- rnorm(n, mean = 0, sd = 1)
  
  Y[treatedIndices] <- trueTau
  
  # Collapse into single variable
  
  Z <- cbind(Y,X,W)
  
  # Calculate residuals ei ( = Yi - conditional mean of Yi)
  
  ei <- Y # mean of control units is 0
  
  ei[treatedIndices] <- 0 # Treated distribution is degenerate
  
  Z <- cbind(Z,ei)
  
  return(Z)
}
