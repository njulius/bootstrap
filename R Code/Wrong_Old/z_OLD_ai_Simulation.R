# This is no longer used

# Abadie & Imbens (2008) Simulation
# Author: Nik Julius
# Last Modified: July 31st, 2016

# This essentially shows the Abadie & Imbens result. We generate data according to their DGP, use 
# their asymptotic approximation to determine the variance of the matching estimator, and contrast
# this result with the variance estimated by the naive bootstrap process.

aiSimulation <- function(numObs, treatRatio, trueTau, numBoots) {
  # Variables
  #
  # numObs, treatRatio, and trueTau are the variables to be fed into genData(), and have the same
  # interpretation. numBoots is a scalar telling us how many bootstraps to run.
  
  # First, generate the dataset.
  Z <- genData(numObs, treatRatio, trueTau)
  
  # The asymptotic variance can be consistently estimated according to the formula found on page 1540
  # of Abadie & Imbens (2008). For this formula, we need to know n1, tauHat, Ki^2, Ksq_i, and sigma2hat
  # of Xi and Wi.
  
  # In the case of the DGP Abadie & Imbens use for their example, this reduces significantly. To start with,
  # the average treatment effect is equal to trueTau for all X, as is the average treatment effect on
  # the treated. Since the Y[i]'s are mean zero conditional on W[i] = 0 and have unit variance, E[tauHat|X,W]
  # is equal to trueTau.
  
  # Because Y[i] and Y[j] are independent for i != j, E[Y[i]^2|W[i] = 0, X,W] = 1. Finallly, since Ki is a
  # deterministic function of X and W, it follows that Var(tauHat|X,W) = (n1)^-2 * sum(Ki^2) for i=1,2,...,n.
  
  # Thus, Var(tauHat) = E[Var(tauHat|X,W)] = (n0/(n1^2)) * E[Ki^2|W[i] = 0]. The DGP used allows for a closed
  # form solution to be found. We need to know only n1 and n0 to calculate it.
  
  temp1 <- Z[which(Z[,2] == 1),]
  temp2 <- Z[which(Z[,2] == 0),]
  
  n1 <- length(temp1[,1])
  n0 <- length(temp2[,1])
  
  exactVar <- (1/n1) + (3/2) * ((n1-1) * (n0 + 8/3))/(n1 * (n0 + 1) * (n0 + 2))
  
  # Now we perform numBoots bootstraps and create a vector of the bootstrapped tauHats.
  
  bTauHats <- rep(0, times = numBoots)
  
  for(i in 1:numBoots) {
    bTauHats[i]  <- naiveBootstrap(Z)
  }
  
  # Calculate bootstrapped variance
  
  bootVar <- var(bTauHats)
  
  # As in Abadie & Imbens, the bootstrap is a valid estimator if n1(E[(tauHat_b - tauHat)^2] - Var(tauHat))
  # converges in probability to 0.  Thus, determine and return this value.
  
  intermediateTerm <- (bTauHats - trueTau)^2
  intermediateTerm2 <- mean(intermediateTerm)
  
  out <- n1 * (intermediateTerm2 - exactVar)
  
  return(out)
  
}
