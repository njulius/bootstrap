# Abadie & Imbens (2008) Simulation
# Author: Nik Julius
# Last Modified: June 18, 2016

# A function to generate data according to Assumptions A3.1 through A3.4 in A&I (2008)

dgpFun <- function(numObs, treatRatio, trueTau) {
  # Variables
  #
  # numObs should be a scalar and represents the number of observations in the dataset
  # to be generated (N in A&I (2008))
  #
  # treatRatio should be a scalar and represents the value of the number of treated units
  # divided by the number of control units (a in A&I (2008))
  #
  # trueTau should be a scalar and represents the true value of the treatment effect (tau 
  # in A&I (2008))
  
  n <- numObs
  a <- treatRatio
  t <- trueTau
  
  # Generate X[i] for i=1,2,...,n
  
  X <- runif(n, min = 0, max = 1)
  
  # Generate W[i]'s such that the treated to control ratio is correct
  # Note that a is a fixed value. Assumption 3.3 in A&I (2008) suggests
  # that we could just assign W[i] = 1 with probability (a / (1+a)), but this
  # will result in a not actually being equal to N1/N0 in general.
  
  # Instead, given a and n, we know the values of N1 and N0. Thus, we sample
  # N1 random values from (1,2,...,n) and assign W[i] = 1 to the sampled indices.
  
  W <- rep(0, times = n)
  
  n1 <- round((a / (1+a)) * n)
  
  # Create sequence of indices to sample from
  indices <- seq(from = 1, to = n, by = 1)
  
  # Sample n1 indices without replacement
  treatedIndices <- sample(indices, size = n1, replace = FALSE)
  
  # Assign W[i] = 1 to treated indices
  
  for(i in 1:length(treatedIndices)) {
    W[treatedIndices[i]] <- 1
  }
  
  # Generate Y[i]'s such that they are distributed degenerately with
  # P[Yi(1) = t] = 1 and the conditional distribution of Yi(0) given Xi
  # i s N(0,1)
  
  Y = rnorm(n, mean = 0, sd = 1)
  
  for(i in 1:length(Y)) {
    if(W[i] == 1) {
      Y[i] <- t
    } else {
      Y[i] <- Y[i]
    }
  }
  
  # Collapse into single variable
  
  Z <- cbind(X,W,Y)
  
  # Stack sample with treated units from i=1 to N1, control units after.
  
  tSample <- Z[which(Z[,2] == 1),]
  cSample <- Z[which(Z[,2] == 0),]
  
  fSample <- rbind(tSample, cSample)
  
  # Calculate residuals ei (ei = Yi - conditional mean of Yi)
  
  ei <- rep(0, times = n)
  
  for(i in 1:length(fSample[,1])) {
    if(fSample[i,2] == 1) {
      ei[i] <- 0
    } else {
      ei[i] <- fSample[i,3]
    }
  }
  
  Z <- cbind(Z,ei)
  
  return(Z)
}
