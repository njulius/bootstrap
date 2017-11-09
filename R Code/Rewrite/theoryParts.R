theoryParts <- function(Z, bootZ, matches, radem) {
  
  # Split sample
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  # Get N1 and N0
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  tau <- 5 # HARDCODED VALUE OF TAU
  
  # Get ATET
  
  tHat <- getATET(Z, matches)
  bHat <- getATET(bootZ, matches)
 
  # Tn is zero by definition with this DGP.
  
  Tn <- 0
  
  # Qn is the sum of differences between the estimated unit TETs and the true tau
  
  bigZ <- makeBigZ(Z, matches)
  
  inter <- bigZ[,6] - tau # HARDCODED VALUE OF TAU
  
  Qn <- inter * radem
  
  Qn <- Qn[!is.na(Qn)]
  
  Qn <- mean(Qn)
  
  # Rn is the sum of differences between tHat and tau, perturbed by radem
  
  Rn <- rep(tau - tHat, times = n1)
  
  Rn <- Rn * radem[1:n1]
  
  Rn <- mean(Rn)
  
  # The correct Qn would be caulcated as in the paper
  
    # Create a column containing Wi when Wi is 1, and Ki when Wi is 0
  
  bigZ <- makeBigZ(Z, matches)
  
  bigBootZ <- makeBigZ(bootZ, matches)
  
  thing <- bigZ[,5] + bigZ[,2]
  
    # Multiply it by e_i
  
  thing <- thing * bigZ[,4]
   
    # Perturb it
  
  thing <- thing * radem
  
  correctQn <- sum(thing) / n1
  
  # So the synthetically corrected bootstrap estimator would be tHat + Rn + correctQn
  
  return(Qn, Rn, tHat, bHat, correctQn)
}
