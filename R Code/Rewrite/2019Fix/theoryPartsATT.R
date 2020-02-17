theoryPartsATT <- function(Z, bootZ, radem, trueTau) {
  
  # Split sample
  treatedArm <- treatedSplit(Z)
  controlArm <- controlSplit(Z)
  
  n1 <- length(treatedArm[,1])
  n0 <- length(controlArm[,1])
  
  # Get ATET
  
  tHat <- mean(Z[,6], na.rm=TRUE)
  bHat <- mean(bootZ[,6], na.rm=TRUE)
  
  # Tn is zero by definition in this DGP
  
  Tn <- 0
  
  # Qn is the mean of differences between the unit TETs and 
  # the true tau, perturbed
  
  diffs <- Z[,6] - trueTau
  
  Qn <- diffs * radem
  
  Qn <- Qn[!is.na(Qn)]
  
  Qn <- mean(Qn)
  
  # Rn is the mean of differences between tauHat and the true Tau
  # perturbed
  
  Rn <- rep(trueTau - tHat, times = n1)
  
  Rn <- Rn * radem[1:n1]
  
  Rn <- mean(Rn)
  
  # The synthetically corrected Qn is calculated as in the paper
  
  thing <- Z[,5] + Z[,3]
 
  # Multipliy it by ei
  
  thing <- thing * Z[,4]
  
  # Perturb it
  
  thing <- thing * radem
  
  correctQn <- sum(thing) / n1
  
  return(cbind(Qn, Rn, tHat, bHat, correctQn))
  
}
