wildBootATT <- function(Z, radem) {
  
  # split
  
  treatedArm <- treatedSplit(Z)
  controlArm <- controlSplit(Z)
  
  tHat <- mean(Z[,6], na.rm=TRUE)
  
  resids <- Z[,1] - tHat - Z[,7]
  
  perturb <- radem * resids
  
  # Make NA perturbs 0, because those are control units
  
  perturb[is.na(perturb)] <- 0
  
  # Perturb outcomes
  
  bootY <- Z[,7] + tHat + perturb
  
  X <- treatedArm[,2]
  W <- treatedArm[,3]
  Y <- bootY[!is.na(bootY)]
  ei <- treatedArm[,4]
  k <- treatedArm[,5]
  
  # Get new Unit TETs 
  
  # yHats are unchanged, because matches are unchanged
  treatedyHats <- treatedArm[,7]
  
  unitTETs <- Y - treatedyHats
  
  bootedTArm <- cbind(Y, X, W, ei, k, unitTETs, treatedyHats)
  
  bootZ <- rbind(bootedTArm, controlArm)
  
  return(bootZ)
  
}
  
  
