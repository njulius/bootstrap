wildBoot <- function(Z, matches) {
  
  bigZ <- makeBigZ(Z, matches)
  
  tHat <- getATET(Z, matches)
  
  # Split sample
  
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  resids <- bigZ[,3] - tHat - bigZ[,7]
  
  perturb <- bigZ[,8] * resids
  
  # Make NA perturbs 0, because we aren't perturbing those
  
  perturb[is.na(perturb)] <- 0
  
  # Perturb outcomes
  
  bootY <- bigZ[,7] + tHat + perturb
  
  X <- treatedSample[,1]
  W <- treatedSample[,2]
  Y <- bootY[!is.na(bootY)]
  ei <- treatedSample[,4]
  
  bootedTSample <- cbind(X, W, Y, ei)
  
  bootZ <- rbind(bootedTSample, controlSample)
  
  return(bootZ)
  
}
