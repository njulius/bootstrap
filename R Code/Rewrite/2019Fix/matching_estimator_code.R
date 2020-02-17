# Z[,3] is the treatment column

# Data Splitters

treatedSplit <- function(Z) {
  
  treatedArm <- Z[which(Z[,3] == 1),]
  
  return(treatedArm)
  
}

controlSplit <- function(Z) { 
  
  controlArm <- Z[which(Z[,3] == 0),]
  
  return(controlArm)
  
}

# Nearest Neighbor Matching Estimators, for ATT and ATC

# TC finds matches *from* the control units *for* the treated units.

getMatchesTC <- function(treatedArm, controlArm, nMatches) {
  
  require(Matrix)
  
  TCdistMatrix <- apply(treatedArm, 1, function(x) abs(x[2] - controlArm[,2]))
  
  TCmatches <- Matrix(apply(TCdistMatrix, 2, function(x) x <= sort(x)[nMatches]), sparse = TRUE)
  
  return(TCmatches)
  
}

# CT finds matches *from* the treated units *for* the control units.

getMatchesCT <- function(treatedArm, controlArm, nMatches) {
  
  require(Matrix)
  
  CTdistMatrix <- apply(controlArm, 1, function(x) abs(x[2] - treatedArm[,2]))
  
  CTmatches <- Matrix(apply(CTdistMatrix, 2, function(x) x <= sort(x)[nMatches]), sparse = TRUE)
  
  return(CTmatches)
  
}

getEstimates <- function(augTreatedArm, augControlArm) {
  
  n1 <- length(augTreatedArm[,1])
  n0 <- length(augControlArm[,1])
  n <- n1 + n0
  
  # Generate ATET
  
  atet <- (sum(augTreatedArm[,1]) - sum(augControlArm[,1]*augControlArm[,5]))*(1/n1)
  
  # Generate ATEU
  
  ateu <- (sum(augTreatedArm[,1]*augTreatedArm[,5]) - sum(augControlArm[,1]))*(1/n0)
  
  # Generate ATE
  
  treatedIntermediate <- sum(augTreatedArm[,1]) + sum(augTreatedArm[,1]*augTreatedArm[,5])
  controlIntermediate <- sum(augControlArm[,1]) + sum(augControlArm[,1]*augControlArm[,5])
  
  ate <- (1/n)*(treatedIntermediate - controlIntermediate)
  
  # Generate Sanity
  
  sanity <- (n1/n)*atet + (n0/n)*ateu
  
  results <- cbind(atet, ateu, ate, sanity)
  
  return(results)
  
}

appendWeightsTC <- function(treatedArm, controlArm, numMatches) {
  
  # Do the matching
  
  matchesTC <- getMatchesTC(treatedArm, controlArm, numMatches)
  
  # Get the weights for each control unit
  
  k <- rowSums(matchesTC)
  
  # Append k to correct arm and return it
  
  augControlArm <- cbind(controlArm, k)
  
  return(augControlArm)
  
}

appendWeightsCT <- function(treatedArm, controlArm, numMatches) {
  
  matchesCT <- getMatchesCT(treatedArm, controlArm, numMatches)
  
  k <- rowSums(matchesCT)
  
  augTreatedArm <- cbind(treatedArm, k)
  
  return(augTreatedArm)
  
}

# Wrapper to do everything from original data

estimators <- function(Z) {
  
  # Split data
  
  treatedArm <- treatedSplit(Z)
  controlArm <- controlSplit(Z)
  
  augControlArm <- appendWeightsTC(treatedArm, controlArm, 1)
  augTreatedArm <- appendWeightsCT(treatedArm, controlArm, 1)
  
  estimates <- getEstimates(augTreatedArm, augControlArm)
  
  return(estimates)
}
