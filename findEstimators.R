# Abadie & Imbens (2008) Simulation
# Author: Nik Julius
# Last Modified: Oct 13, 2016

# Function that takes a dataset and returns estimators of the ATE and ATET.

getATE <- function(Z) {
  # Variables 
  #
  # Z should be the output of the function genData(), or in general a matrix with 3
  # columns corresponding to X, W, and Y
  
  # First, we need to get the match assignments:
  
  matches <- findMatches(Z)
  
  # Now, split sample into treatment and control groups.
  
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  # matches is best understood as having each treated observation corresponding to a column,
  # and each control observation to a row. A true value anywhere in the matrix indicates that
  # the treated observation represented by that column is most closely matched by the control
  # observation represented by that row.
  
  summedATE <- 0
  
  # Now, iterate through the columns of matches, and sum up the treatment effect estimates from
  # each match. Then, obviously, average them.
  
  for(i in 1:n1) {
    # Get the vector (which may contain only one element) of closest matches for treated 
    # oservation i
    
    mates <- controlSample[which(matches[,i] == TRUE),]
    
    # Get vector of treatment effects for each match
    
    treatmentEffects <- treatedSample[i,3] - mean(mates[,3])
    
    # Get mean effect and add to estimatedATE
    
    summedATE <- summedATE + mean(treatmentEffects)
  }
  
  # Divide to get average
  
  estimatedATE <- summedATE / n1
  
  return(estimatedATE)
}

getATET <- function(Z) {
  # Actually fairly sure that's what I do in getATE(). This is left alone until I figure it out  
}

