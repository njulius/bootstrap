# Abadie & Imbens (2008) Simulation
# Author: Nik Julius
# Last Modified: Oct 11, 2016

# A function that takes in a dataset containing controls and treated units, and finds
# the closest match(es) for each treated unit. 

findMatches <- function(Z) {
  require(Matrix)
  
  # Variables 
  #
  # Z should be the output of the function genData(), or in general a matrix with 3
  # columns corresponding to X, W, and Y
  
  # First, we need to separate Z into the two subsamples consisting of treated units
  # (W[i] = 1) and control units (W[i] = 0)
  
  # First, we need to separate Z into the two subsamples consisting of treated units
  # (W[i] = 1) and control units (W[i] = 0)
  
  treatedSample <- Z[which(Z[,2] == 1),]
  controlSample <- Z[which(Z[,2] == 0),]
  
  n1 <- length(treatedSample[,1])
  n0 <- length(controlSample[,1])
  
  # Construct spare logical matrix to contain logical matches. For illustration, consider
  # 3 treated units and 3 control units:
  # ############## This matrix represents the case where C1 and C2 are both the closest match
  # #   T1 T2 T3 # for T1, C2 and C3 are the closest match for T2, and C3 is the closest match
  # # C1 1  0  0 # for T3. The sparse argument makes the matrix only store the true values.
  # # C2 1  1  0 #
  # # C3 0  1  1 #
  # ##############
  
  matches <- Matrix(nrow = n0, ncol = n1, sparse = TRUE)
  numMatches <- rep(1, times=n1)
  
  for(i in 1:n1) {
    # Get distance between covariate for treated unit i and all of the control units:
    dists <- abs(controlSample[,1] - treatedSample[i,1])
    
    # Add a column containing the index for each control unit, to produce a 2 colum matrix:
    indexedDists <- cbind(seq(from=1, to=n0, by=1), dists)
    
    # Get a list of indexes ordered by distance, smallest to largest:
    distList <- order(indexedDists[,2])
    orderedDists <- cbind(distList, dists[distList])
    
    # Find number of matches for each treated unit:
    for(j in 1:n0) {
      if(isTRUE(all.equal(orderedDists[1,2], orderedDists[j,2]))) {
        numMatches[i] <- j
      } else {
        break
      }
    }
    
    # Populate logical matrix:
    for(j in 1:numMatches[i]) {
      matches[orderedDists[j,1],i] <- TRUE
    }
    
    # Check that each column of the matrix has at least one match (that is, check that this worked)
    
    for(q in 1:n1) {
      if(any(matches[,q] == TRUE)) {
        # do nothing
      } else {
        cat("a disaster has occurred \n")
      }
    }
    
    return(matches)
  }
}
