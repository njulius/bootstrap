# Thing to generate all the columns needed at once for the simulation

initiatorATT <- function(nTreated, nControl, trueTau) {
  
  # Generate Y,X,W
  
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
  
  # Generate weight column for ATT matching
  
  treatedArm <- treatedSplit(Z)
  controlArm <- controlSplit(Z)
  
  matchesTC <- getMatchesTC(treatedArm, controlArm, 1)
  
  controlK <- rowSums(matchesTC)
  
  treatedArm <- cbind(treatedArm, rep(0, times = nTreated))
  controlArm <- cbind(controlArm, controlK)
  
  # Generate Unit-specific TET estimates
  
  inter <- matrix(controlArm[,1], nrow = length(controlArm[,1]), ncol = nTreated)
  
  nMatches <- colSums(matchesTC)
  
  summedCF <- colSums(inter * matchesTC)
  
  CFs <- summedCF / nMatches
  
  unitTETs <- treatedArm[,1] - CFs
  
  treatedArm <- cbind(treatedArm, unitTETs)
  controlArm <- cbind(controlArm, rep(NA, times = nControl))
  
  # Generate yHats
  
  yHats <- CFs
  
  treatedArm <- cbind(treatedArm, yHats)
  controlArm <- cbind(controlArm, rep(NA, times = nControl))
  
  # Generate theory parts
  
  # tauHat <- mean(unitTETs) # This is correct and checked
  
  outZ <- rbind(treatedArm, controlArm)
  colnames(outZ) <- c("Y", "X", "W", "ei", "k", "unitTETs", "yHats")
  
  return(outZ)
}
