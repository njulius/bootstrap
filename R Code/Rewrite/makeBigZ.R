makeBigZ <- function(Z, matches) {
  
  Z <- appendMatches(Z, matches)
  Z <- appendUnitTET(Z, matches)
  Z <- appendyHat(Z, matches)
  
  return(Z)
}
