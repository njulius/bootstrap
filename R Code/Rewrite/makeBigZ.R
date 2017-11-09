makeBigZ <- function(Z, matches) {
  
  Z <- appendMatches(Z, matches)
  Z <- appendUnitTET(Z, matches)
  
  return(Z)
}
