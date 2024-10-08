#function to calculate occurrence
occurrence <- function(x) {
  occur <- sum(x > 0)
  return(occur)
}