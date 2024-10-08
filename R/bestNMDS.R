#function to perform NMDS with the specific seed value with which the stress value of NMDS is lowest.
library(vegan)

#perform NMDS 100 times starting with 100 different seed values
bestNMDS <- function(dist) {
  stress <- numeric(100)
  for (i in 1:100) {
    set.seed(i)
    nmds <- metaMDS(dist, trymax = 100)
    stress[i] <- nmds$stress
  }
  
  #perform NMDS starting with the seed with the lowest stress
  bestseed <- grep(min(stress), stress)
  set.seed(bestseed)
  nmds <- metaMDS(dist, trymax = 100)
  result <- list(nmds = nmds, seed = bestseed)
  
  return(result)
}