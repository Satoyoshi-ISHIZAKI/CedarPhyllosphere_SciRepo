#This is the source code of "CLOUD" analysis, developed by E. Montassier et al., 2018
#CLOUD is a nonparametric multivariate outlier analysis to detect microbiome outliers.
#Montassier, E., Al-Ghalith, G.A., Hillmann, B. et al. CLOUD: a non-parametric detection test for microbiome outliers. Microbiome 6, 137 (2018). 
#https://doi.org/10.1186/s40168-018-0514-4

# inputs a distance matrix
# returns piecewise distances of samples and their outlier percentile
# and a matrix of the repeated measures of distances  
# k is number of neighbors chosen
"piecewise_kn_V1" <- function(d, test.ix, k=X, ndim=-1){ 
  if(class(d) != 'matrix') d <- as.matrix(d) 
  stats <- numeric(length(test.ix))
  pvals <- numeric(length(test.ix))  
  for(i in 1:length(test.ix)){
    ref.ix <- test.ix[-i]
    keep.ix <- c(test.ix[i], ref.ix)
    if(ndim > -1){
      pc <- cmdscale(d[keep.ix,keep.ix,drop=F],k=ndim)
      d.i <- as.matrix(dist(pc))
    } else {
      d.i <- d[keep.ix,keep.ix,drop=F]
    }
    test.dist <- mean(sort(d.i[1,-1])[1:k])
    ref.dists <- numeric(length(ref.ix))
    for(j in 1:length(ref.ix)){
      ref.dists[j] <- mean(sort(d.i[-1,-1][j,-j]))
    }
    
    stats[i] <- test.dist / mean(ref.dists)
    pvals[i] <- mean(test.dist < ref.dists)
  }
  result <- list()
  result$stats <- stats
  result$pvals <- pvals
  outcome <- pvals <= 0.05
  result$length <- length(outcome[outcome==TRUE]) 
  return(result) 
}