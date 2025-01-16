#function to convert "alpha", "beta" ... to Greek letters
#function to convert isomer symbols italic
greek <- function(names) {
  #output vector
  out <- names
  
  #replace with Greek letters
  out <- gsub("alpha", sprintf("%s", "\u03B1"), out)
  out <- gsub("beta", sprintf("%s", "\u03B2"), out)
  out <- gsub("gamma", sprintf("%s", "\u03B3"), out)
  out <- gsub("delta", sprintf("%s", "\u03B4"), out)
    
  return(out)
}