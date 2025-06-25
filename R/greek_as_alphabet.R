#function to convert "a", "b" ... to "alpha", "beta", ...
greek_as_alphabet <- function(names) {
  #output vector
  out <- names
  
  #replace with Greek letters
  out <- gsub("a-", "alpha-", out, fixed = T)
  out <- gsub("b-", "beta-", out, fixed = T)
  out <- gsub("g-", "gamma-", out, fixed = T)
  out <- gsub("d-", "delta-", out, fixed = T)
  out <- gsub("l-", "lambda-", out, fixed = T)
  out <- gsub("o-", "olto-", out, fixed = T)
  out <- gsub("m-", "meta-", out, fixed = T)
  out <- gsub("p-", "para-", out, fixed = T)
  
  return(out)
}