#function to convert isomer symbols italic
html_italic <- function(names, sep) {
  #take a string vector
  #Greek letters and alphabets are pasted by "-"
  split <- strsplit(names, sep)
  split <- lapply(split, 
                  function(x){c(rep(NA, max(sapply(split,length)) - length(x)), x)})
  #elements should be the same length
  
  #the positions at which Greek letters are used
  gr <- grep("[Α-ω]", sapply(split, `[`, 1))
  #alphabets
  latin <- grep("[A-Za-z]", sapply(split, `[`, 1))
  
  #output vector
  out <- names
  
  #other alphabets
  out[gr] <- paste0(sprintf("<i>%s</i>", sapply(split, `[`, 1)[gr]), sep, sapply(split, `[`, 2)[gr])
  out[latin] <- paste0(sprintf("<i>%s</i>", sapply(split, `[`, 1)[latin]), sep, sapply(split, `[`, 2)[latin])
  
  
  return(out)
}