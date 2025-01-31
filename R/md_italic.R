#convert isomer labels to italic
#show texts in italic in geom_text, geom_text_repel, geom_label_repel, etc.
#convert space to "~"

md_italic <- function (names, sep) {
  #take a string vector
  #Greek letters and alphabets are pasted by seperators
  split <- strsplit(names, sep)
  split <- lapply(split, 
                  function(x){c(rep(NA, max(sapply(split,length)) - length(x)), x)})
  #elements should be the same length
  
  #the positions at which Greek letters are used
  gr <- grep("[Α-ω]", sapply(split, `[`, 1))
  #other alphabets
  latin <- grep("[A-Za-z]", sapply(split, `[`, 1))
  #space
  space <- grep(" ", names)
  
  #output vector
  out <- names
  
  #replace with Greek letters
  out[gr] <- paste0("*", sapply(split, `[`, 1)[gr], "*", sep, sapply(split, `[`, 2)[gr])
  #other alphabets
  out[latin] <- paste0("*", sapply(split, `[`, 1)[latin], "*", sep, sapply(split, `[`, 2)[latin])
  
  return(out)
}