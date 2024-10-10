# add NAs to tax table
# take taxonomy data (in which taxonomy list is contained in "taxonomy$data$Taxon")
#returns taxonomy list in which "NA"s are inserted to blanks

# define add_NA
# takes vector data and add "NA"s to the data
# until the length of the vector reaches the maximum number of phylogenetic levels in taxonomy list
add_NA <- function(vec, levelmax) {
  levels_lacked <- levelmax - length(vec)  #the number of blanks in given taxonomy vector
  vec <- c(vec, rep(NA, levels_lacked)) #insert "NA" in the blanks
  return(vec)
}

insert_NA <- function(taxonomy) {
  tax_list <- strsplit(as.character(taxonomy$data$Taxon), ";")
  level_max <- max(sapply(tax_list, length)) # maximum number of phylogenetic levels
  # insert "NA" in blanks in the given taxonomy list
  tax_list_inserted <- lapply(tax_list, add_NA, level_max)
  
  return(tax_list_inserted)
}