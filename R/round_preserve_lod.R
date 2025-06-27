# Custom function to round numeric values while preserving "<LOD"
round_preserve_lod <- function(x, digits = 3) {
  #Check if the value is NA
  if (is.na(x)) {
    return(x)
    } else {
    # Check if the value is "<LOD"
    if (x == "<LOD") {
      return(x)
      } else {
        # Convert to numeric and round
        return(as.character(round(as.numeric(x), digits)))
        }
    }
}