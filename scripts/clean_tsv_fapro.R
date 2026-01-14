# Description: This script cleans FAPROTAX output files by removing comment lines

# path to the FAPROTAX output files
source("scripts/path.R")

# Function to clean FAPROTAX output files
clean_faprotax_file <- function(input_file, output_file) {
  # Read all lines
  lines <- readLines(input_file)
  
  # Find where data starts (after comment lines)
  data_start <- which(!grepl("^#", lines))[1]
  
  # Read the actual data
  data <- read.table(input_file,
                     sep = "\t",
                     header = TRUE,
                     row.names = 1,
                     comment.char = "#",
                     check.names = FALSE)
  
  # Write clean TSV
  write.table(data,
              file = output_file,
              sep = "\t",
              quote = FALSE,
              col.names = NA,
              row.names = TRUE)
  
  cat("Clean file saved to:", output_file, "\n")
}

# Use the function
clean_faprotax_file(paste0(path_out, "/231202_Sugi_phyllosphere_June_16S/processed_data/mod/group_overlaps_mod.tsv"),
                    paste0(path_out, "/231202_Sugi_phyllosphere_June_16S/processed_data/mod/group_overlaps_modc.tsv"))

clean_faprotax_file(paste0(path_out, "/231202_Sugi_phyllosphere_June_16S/processed_data/mod/groups2otus_mod.tsv"),
                    paste0(path_out, "/231202_Sugi_phyllosphere_June_16S/processed_data/mod/groups2otus_modc.tsv"))
