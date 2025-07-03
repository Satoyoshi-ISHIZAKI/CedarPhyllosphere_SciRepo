library(phyloseq)
library(ggplot2)
library(dplyr)
library(tidyr)

plot_top_taxa <- function(physeq,
                          n_taxa = 10,
                          taxonomic_level = "Genus",
                          transform = "relative",
                          sort_by = "mean",
                          custom_colors = NULL,
                          sample_order = NULL,
                          show_unknown = TRUE) {
  
  # Input validation
  if (!taxonomic_level %in% rank_names(physeq)) {
    stop("Specified taxonomic level not found in the phyloseq object")
  }
  
  # check if taxa are rows
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_table(physeq) <- phyloseq::otu_table(t(otu_table(physeq)), taxa_are_rows = T) 
  }
  
  # Transform counts to relative abundance if specified
  if (transform == "relative") {
    physeq_transformed <- transform_sample_counts(physeq, function(x) x / sum(x) * 100)
  } else {
    physeq_transformed <- physeq
  }
  
  # Extract abundance matrix and taxonomy
  otu_table <- as.data.frame(otu_table(physeq_transformed))
  tax_table <- as.data.frame(tax_table(physeq_transformed))
  
  # Get sample metadata
  sample_data <- data.frame(sample_data(physeq_transformed))
  
  # Aggregate at specified taxonomic level
  tax_table$Taxa <- tax_table[[taxonomic_level]]
  tax_table$Taxa[is.na(tax_table$Taxa)] <- "Unknown"
  
  # If show_unknown is FALSE, treat "Unknown" as "Other"
  if (!show_unknown) {
    tax_table$Taxa[tax_table$Taxa == "Unknown"] <- "Other"
  }
  
  # Calculate abundance per taxa
  taxa_abundances <- data.frame(
    Taxa = tax_table$Taxa,
    Abundance = rowSums(otu_table)
  )
  
  # Group and summarize by taxa, excluding "Other" from ranking
  taxa_summary <- taxa_abundances %>%
    dplyr::filter(Taxa != "Other") %>%  # Exclude any existing "Other" entries
    group_by(Taxa) %>%
    summarise(
      total = sum(Abundance),
      mean = mean(Abundance)
    ) %>%
    arrange(desc(if(sort_by == "mean") mean else total))
  
  # Select top N taxa (excluding "Unknown" if show_unknown is FALSE)
  if (!show_unknown) {
    taxa_summary <- taxa_summary %>% dplyr::filter(Taxa != "Unknown")
  }
  top_taxa <- head(taxa_summary$Taxa, n_taxa)
  
  # Prepare plotting data with the corrected approach
  OTU_taxa <- tax_table %>% 
    dplyr::select(Taxa) %>%
    mutate(OTU = row.names(tax_table))
  
  plot_data <- otu_table %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    mutate(Sample = rownames(.)) %>%
    gather(key = "OTU", value = "Abundance", -Sample) %>%
    left_join(
      OTU_taxa,
      by = "OTU"
    ) %>%
    mutate(
      Taxa = ifelse(Taxa %in% top_taxa, Taxa, "Other")
    ) %>%
    group_by(Sample, Taxa) %>%
    summarise(Abundance = sum(Abundance), .groups = "drop")
  
  # Add sample metadata
  plot_data <- plot_data %>%
    left_join(
      sample_data %>% 
        mutate(Sample = rownames(sample_data)),
      by = "Sample"
    )
  
  # Order samples if specified
  if (!is.null(sample_order)) {
    plot_data$Sample <- factor(plot_data$Sample, levels = sample_order)
  }
  
  # Create color palette if not provided
  if (is.null(custom_colors)) {
    n_colors <- length(top_taxa)  # Number of colors needed for top taxa
    taxa_colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
    # Create named vector matching colors to taxa
    names(taxa_colors) <- top_taxa
    # Add "Other" with grey color
    custom_colors <- c(taxa_colors, "Other" = "#808080")
  }
  
  # Ensure "Other" appears at the top of the stack
  plot_data$Taxa <- factor(plot_data$Taxa, 
                           levels = c("Other", setdiff(unique(plot_data$Taxa), "Other")))
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Sample, y = Abundance, fill = Taxa)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = custom_colors) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    labs(
      x = "Sample",
      y = ifelse(transform == "relative", "Relative Abundance (%)", "Abundance"),
      fill = taxonomic_level
    )
  
  return(list(plot = p, data = plot_data, colors = custom_colors))
}
