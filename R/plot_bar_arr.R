#plot community composition barplot at the specified taxon level
#slightly modified the function "plot_nested_bar" in the package fantaxtic

require(fantaxtic)
require(viridis)

plot_bar_arr <- function(ps_obj,
                         top_level,
                         top_merged_label = "Other",
                         palette = NULL,
                         base_clr = "#008CF0",
                         merged_clr = "grey90",
                         include_rank = T,
                         na_taxon_label = "<tax> (<rank>)",
                         asv_as_id = F,
                         duplicate_taxon_label = "<tax> <id>",
                         relative_abundances = T,
                         sample_order = NULL,
                         ...){
  
  #default color palette (viridis)
  if(is.null(palette)){
    palette <- viridis::viridis(ntaxa(ps_obj))
  }
  
  # Create labels
  ps_tmp <- ps_obj %>%
    name_na_taxa(include_rank = include_rank,
                 na_label = na_taxon_label)
  
  # Generate a palette
  pal <- taxon_colours(ps_tmp,
                       tax_level = top_level,
                       merged_label = top_merged_label,
                       merged_clr = merged_clr,
                       palette = palette,
                       base_clr = base_clr)
  
  # Convert physeq to df
  psdf <- psmelt(ps_tmp)
  
  # Move the merged labels to the appropriate positions
  psdf <- move_label(psdf = psdf,
                     col_name = top_level,
                     label = top_merged_label,
                     pos = 0)
  
  # Reorder samples
  if(!is.null(sample_order)){
    if(all(sample_order %in% unique(psdf$Sample))){
      psdf <- psdf %>%
        mutate(Sample = factor(Sample, levels = sample_order))
    } else {
      stop("Error: not all(sample_order %in% sample_names(ps_obj)).")
    }
    
  }
  
  # Generate a bar plot
  p <- ggplot(psdf,
              mapping = aes(x = Sample,
                            y = Abundance, 
                            fill = eval(parse(text = top_level)))) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = pal) +
    labs(fill = top_level) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90))
  if (relative_abundances){
    p <- p + geom_col(position = position_fill())
  } else {
    p <- p + geom_col()
  }
  return(p)
  
}