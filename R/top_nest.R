#select top n abundant taxa and merge other taxa by higher taxonomic identities
#slightly modified the function "nested_top_taxa" in the package fantaxtic
library(phyloseq)
library(fantaxtic)

top_nest <- function(ps_obj, 
                     tax_level, 
                     top_level, 
                     n, 
                     top_merged_label = "Other", 
                     nested_merged_label = "Other <tax>", 
                     by_proportion = T, 
                     ...) {
  
  #tax ranks character vector
  tax_ranks <- c(rank_names(ps_obj))
  
  # Check arguments
  if (tax_level == "ASV" & !"ASV" %in% tax_ranks){
    tax_ranks <- c(tax_ranks, "ASV")
  }
  if (is.null(top_level)){
    stop("Error: no top_level provided")
  } else if (!top_level %in% tax_ranks){
    msg <- sprintf("Error: top_level %s not in rank_names(ps_obj)", top_level)
    stop(msg)
  }
  if (is.null(tax_level)){
    stop("Error: no tax_level provided")
  } else if (!tax_level %in% tax_ranks){
    msg <- sprintf("Error: tax_level %s not in rank_names(ps_obj)", tax_level)
    stop(msg)
  }
  if(top_level == tax_level){
    msg <- sprintf("Error: top_level and tax_level cannot both be %s", top_level)
    stop(msg)
  }
  if(which(tax_ranks == top_level) > which(tax_ranks == tax_level)){
    msg <- sprintf("Error: top_level needs to be a higher taxonomic rank than tax_level.
                    Tax ranks in this object are %s", paste(tax_ranks, collapse = ", "))
    stop(msg)
  }
  if(!grepl("<tax>", nested_merged_label)){
    stop("Error: nested merged label must include <tax>")
  }
  
  # Make sure taxa are rows
  if (!phyloseq::taxa_are_rows(ps_obj)) {
    otu_table(ps_obj) <- phyloseq::otu_table(t(otu_table(ps_obj)), taxa_are_rows = T)
  }
  
  # Get the top taxa and its higher level taxonomies
  top_top <- top_taxa(ps_obj, tax_level = tax_level, n_taxa = n,
                      by_proportion = by_proportion, ...)$top_taxa
  ranks <- tax_ranks[1:which(tax_ranks == top_level)]
  top_top_taxa <- apply(top_top[,ranks,drop=FALSE], 1, paste, collapse = '_')
  
  # Extract the taxonomy of the top_tax_level and collapse all others
  taxa <- tax_table(ps_obj) %>%
    data.frame() %>%
    select(ranks) %>%
    unite(col = 'full_taxonomy') %>%
    filter(full_taxonomy %in% top_top_taxa)
  taxids <- row.names(taxa)
  ps_obj_nest <- collapse_taxa(ps_obj, taxids, merged_label = top_merged_label)
  
  # Add an ASV column if working at ASV level
  if (tax_level == "ASV"){
    ranks <- rank_names(ps_obj_nest)
    if(!"ASV" %in% ranks){
      tax_table(ps_obj_nest) <- cbind(tax_table(ps_obj_nest),
                                      ASV = row.names(tax_table(ps_obj_nest)))
    }
  } else {
    
    # If not ASVs, glom at the nested level.
    ps_obj_nest <- tax_glom(ps_obj_nest, taxrank = tax_level, NArm = F)
  }
  
  # Loop through each top_tax_level and merge not top taxa
  lvls <- unique(taxa$full_taxonomy)
  top_nest <- list()
  merged_ids <- c()
  # top_top with full_taxonomy column
  top_top_fulltax <- top_top %>%
    data.frame() %>%
    select(ranks) %>%
    unite(col = 'full_taxonomy') %>%
    cbind(top_top)
  
  for (lvl in lvls){
    
    # Get the taxids of all nested_taxa within the current top_level taxon.
    all_taxids <- taxa %>%
      filter(full_taxonomy == lvl) %>%
      row.names(.)
    all_taxids <- all_taxids[all_taxids %in% taxa_names(ps_obj_nest)]
    
    # Select named taxa only
    named_taxids <- tax_table(ps_obj_nest) %>%
      data.frame(taxid = row.names(.)) %>%
      filter(taxid %in% all_taxids,
             !is.na(!!as.symbol(tax_level))
      ) %>%
      pull(taxid)
    
    # Get the top taxa
    if (length(named_taxids) != 0){
      
      # Remove all other taxa
      ps_obj_tmp <- collapse_taxa(ps_obj_nest, named_taxids, discard_other = T) %>%
        suppressWarnings()
      
      # Get the top taxa in the lvl
      top_nest[[lvl]] <- top_top_fulltax %>% filter(full_taxonomy == lvl) %>%
        select(-full_taxonomy) %>%
        suppressWarnings()
      
      # Find all taxa to merge
      to_merge <- all_taxids[!all_taxids %in% top_nest[[lvl]]$taxid]
    } else {
      to_merge <- all_taxids
    }
    
    
    # Merge the non-top taxa within this top-level taxon
    ps_obj_nest <- merge_taxa(ps_obj_nest, to_merge, 1) %>%
      suppressWarnings()
    
    # Store the merged ids
    if(length(to_merge) > 0 ){
      merged_ids <- c(merged_ids, to_merge[1])
    }
  }
  
  # Combine the results
  top_nest <- do.call("rbind", top_nest)
  
  # Add top abundances
  top <- top_top %>%
    select(where(~!all(is.na(.x)))) %>%
    select(!c(taxid)) %>%
    rename(top_abundance = abundance,
           top_tax_rank = tax_rank) %>%
    left_join(top_nest, .) %>%
    rename(nested_abundance = abundance,
           nested_tax_rank = tax_rank) %>%
    relocate(taxid, top_abundance, nested_abundance, top_tax_rank, nested_tax_rank) %>%
    suppressMessages()
  
  # Update the taxon name to nested_merged_label
  tax_tbl <- phyloseq::tax_table(ps_obj_nest) %>%
    as.data.frame() %>%
    as.matrix()
  for (i in merged_ids){
    lab <- gsub("<tax>", tax_tbl[i,top_level], nested_merged_label)
    tax_tbl[i, is.na(tax_tbl[i,])] <- lab
  }
  phyloseq::tax_table(ps_obj_nest) <- tax_table(tax_tbl)
  
  # Return a list of top and merged values
  return(list(ps_obj = ps_obj_nest,
              top_taxa = top))
}

