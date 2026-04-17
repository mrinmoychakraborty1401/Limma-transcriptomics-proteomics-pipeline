# ==================================
# Analysis Helper Functions
# ==================================
# PROTOCOL: STRICT PRESERVATION MODE
# 1. DO NOT REMOVE EXISTING FEATURES for "cleanliness" or "minimalism".
# 2. IF EDITING: Only modify the specific logic requested.
# ==================================
# This script contains the core, reusable functions for the analysis pipeline.
# UPDATED: 
# 1. Fixed PPI Color Palette Logic (Rank-based naming).
# 2. Renumbered clusters 1..N based on size.
# 3. Hidden "Other" from legends.
# 4. Added Compare Cluster Analysis for ALL DBs (GO, KEGG, Reactome, DisGeNET, TFs).
# 5. Advanced Integrated Layout: LogFC nodes, Horizontal grid, Individual Saves with Tags.
# 6. Unified Plot Styling: Magma palette, Adj.P color (non-log), consistent legends (P on top).
# 7. Clean Titles (No underscores).
# 8. Flexible 'project_name' in QC plots.
# 9. FIXED: PCA Ellipse logic to prevent crashes with < 4 samples per group.
# 10. FIXED: Added automatic directory creation in .create_qc_plots to prevent ggsave "Cannot find directory" errors.
# 11. ADJUSTED: Switched PPI layout to 'fr', removed edge weights, increased iterations and spacing for max separation.
# 12. ADJUSTED: Label logic - Red (>2), Blue (<-2), Green (Top 10% Hubs).
# 13. ADJUSTED: Loop ensures Enrichment folders are created for ALL clusters > 3 nodes.
# 14. FIXED: Moved Enrichment Loop BEFORE Plotting Loop to ensure folders are created even if plotting fails.
# 15. FIXED: Corrected label variable in mini-panels to prevent 'Problem while computing aesthetics' crash.
# 16. ADDED: Strict Input Filtering (|logFC| >= 2, P <= 0.05) to reduce network clutter.
# 17. ADDED: Automatic Top 3 Hubs Extraction to CSV.
# 18. ADDED: Split CompareCluster Analysis (Top 5 vs All) into separate folders.
# 19. CHECKPOINT UPDATE: Granular skipping. Deleting PNG triggers ONLY plotting. Enrichment skips if folders exist.
# 20. ADJUSTED: Compare Cluster plots now have DYNAMIC HEIGHT to prevent overlap.
# 21. FIXED: TF Compare Cluster X-axis is now numerically sorted (1, 2, 3...) instead of alphabetical (1, 10, 2).
# 22. REFINED LABELING: Adaptive thresholds (Top 5% LogFC + Degree check) to reduce clutter.
# 23. ADDED: Priority Gene Rescue mechanism (for Survival genes).
# 24. RE-ARCHITECTED: Cluster Enrichment now runs BEFORE Network Plotting to allow TF-based node labeling.
# 25. FIXED: TF Labeling Logic. Now only labels actual DNA-binding TFs (GO:0003700) that match enriched terms.
# 26. REFINED: Labeling logic restricted. Yellow labels now EXCLUSIVELY for Top 3 Hubs per cluster. TF labeling disabled to reduce clutter.
# 27. UPDATED: Hubs reverted to Green. Added High Risk Survival logic (Black Label + Rhombus Shape for HR > 1.8).
# 28. REFINED: TF Labeling Re-enabled (Yellow). Now selects EXACTLY ONE Top TF per cluster based on enrichment files.
# 29. ROBUSTNESS: Enhanced TF name extraction to handle complex strings (e.g. "FOXM1 25889361...") and case-sensitivity.
# 30. VISUALS: Switched to 'graphopt' layout with charge repulsion to ELIMINATE node overlap.
# 31. STABILITY: Added set.seed(42) to ensure clusters do not flip identities or positions between runs.
# 32. REFINED LAYOUT: Increased 'charge' to 0.1 and 'niter' to 10000 to force stronger node separation in dense clusters.
# 33. BUG FIX: Defined 'node_spacing_dial' BEFORE layout calculation to prevent 'object not found' error.
# 34. BUG FIX: Re-added missing 'stats_df' calculation logic.
# 35. ADJUSTED: Increased PPI filter to |logFC| >= 2.0 to reduce congestion.
# 36. ADJUSTED: Added hardcoded path for Survival Data.
# 37. REVERTED: Labeling logic to Quantile-based (Top 5%) to reduce clutter, as requested.
# 38. FIXED: Relaxed component size filter (>1) and force-keep priority gene clusters to rescue ALYREF.
# 39. FIXED: Syntax errors in .create_qc_plots function (removed artifact characters).
# 40. ADDED: Boxplots and Density Plots reinstated in QC.
# 41. ADDED: Dynamic PPI Cluster and Hub algorithms.
# 42. UPDATED: 'run_ppi_analysis' now accepts 'ppi_score' argument.
# 43. FIXED: Removed hardcoded strict filter in PPI to respect user settings.

# === 🧬 Reusable Enhanced Enrichment Analysis Function ===
run_advanced_enrichment_core <- function(genes, gene_list_fc, title_prefix, base_enrich_dir) {
  dir.create(base_enrich_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Lower threshold for enrichment to ensure we get results for smaller sets
  local_min_size <- 2 
  
  # --- CHECKPOINT: SKIPPED TOO SMALL ---
  receipt_too_small <- file.path(base_enrich_dir, ".SKIPPED_TOO_SMALL")
  if (file.exists(receipt_too_small)) {
    send_progress(paste0("    SKIPPED: Enrichment for '", title_prefix, "' (previously determined too small)."))
    return()
  }
  
  if(length(genes) < local_min_size) {
    send_progress(paste0("    NOTE: Not enough genes (", length(genes), ") in '", title_prefix, "' for enrichment (need ", local_min_size, ")."))
    file.create(receipt_too_small) 
    return()
  } else {
    if(file.exists(receipt_too_small)) unlink(receipt_too_small)
  }
  
  send_progress(paste0("    Running enrichment for ", length(genes), " genes in '", title_prefix, "'..."))
  
  # Map Symbols to Entrez
  entrez_ids <- tryCatch({
    bitr(genes, fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db)$ENTREZID
  }, error = function(e) {
    send_progress(paste0("    WARNING: Could not map gene symbols to Entrez IDs. Skipping KEGG/Reactome."))
    return(NULL)
  })
  
  # --- Helper: Generic Run and Save ---
  run_and_save <- function(db_name, enrichment_func) {
    receipt_success <- file.path(base_enrich_dir, paste0(".SUCCESS_", db_name))
    receipt_no_results <- file.path(base_enrich_dir, paste0(".NO_RESULTS_", db_name))
    
    if (file.exists(receipt_success) || file.exists(receipt_no_results)) {
      send_progress(paste0("      SKIPPED: ", db_name, " (already analyzed)."))
      return(NULL)
    }
    
    send_progress(paste0("      Querying ", db_name, "..."))
    res <- tryCatch(enrichment_func(), error = function(e) {
      send_progress(paste0("      ", db_name, " query failed: ", e$message))
      return(NULL)
    })
    
    if(!is.null(res) && nrow(res) > 0) {
      res_readable <- if (is(res, "enrichResult")) setReadable(res, OrgDb = org.Hs.eg.db, keyType = "ENTREZID") else res
      
      enrichment_csv_path <- file.path(base_enrich_dir, paste0("Enrichment_", db_name, ".csv"))
      plot_path <- file.path(base_enrich_dir, paste0("Enrichment_", db_name, ".png"))
      cnetplot_path <- file.path(base_enrich_dir, paste0("Cnet_", db_name, ".png"))
      
      df <- as.data.frame(res_readable)
      if("geneID" %in% names(df)) { df$geneID <- stringr::str_replace_all(df$geneID, "/", ", ") }
      if("core_enrichment" %in% names(df)){ df$Associated_Genes <- str_replace_all(df$core_enrichment, "/", ", "); df$core_enrichment <- NULL }
      write.csv(df, enrichment_csv_path, row.names=FALSE)
      
      # Generate Dot Plot
      plot_title <- paste(db_name, "Enrichment for", title_prefix)
      tryCatch({
        p_viz <- dotplot(res_readable, showCategory=20, title=plot_title) + theme_bw(base_size = 12)
        ggsave(plot_path, plot=p_viz, width=10, height=8, dpi=300)
      }, error = function(e) NULL)
      
      # Network Plot
      if(!is.null(gene_list_fc) && db_name %in% c("GO_BP", "KEGG", "Reactome")) {
        tryCatch({
          # Explicitly pass color.params to avoid deprecation warning if possible, but keep robust fallback
          cnet_p <- cnetplot(res_readable, foldChange = gene_list_fc, circular = FALSE, color_by = "logFC", showCategory = 10, node_label = "all") +
            ggtitle(paste("Gene-Concept Network:", db_name)) +
            scale_color_gradient2(low = "#0571b0", mid = "white", high = "#ca0020", midpoint = 0, name = "logFC")
          ggsave(cnetplot_path, plot=cnet_p, width=12, height=10, dpi=300)
        }, error = function(e) NULL)
      }
      
      file.create(receipt_success) 
    } else {
      send_progress(paste0("      No significant terms found for ", db_name, "."))
      file.create(receipt_no_results) 
    }
  }
  
  # --- Standard ORA Analysis ---
  run_and_save("GO_BP", function() enrichGO(genes, OrgDb=org.Hs.eg.db, keyType="SYMBOL", ont="BP", readable=TRUE))
  if(!is.null(entrez_ids)) {
    run_and_save("KEGG", function() enrichKEGG(entrez_ids, organism='hsa'))
    run_and_save("Reactome", function() ReactomePA::enrichPathway(entrez_ids, organism="human", readable=TRUE))
  }
  
  # --- Integrated Enrichr ---
  run_enrichr_set <- function(dbs, prefix_tag = "Enrichr") {
    send_progress(paste0("      Querying Enrichr DBs: ", paste(dbs, collapse=", ")))
    setEnrichrSite("Enrichr")
    
    tryCatch({
      res_list <- enrichr(genes, dbs)
      if(is.null(res_list)) return()
      
      for(db in names(res_list)) {
        df <- res_list[[db]]
        if (nrow(df) > 0) {
          df <- df %>% filter(Adjusted.P.value < 0.05) %>% arrange(Adjusted.P.value)
          if(nrow(df) > 0) {
            write.csv(df, file.path(base_enrich_dir, paste0(prefix_tag, "_", db, ".csv")), row.names=FALSE)
            
            plot_df <- head(df, 15)
            fill_col <- if(grepl("ChEA|TRRUST|ENCODE", db)) "darkorange" else "steelblue"
            
            p_bar <- ggplot(plot_df, aes(x=reorder(Term, -log10(Adjusted.P.value)), y=-log10(Adjusted.P.value))) +
              geom_bar(stat="identity", fill=fill_col) +
              coord_flip() +
              theme_bw() +
              labs(title=paste0(db, " (", title_prefix, ")"), x="Term", y="-Log10 Adj. P-val")
            
            ggsave(file.path(base_enrich_dir, paste0(prefix_tag, "_Plot_", db, ".png")), p_bar, width=10, height=6)
          }
        }
      }
    }, error = function(e) send_progress(paste0("      Enrichr query failed: ", e$message)))
  }
  
  general_dbs <- c("DisGeNET", "MSigDB_Hallmark_2020", "GO_Biological_Process_2023")
  run_enrichr_set(general_dbs, "Enrichment")
  
  # Updated ChEA_2022
  tf_dbs <- c("ChEA_2022", "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X", "TRRUST_Transcription_Factors_2019")
  run_enrichr_set(tf_dbs, "TF_Enrichment")
}


# === 🛰️ MERGED Advanced PPI Network Analysis Function (Meta-Analysis Style) ===
# UPDATED ARGUMENTS: Added survival_data, cluster_algo, hub_measure, ppi_score
run_ppi_analysis <- function(all_sig_genes, gene_list_fc, contrast_name, ppi_output_dir, plot_font_size, ppi_score = 900, priority_genes = NULL, survival_data = NULL, cluster_algo = "louvain", hub_measure = "pagerank") {
  send_progress(paste0("--- 🛰️ Starting Advanced PPI Network Analysis for ", contrast_name, " ---"))
  
  ppi_png_path <- file.path(ppi_output_dir, paste0("PPI_Network_Hubs_", gsub("[^A-Za-z0-9_]", "_", contrast_name), ".png"))
  ppi_stats_path <- file.path(ppi_output_dir, "PPI_statistics.csv")
  node_table_path <- file.path(ppi_output_dir, "PPI_Node_Table.csv")
  
  # --- 0. PRE-FILTERING (Strict Filter: |logFC| >= 2, P <= 0.05 + RESCUE) ---
  if (!is.null(gene_list_fc)) {
    # 1. Filter all_sig_genes to only those with valid logFC
    valid_genes <- intersect(all_sig_genes, names(gene_list_fc))
    
    # 2. (REMOVED HARDCODED STRICT FILTER) - Respecting upstream user settings
    # We maintain strict_genes as valid_genes unless specific high-filtering is requested
    strict_genes <- valid_genes 
    
    # 3. PRIORITY RESCUE: Add priority_genes if provided (Survival genes)
    if (!is.null(priority_genes)) {
      # Only rescue if they exist in the input expression data
      rescue_genes <- intersect(priority_genes, names(gene_list_fc))
      if(length(setdiff(rescue_genes, strict_genes)) > 0) {
        send_progress(paste0("    Rescuing ", length(rescue_genes), " priority genes (Survival/HR) regardless of filters."))
        strict_genes <- unique(c(strict_genes, rescue_genes))
      }
    }
    
    all_sig_genes <- strict_genes
  } 
  
  local_min_cluster_size <- 5 
  if (length(all_sig_genes) < local_min_cluster_size) {
    send_progress(paste0("SKIPPED: PPI for '", contrast_name, "' - too few genes."))
    return()
  }
  
  # --- LOGIC: We need to BUILD the network object 'net_main' if:
  # 1. The image is missing (we need to plot it)
  # 2. OR if Enrichment folders are missing (we need to analyze clusters)
  # We cannot skip building 'net_main' if we need to do #2, even if #1 exists.
  
  need_plot <- !file.exists(ppi_png_path)
  need_enrichment <- !dir.exists(file.path(ppi_output_dir, "Cluster_Analysis")) || !dir.exists(file.path(ppi_output_dir, "Comparison_Top5_Clusters"))
  
  if (!need_plot && !need_enrichment) {
    send_progress(paste0("SKIPPED: PPI for '", contrast_name, "' (All outputs exist). Delete PNG or folders to regenerate."))
    return()
  }
  
  # --- STRING DB Init (Run if we need anything) ---
  # UPDATED: Use ppi_score argument
  string_db <- STRINGdb::STRINGdb$new(
    version="11.5", species=9606, score_threshold=ppi_score, input_directory=string_cache_dir
  )
  
  mapped_df <- data.frame(gene = all_sig_genes)
  if (!is.null(gene_list_fc)) mapped_df$logFC <- gene_list_fc[match(mapped_df$gene, names(gene_list_fc))]
  
  mapped_genes <- string_db$map(mapped_df, "gene", removeUnmappedRows = TRUE)
  mapped_genes <- mapped_genes %>% dplyr::distinct(STRING_id, .keep_all = TRUE)
  
  if (nrow(mapped_genes) < 2) return()
  
  interactions <- string_db$get_interactions(mapped_genes$STRING_id)
  if (nrow(interactions) == 0) return()
  
  net <- igraph::graph_from_data_frame(d=interactions, directed=FALSE)
  
  vertex_metadata <- data.frame(STRING_id = V(net)$name) %>% dplyr::left_join(mapped_genes, by = "STRING_id")
  V(net)$gene <- vertex_metadata$gene
  if("logFC" %in% colnames(vertex_metadata)) V(net)$logFC <- vertex_metadata$logFC
  
  # Filter Main Components
  components <- igraph::components(net)
  
  # LOGIC CHANGE: Since we are strictly filtering by LogFC > 2, we can be lenient with component size.
  # Keep any component with at least 2 nodes (interactions exist).
  # Also ALWAYS keep components containing priority genes, even if disconnected from main mass.
  keep_clusters <- which(components$csize > 1) # Keep pairs/triplets
  
  if (!is.null(priority_genes)) {
    # Find which clusters contain priority genes
    p_indices <- which(V(net)$gene %in% priority_genes)
    if (length(p_indices) > 0) {
      p_clusters <- unique(components$membership[p_indices])
      keep_clusters <- unique(c(keep_clusters, p_clusters))
    }
  }
  
  valid_component_ids <- keep_clusters
  if (length(valid_component_ids) == 0) valid_component_ids <- which.max(components$csize)
  net_main <- igraph::induced_subgraph(net, V(net)[components$membership %in% valid_component_ids])
  
  # Check for missing priority genes
  if (!is.null(priority_genes)) {
    missing_p <- setdiff(intersect(priority_genes, all_sig_genes), V(net_main)$gene)
    if (length(missing_p) > 0) {
      send_progress(paste0("    NOTE: The following Priority Genes were dropped (No PPI interactions > ", ppi_score, " with other DEGs): ", paste(head(missing_p, 5), collapse=", ")))
    }
  }
  
  if (vcount(net_main) > 0) {
    # --- Dynamic Hub Measure ---
    send_progress(paste0("    Calculating Hubs using: ", hub_measure))
    hub_scores <- switch(hub_measure,
                         "pagerank" = igraph::page_rank(net_main, directed = FALSE)$vector,
                         "degree" = igraph::degree(net_main, normalized = TRUE),
                         "betweenness" = igraph::betweenness(net_main, normalized = TRUE),
                         "closeness" = igraph::closeness(net_main, normalized = TRUE),
                         igraph::page_rank(net_main, directed = FALSE)$vector # Default fallback
    )
    V(net_main)$hub_score <- hub_scores
    
    # FIX: Set seed for reproducible cluster assignment (prevents "flipping" of cluster IDs)
    set.seed(42)
    # --- Dynamic Clustering ---
    send_progress(paste0("    Detecting Clusters using: ", cluster_algo))
    louvain_clusters <- switch(cluster_algo,
                               "louvain" = igraph::cluster_louvain(net_main),
                               "walktrap" = igraph::cluster_walktrap(net_main),
                               "fast_greedy" = igraph::cluster_fast_greedy(net_main),
                               "leiden" = igraph::cluster_louvain(net_main), # Fallback if leiden not installed, or can use cluster_leiden if available
                               igraph::cluster_louvain(net_main) # Default
    )
    
    V(net_main)$cluster <- igraph::membership(louvain_clusters)
    
    cluster_sizes <- sort(sizes(louvain_clusters), decreasing = TRUE)
    all_valid_clusters <- names(cluster_sizes)[cluster_sizes > 3]
    if(length(all_valid_clusters) == 0) all_valid_clusters <- names(cluster_sizes)[1:min(5, length(cluster_sizes))]
    top_5_ids <- names(cluster_sizes)[1:min(5, length(cluster_sizes))]
    
    cluster_rank_map <- setNames(seq_along(all_valid_clusters), all_valid_clusters)
    
    # ----------------------------------------------------------------------
    # 1. ENRICHMENT ANALYSIS (Granular Check: Run if folder missing)
    # ----------------------------------------------------------------------
    cluster_out_dir <- file.path(ppi_output_dir, "Cluster_Analysis")
    detected_tfs <- list() # Store for labeling later
    
    # Helper to extract ONE TF per cluster from CSVs
    extract_one_tf <- function(c_dir, cid, cluster_genes_vec) {
      tf_files <- list.files(c_dir, pattern = "TF_Enrichment.*\\.csv", full.names = TRUE)
      if (length(tf_files) > 0) {
        # Check files in order found (ChEA usually first)
        for(fpath in tf_files) {
          tryCatch({
            tf_data <- read.csv(fpath)
            if (nrow(tf_data) > 0) {
              # Look for the first Term that has a TF symbol existing in our cluster
              # We check top 10 most significant terms
              for(row_idx in 1:min(10, nrow(tf_data))) {
                term_raw <- as.character(tf_data$Term[row_idx])
                # Extract potential TF (e.g. "FOXM1" from "FOXM1 25889361..." or "YBX1 human")
                # Split by space, underscore, or parenthesis to handle various formats
                tf_candidate <- strsplit(term_raw, "[ _(]")[[1]][1]
                tf_candidate <- trimws(tf_candidate)
                
                # Check case-insensitive match against cluster genes
                if (toupper(tf_candidate) %in% toupper(cluster_genes_vec)) {
                  # Return the actual symbol from the cluster list (preserves correct casing)
                  actual_symbol <- cluster_genes_vec[which(toupper(cluster_genes_vec) == toupper(tf_candidate))[1]]
                  return(actual_symbol) 
                }
              }
            }
          }, error = function(e) NULL)
        }
      }
      return(NULL)
    }
    
    if (dir.exists(cluster_out_dir)) {
      send_progress("    Skipping Sub-Cluster Enrichment (Folder exists). Loading existing TF data for labeling...")
      # If folder exists, we still need 'detected_tfs' for the plot.
      for (i in seq_along(all_valid_clusters)) {
        cid <- all_valid_clusters[i]
        rank_idx <- cluster_rank_map[cid]
        c_dir <- file.path(cluster_out_dir, paste0("Cluster_", rank_idx))
        
        cluster_genes <- V(net_main)$gene[as.character(V(net_main)$cluster) == cid]
        found_tf <- extract_one_tf(c_dir, cid, cluster_genes)
        if(!is.null(found_tf)) detected_tfs[[cid]] <- found_tf
      }
    } else {
      send_progress("    Running Sub-Cluster Enrichment Analysis...")
      dir.create(cluster_out_dir, showWarnings = FALSE)
      
      for (i in seq_along(all_valid_clusters)) {
        cid <- all_valid_clusters[i]
        rank_idx <- cluster_rank_map[cid]
        rank_label <- paste0("Cluster_", rank_idx)
        
        cluster_genes <- V(net_main)$gene[as.character(V(net_main)$cluster) == cid]
        cluster_genes <- cluster_genes[!is.na(cluster_genes)]
        
        if (length(cluster_genes) >= 3) {
          c_dir <- file.path(cluster_out_dir, rank_label)
          run_advanced_enrichment_core(cluster_genes, gene_list_fc, paste0("PPI ", rank_label), c_dir)
          
          # EXTRACT ONE TF FOR LABELING
          found_tf <- extract_one_tf(c_dir, cid, cluster_genes)
          if(!is.null(found_tf)) detected_tfs[[cid]] <- found_tf
        }
      }
    }
    
    # ----------------------------------------------------------------------
    # 2. PLOTTING (Granular Check: Run if PNG missing)
    # ----------------------------------------------------------------------
    if (need_plot) {
      send_progress("    Generating PPI Network Plot...")
      
      # --- AUTO-DETECT SURVIVAL FILE IF MISSING ---
      if (is.null(survival_data)) {
        # Check user specific path first - UPDATED FOR REPRODUCIBILITY
        hardcoded_path <- "./data/Survival_Data.csv"
        if (file.exists(hardcoded_path)) {
          tryCatch({
            survival_data <- read.csv(hardcoded_path)
            send_progress(paste0("    Found Survival Data at: ", hardcoded_path))
          }, error = function(e) NULL)
        }
        
        # If still null, look in parent directories for standard survival output
        if (is.null(survival_data)) {
          parent_dir <- dirname(ppi_output_dir)
          surv_files <- list.files(parent_dir, pattern = "Survival_Analysis_Results.*\\.csv", recursive = TRUE, full.names = TRUE)
          if (length(surv_files) > 0) {
            tryCatch({
              survival_data <- read.csv(surv_files[1])
              send_progress(paste0("    Found Survival Data: ", basename(surv_files[1])))
            }, error = function(e) NULL)
          }
        }
      }
      
      # --- ROBUST TF CHECK (GO) ---
      # REMOVED generic GO check to avoid over-labeling. 
      # Labeling now relies STRICTLY on 'detected_tfs' from enrichment files.
      known_tfs <- c() 
      
      # --- LABELING LOGIC ---
      V(net_main)$label_text <- NA
      V(net_main)$label_hex <- "black"
      V(net_main)$shape_code <- 21 # Default Circle
      
      # RESTORED: Quantile-based thresholds to prevent clutter with larger network
      up_thresh <- quantile(V(net_main)$logFC, 0.95, na.rm=TRUE)
      down_thresh <- quantile(V(net_main)$logFC, 0.05, na.rm=TRUE)
      
      is_top_hub <- function(node_idx) {
        c_id <- as.character(V(net_main)$cluster[node_idx])
        if (!c_id %in% top_5_ids) return(FALSE)
        c_nodes <- which(as.character(V(net_main)$cluster) == c_id)
        c_scores <- V(net_main)$hub_score[c_nodes]
        names(c_scores) <- c_nodes
        top_3_in_cluster <- names(head(sort(c_scores, decreasing=TRUE), 3))
        return(as.character(node_idx) %in% top_3_in_cluster)
      }
      
      is_enriched_tf <- function(node_idx) {
        c_id <- as.character(V(net_main)$cluster[node_idx])
        sym <- V(net_main)$gene[node_idx]
        if (!is.null(detected_tfs[[c_id]]) && sym %in% detected_tfs[[c_id]]) return(TRUE)
        # Check against list. If sym is the ONE found TF for this cluster, return TRUE
        return(FALSE)
      }
      
      for(i in 1:vcount(net_main)) {
        lfc <- V(net_main)$logFC[i]
        sym <- V(net_main)$gene[i]
        deg <- igraph::degree(net_main, v=i)
        
        # Check Survival (High Risk > 1.8)
        is_high_risk <- FALSE
        if (!is.null(survival_data) && "Gene" %in% colnames(survival_data)) {
          # Try to find HR or Hazard.Ratio column
          hr_col <- grep("HR|Hazard", colnames(survival_data), ignore.case = TRUE, value = TRUE)[1]
          if (!is.na(hr_col)) {
            match_idx <- which(survival_data$Gene == sym)
            if (length(match_idx) > 0) {
              hr_val <- survival_data[[hr_col]][match_idx[1]]
              if (!is.na(hr_val) && hr_val > 1.8) is_high_risk <- TRUE
            }
          }
        }
        
        if (is_high_risk) {
          V(net_main)$label_text[i] <- sym
          V(net_main)$label_hex[i] <- "black"
          V(net_main)$shape_code[i] <- 23 # Diamond/Rhombus
        } else if (!is.null(priority_genes) && sym %in% priority_genes) {
          V(net_main)$label_text[i] <- sym
          V(net_main)$label_hex[i] <- "#800080" # Purple (Survival/Priority List)
        } else if (is_enriched_tf(i)) {
          V(net_main)$label_text[i] <- sym
          V(net_main)$label_hex[i] <- "#E6B800" # Yellow (Top TF from Enrichment) - RE-ENABLED
        } else if (is_top_hub(i)) {
          V(net_main)$label_text[i] <- sym
          V(net_main)$label_hex[i] <- "#008000" # Green (Hub)
        } else if (!is.na(lfc) && lfc >= up_thresh && deg > 5) {
          V(net_main)$label_text[i] <- sym
          V(net_main)$label_hex[i] <- "#D20000" # Red (Top 5% Up)
        } else if (!is.na(lfc) && lfc <= down_thresh && deg > 5) {
          V(net_main)$label_text[i] <- sym
          V(net_main)$label_hex[i] <- "#0000D2" # Blue (Top 5% Down)
        }
      }
      V(net_main)$label <- V(net_main)$label_text
      
      node_clusters <- as.character(V(net_main)$cluster)
      V(net_main)$cluster_display <- ifelse(node_clusters %in% all_valid_clusters, paste0("Cluster ", cluster_rank_map[node_clusters]), "Other")
      ordered_levels <- paste0("Cluster ", seq_along(all_valid_clusters))
      if("Other" %in% V(net_main)$cluster_display) ordered_levels <- c(ordered_levels, "Other")
      V(net_main)$cluster_display <- factor(V(net_main)$cluster_display, levels = ordered_levels)
      
      n_clusters <- length(all_valid_clusters)
      base_palette <- RColorBrewer::brewer.pal(min(9, max(3, n_clusters)), "Set1")
      if (n_clusters > length(base_palette)) cluster_cols <- colorRampPalette(base_palette)(n_clusters) else cluster_cols <- base_palette[1:n_clusters]
      names(cluster_cols) <- paste0("Cluster ", 1:n_clusters)
      if("Other" %in% levels(V(net_main)$cluster_display)) cluster_cols <- c(cluster_cols, "Other" = "grey80")
      
      rank_sizes <- cluster_sizes[all_valid_clusters] 
      legend_labels <- setNames(paste0("Cluster ", seq_along(all_valid_clusters), " (n=", rank_sizes, ")"), paste0("Cluster ", seq_along(all_valid_clusters)))
      legend_breaks <- paste0("Cluster ", 1:n_clusters)
      
      node_cluster_map <- setNames(as.character(V(net_main)$cluster_display), V(net_main)$name)
      edge_list_names <- as_edgelist(net_main, names = TRUE)
      edge_types <- apply(edge_list_names, 1, function(x) {
        c1 <- node_cluster_map[x[1]]; c2 <- node_cluster_map[x[2]]
        if(c1 == c2 && c1 != "Other") return(c1) else return("Inter-cluster")
      })
      E(net_main)$edge_type <- edge_types
      edge_palette <- c(cluster_cols, "Inter-cluster" = "#666666")
      alpha_vals <- c("Inter-cluster" = 0.5)
      if(length(cluster_cols) > 0) alpha_vals <- c(alpha_vals, setNames(rep(0.6, length(grep("Cluster", names(cluster_cols), value=TRUE))), grep("Cluster", names(cluster_cols), value=TRUE)))
      if ("Other" %in% names(cluster_cols)) alpha_vals["Other"] <- 0.4
      
      # Use graphopt layout with high charge for maximum node separation (reduces overlap)
      # FIX: Set seed for reproducible layout
      set.seed(42)
      node_spacing_dial <- 300.0 # FIX: Define this variable explicitly
      l_main <- create_layout(net_main, layout = 'graphopt', charge = 0.1, niter = 10000)
      l_main$x <- l_main$x * node_spacing_dial
      l_main$y <- l_main$y * node_spacing_dial
      
      gg_ppi <- ggraph(l_main) + 
        geom_edge_fan(aes(color = edge_type, alpha = edge_type), width=0.6, show.legend = FALSE) +
        scale_edge_color_manual(values = edge_palette) + scale_edge_alpha_manual(values = alpha_vals) +
        geom_node_point(aes(size = hub_score, fill = logFC, color = cluster_display, shape = I(shape_code)), stroke = 1.5) +
        scale_color_manual(values = cluster_cols, name = "Clusters", labels = legend_labels, breaks = legend_breaks) + 
        scale_fill_gradient2(low = "#0571b0", mid = "white", high = "#ca0020", midpoint = 0, name = "logFC") +
        scale_size_continuous(range = c(2, 7), name = "Hub Score") +
        geom_node_text(aes(label = label_text, color = I(label_hex)), repel = TRUE, size = 4.5, max.overlaps = Inf, bg.color = "white", bg.r = 0.15, fontface = "bold") +
        theme_void() + theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold")) +
        labs(title = paste("PPI Network:", gsub("_", " ", contrast_name)))
      ggsave(ppi_png_path, plot=gg_ppi, width=12, height=10, dpi=300, bg="white")
      
      # Save Stats
      # FIX: Ensure stats_df is created properly
      stats_df <- data.frame(
        Metric = c("Nodes", "Edges", "Average Degree", "Clustering Coefficient"),
        Value = c(
          vcount(net_main),
          ecount(net_main),
          round(mean(igraph::degree(net_main)), 2),
          round(transitivity(net_main, type = "global"), 4)
        )
      )
      write.csv(stats_df, ppi_stats_path, row.names = FALSE)
      
      # Save Node Table
      all_sorted_ids <- names(cluster_sizes)
      all_rank_map <- setNames(seq_along(all_sorted_ids), all_sorted_ids)
      all_node_ranks <- all_rank_map[as.character(V(net_main)$cluster)]
      node_table <- data.frame(
        GeneSymbol = V(net_main)$gene, STRING_ID = V(net_main)$name, Cluster_Rank = all_node_ranks,
        Cluster_Label = V(net_main)$cluster_display, PageRank_Score = round(V(net_main)$hub_score, 4),
        Degree = igraph::degree(net_main), Betweenness = round(igraph::betweenness(net_main), 2), logFC = round(V(net_main)$logFC, 3)
      )
      node_table <- node_table[order(node_table$Cluster_Rank, -node_table$PageRank_Score), ]
      write.csv(node_table, node_table_path, row.names = FALSE)
      
      # Save Top 3 Hubs
      hubs_report <- data.frame()
      unique_clusters <- grep("Cluster", unique(node_table$Cluster_Label), value=TRUE)
      for(clust in unique_clusters) {
        clust_data <- node_table[node_table$Cluster_Label == clust, ]
        top_3 <- head(clust_data[order(clust_data$PageRank_Score, decreasing=TRUE), ], 3)
        if(nrow(top_3) > 0) { top_3$Hub_Rank <- 1:nrow(top_3); hubs_report <- rbind(hubs_report, top_3) }
      }
      if(nrow(hubs_report) > 0) write.csv(hubs_report, file.path(ppi_output_dir, "PPI_Top_Hubs_Summary.csv"), row.names = FALSE)
      
      # Grid Plots (Depend on Plotting)
      if(length(top_5_ids) > 0 && require("patchwork", quietly=TRUE)) {
        send_progress("    Generating Integrated Network + Donut Panels...")
        panel_plots <- list()
        for(i in seq_along(top_5_ids)) {
          cid <- top_5_ids[i]; rank_idx <- cluster_rank_map[cid]; rank_label <- paste0("Cluster ", rank_idx)
          cluster_nodes <- V(net_main)[as.character(V(net_main)$cluster) == cid]
          if(length(cluster_nodes) < 2) next
          subg <- igraph::induced_subgraph(net_main, cluster_nodes)
          # FIX: Set seed for reproducible sub-layouts
          set.seed(42)
          l_mini <- create_layout(subg, layout = 'graphopt', charge = 0.1, niter = 5000)
          
          p_mini_net <- ggraph(l_mini) + geom_edge_link(color = "grey80", alpha = 0.6) +
            geom_node_point(aes(size = hub_score, fill = logFC), color="black", shape = 21) + 
            scale_fill_gradient2(low = "#0571b0", mid = "white", high = "#ca0020", midpoint = 0, guide = "none") + 
            scale_size_continuous(range = c(3, 8), guide = "none") +
            geom_node_text(aes(label = label_text), repel = TRUE, size = 1.0, max.overlaps=Inf) +
            theme_void() + labs(subtitle = paste0(rank_label, " Network")) + theme(plot.subtitle = element_text(hjust=0.5, face="bold"))
          
          c_genes <- V(subg)$gene; c_genes <- c_genes[!is.na(c_genes)]
          p_donut <- ggplot() + theme_void() 
          tryCatch({ ego <- enrichGO(c_genes, OrgDb=org.Hs.eg.db, keyType="SYMBOL", ont="BP", pvalueCutoff=0.05)
          if(!is.null(ego) && nrow(ego) > 0) {
            top_terms <- head(as.data.frame(ego), 4)
            if(nrow(top_terms) > 0) {
              top_terms$Description <- stringr::str_trunc(top_terms$Description, 25)
              top_terms$Fraction <- top_terms$Count / sum(top_terms$Count)
              top_terms$ymax <- cumsum(top_terms$Fraction); top_terms$ymin <- c(0, head(top_terms$ymax, n=-1))
              p_donut <- ggplot(top_terms, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Description)) +
                geom_rect(color="white") + coord_polar(theta="y") + xlim(c(1, 4)) +
                scale_fill_brewer(palette = "Pastel1") + theme_void() + theme(legend.position = "right", legend.text = element_text(size=7), legend.title = element_blank())
            }
          }}, error=function(e) NULL)
          combined_panel <- p_mini_net / p_donut + plot_layout(heights = c(2, 1)) + plot_annotation(tag_levels = list(paste0("(", letters[i], ")"))) & theme(plot.tag = element_text(face = 'bold', size = 12))
          panel_plots[[length(panel_plots) + 1]] <- combined_panel
          ggsave(file.path(ppi_output_dir, paste0("Integrated_Panel_Cluster_", rank_idx, ".png")), combined_panel, width=6, height=8, bg="white")
        }
        if(length(panel_plots) > 0) {
          final_grid <- wrap_plots(panel_plots, nrow = 1) + plot_annotation(title = paste("Integrated Cluster Analysis:", gsub("_", " ", contrast_name)), theme = theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5)))
          ggsave(file.path(ppi_output_dir, "Integrated_Cluster_Grid.png"), final_grid, width = 6 * length(panel_plots), height = 8, dpi=300, bg="white")
        }
      }
    } else {
      send_progress("SKIPPED: Network Plotting (Image exists).")
    }
    
    # ----------------------------------------------------------------------
    # 3. COMPARE CLUSTER ANALYSIS (Granular Check: Run if folders missing)
    # ----------------------------------------------------------------------
    run_compare_analysis_subset <- function(cluster_ids_to_use, folder_suffix) {
      subset_dir <- file.path(ppi_output_dir, paste0("Comparison_", folder_suffix))
      if(dir.exists(subset_dir)) {
        send_progress(paste0("    Skipping Compare Analysis for ", folder_suffix, " (Folder exists)."))
        return()
      }
      dir.create(subset_dir, showWarnings=FALSE)
      send_progress(paste0("    Running Compare Cluster Analysis for: ", folder_suffix))
      
      cluster_gene_list <- list()
      for(i in seq_along(cluster_ids_to_use)) {
        cid <- cluster_ids_to_use[i]
        rank_idx <- cluster_rank_map[cid]
        c_genes <- V(net_main)$gene[as.character(V(net_main)$cluster) == cid]
        c_genes <- c_genes[!is.na(c_genes)]
        if(length(c_genes) >= 3) cluster_gene_list[[paste0("Cluster ", rank_idx)]] <- c_genes
      }
      
      if(length(cluster_gene_list) >= 2) {
        calc_height <- max(8, length(cluster_gene_list) * 0.5 + 3)
        entrez_cluster_list <- lapply(cluster_gene_list, function(x) {
          tryCatch(bitr(x, fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db)$ENTREZID, error=function(e) NULL)
        })
        entrez_cluster_list <- entrez_cluster_list[sapply(entrez_cluster_list, length) > 2]
        style_compare_plot <- function(p, title_str) {
          p + scale_color_viridis_c(option = "magma", direction = -1, name = "Adj. P-val", guide = guide_colorbar(order = 1)) +
            scale_size_continuous(range = c(3, 8), name = "Gene Ratio", guide = guide_legend(order = 2)) +
            labs(title = gsub("_", " ", title_str)) + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
        tryCatch({ ck_go <- compareCluster(geneCluster = entrez_cluster_list, fun = "enrichGO", OrgDb = org.Hs.eg.db, ont="BP")
        if(!is.null(ck_go) && nrow(as.data.frame(ck_go)) > 0) {
          ck_go <- setReadable(ck_go, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
          ggsave(file.path(subset_dir, "Cluster_Compare_GO_BP.png"), style_compare_plot(dotplot(ck_go, showCategory=5), "Compare Cluster GO BP"), width=12, height=calc_height)
        }}, error=function(e) NULL)
        
        tryCatch({ ck_kegg <- compareCluster(geneCluster = entrez_cluster_list, fun = "enrichKEGG", organism="hsa")
        if(!is.null(ck_kegg) && nrow(as.data.frame(ck_kegg)) > 0) {
          ck_kegg <- setReadable(ck_kegg, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
          ggsave(file.path(subset_dir, "Cluster_Compare_KEGG.png"), style_compare_plot(dotplot(ck_kegg, showCategory=5), "Compare Cluster KEGG"), width=12, height=calc_height)
        }}, error=function(e) NULL)
        
        tryCatch({ if(require("ReactomePA", quietly=TRUE)) {
          ck_react <- compareCluster(geneCluster = entrez_cluster_list, fun = "enrichPathway", organism="human")
          if(!is.null(ck_react) && nrow(as.data.frame(ck_react)) > 0) {
            ck_react <- setReadable(ck_react, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
            ggsave(file.path(subset_dir, "Cluster_Compare_Reactome.png"), style_compare_plot(dotplot(ck_react, showCategory=5), "Compare Cluster Reactome"), width=12, height=calc_height)
          }
        }}, error=function(e) NULL)
        
        tf_dbs <- c("ChEA_2022", "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X", "TRRUST_Transcription_Factors_2019")
        for(db_name in tf_dbs) {
          tf_results_df <- data.frame()
          for(grp_name in names(cluster_gene_list)) {
            tryCatch({
              res_list <- enrichr(cluster_gene_list[[grp_name]], db_name)
              df <- res_list[[db_name]]
              if(!is.null(df) && nrow(df) > 0) {
                df <- df %>% filter(Adjusted.P.value < 0.05) %>% head(5)
                if(nrow(df) > 0) {
                  df$GeneRatio <- 0.1; if("Overlap" %in% colnames(df)) { parts <- strsplit(df$Overlap, "/"); df$HitCount <- as.numeric(sapply(parts, `[`, 1)); df$GeneRatio <- df$HitCount / length(cluster_gene_list[[grp_name]]) }
                  df$Cluster <- grp_name; tf_results_df <- rbind(tf_results_df, df[, c("Term", "Adjusted.P.value", "GeneRatio", "Cluster")])
                }
              }
            }, error=function(e) NULL)
          }
          if(nrow(tf_results_df) > 0) {
            tf_results_df$ClusterNum <- as.numeric(gsub("Cluster ", "", tf_results_df$Cluster))
            tf_results_df$Cluster <- factor(tf_results_df$Cluster, levels = paste0("Cluster ", sort(unique(tf_results_df$ClusterNum))))
            p_tf <- ggplot(tf_results_df, aes(x=Cluster, y=reorder(Term, -log10(Adjusted.P.value)), size=GeneRatio, color=Adjusted.P.value)) +
              geom_point() + scale_color_viridis_c(option = "magma", direction = -1, name = "Adj. P-val", guide = guide_colorbar(order = 1)) +
              scale_size_continuous(range = c(3, 8), name = "Gene Ratio", guide = guide_legend(order = 2)) +
              theme_bw() + labs(title=paste("Compare Cluster:", gsub("_", " ", db_name)), x="", y="") +
              theme(axis.text.x = element_text(angle=45, hjust=1))
            ggsave(file.path(subset_dir, paste0("Cluster_Compare_TF_", gsub("[^A-Za-z0-9]", "_", db_name), ".png")), p_tf, width=12, height=calc_height)
          }
        }
      }
    }
    run_compare_analysis_subset(top_5_ids, "Top5_Clusters")
    if (length(all_valid_clusters) > length(top_5_ids)) {
      run_compare_analysis_subset(all_valid_clusters, "All_Clusters")
    }
  }
}

# --- Internal plotting function for generate_qc_plots ---
.create_qc_plots <- function(expression_matrix, metadata, prefix, out_dir, project_name = "Analysis") {
  # FIX: Ensure output directory exists before saving anything.
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  if(ncol(expression_matrix) < 2) {
    send_progress(paste0("  Skipping plots for ", prefix, " as there are fewer than 2 samples."))
    return()
  }
  
  # --- ADDED: Boxplot and Density Plots (Reinstated) ---
  # Reshape data for ggplot
  tryCatch({
    long_data <- as.data.frame(expression_matrix) %>%
      tibble::rownames_to_column("Gene") %>%
      tidyr::pivot_longer(cols = -Gene, names_to = "Sample", values_to = "Expression") %>%
      dplyr::left_join(metadata, by = "Sample")
    
    # 1. Boxplot
    p_box <- ggplot(long_data, aes(x=Sample, y=Expression, fill=Group)) +
      geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      labs(title = paste("Expression Distribution -", prefix), y = "Expression Level")
    ggsave(file.path(out_dir, paste0(project_name, "_Boxplot_", prefix, ".png")), p_box, width = 12, height = 8)
    
    # 2. Density Plot
    p_density <- ggplot(long_data, aes(x=Expression, color=Group, group=Sample)) +
      geom_density(alpha = 0.2) +
      theme_bw() +
      labs(title = paste("Density Plot -", prefix), x = "Expression Level")
    ggsave(file.path(out_dir, paste0(project_name, "_Density_", prefix, ".png")), p_density, width = 10, height = 6)
    
  }, error = function(e) send_progress(paste0("  Warning: Box/Density plots failed for ", prefix, ": ", e$message)))
  
  
  row_variances <- apply(expression_matrix, 1, var, na.rm = TRUE)
  expression_matrix_var <- expression_matrix[row_variances > 1e-6, , drop = FALSE]
  
  if(nrow(expression_matrix_var) < 2) {
    send_progress(paste0("  Skipping PCA/PLSDA for ", prefix, " as there are fewer than 2 genes with variance."))
  } else {
    # PCA
    pca_data <- prcomp(t(expression_matrix_var), scale. = TRUE)
    pca_df <- as.data.frame(pca_data$x) %>% tibble::rownames_to_column("Sample") %>% dplyr::left_join(metadata, by = "Sample")
    percent_var <- pca_data$sdev^2 / sum(pca_data$sdev^2)
    
    pca_plot_path <- file.path(out_dir, paste0(project_name, "_PCA_Plot_", prefix, ".png"))
    if(!file.exists(pca_plot_path)){
      pca_plot <- ggplot(pca_df, aes(x=PC1, y=PC2, color=Group))
      if (length(unique(metadata$Batch)) > 1) {
        pca_plot <- pca_plot + geom_point(aes(shape=Batch), size=5, alpha=0.8)
      } else {
        pca_plot <- pca_plot + geom_point(size=5, alpha=0.8)
      }
      pca_plot <- pca_plot +
        ggrepel::geom_text_repel(aes(label = Sample), size = 3.5, max.overlaps = 15, box.padding = 0.5) +
        labs(title = paste("PCA Plot -", prefix), x = paste0("PC1: ", round(percent_var[1] * 100), "% variance"), y = paste0("PC2: ", round(percent_var[2] * 100), "% variance")) +
        theme_bw(base_size = plot_font_size) + theme(legend.position = "bottom", plot.title = element_text(face="bold"))
      
      # FIX: Only add ellipse if all groups have at least 4 points
      group_counts <- table(pca_df$Group)
      if (all(group_counts >= 4)) {
        pca_plot <- pca_plot + stat_ellipse(aes(group = Group, color = Group), type = "t", level = 0.95, linetype = 2, linewidth = 1, show.legend = FALSE)
      } else {
        send_progress(paste0("  Note: Ellipses skipped for ", prefix, " (Need >=4 samples per group)."))
      }
      
      tryCatch({
        ggsave(pca_plot_path, plot=pca_plot, width=10, height=8, dpi=300)
      }, error = function(e) {
        send_progress(paste0("  Error saving PCA plot for ", prefix, ": ", e$message))
      })
    }
    
    # PLS-DA
    if (length(unique(metadata$Group)) > 1) {
      plsda_path <- file.path(out_dir, paste0(project_name, "_PLSDA_Plot_", prefix, ".png")) # Added prefix to filename
      if(!file.exists(plsda_path)){
        tryCatch({
          plsda_res <- mixOmics::plsda(t(expression_matrix_var), metadata$Group, ncomp = 2)
          png(plsda_path, width=10, height=8, units="in", res=300)
          mixOmics::plotIndiv(plsda_res, group = metadata$Group, ind.names = TRUE, legend = TRUE, ellipse = TRUE, title = paste("PLS-DA Plot -", prefix))
          dev.off()
        }, error = function(e) send_progress(paste0("  WARNING: PLS-DA calculation failed for ", prefix, ": ", e$message)))
      }
    }
  }
}