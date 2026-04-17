# 04_enhanced_visualizations.R
# ==============================================================================
# PROTOCOL: STRICT PRESERVATION MODE
# 1. DO NOT REMOVE EXISTING FEATURES for "cleanliness" or "minimalism".
# 2. IF EDITING: Only modify the specific logic requested.
# 3. PRESERVE: All custom column parsing, MyGene caching, and metadata handling.
# ==============================================================================
# VISUALIZATION HELPER FUNCTIONS
# ==============================================================================
# Contains: generate_interactive_volcano, run_gsea_analysis, save_gsea_plots
# UPDATED:
# 1. Added ReactomePA and msigdbr to dependencies.
# 2. Added Reactome GSEA Logic.
# 3. Added Transcription Factor (TF) GSEA Logic using MSigDB C3:TFT.
# 4. Maintained Receipt/Checkpoint system for new analyses.
# 5. FIXED: Robust column name handling for GSEA (Symbol/GeneSymbol).
# 6. FIXED: Set selfcontained = FALSE for interactive widget to avoid Pandoc requirement.
# 7. FIXED: Correctly handles Named Numeric Vector input for GSEA.
# 8. ENHANCED: GSEA plots now split by Activation/Suppression with P-value coloring.
# 9. DISABLED: Ridgeplot generation disabled to prevent fatal "joint bandwidth" R crashes.

if (!require("pacman", quietly = TRUE)) install.packages("pacman", repos = "http://cran.us.r-project.org")
# ADDED: ReactomePA (for Reactome) and msigdbr (for TF targets)
pacman::p_load(ggplot2, plotly, htmlwidgets, clusterProfiler, enrichplot, org.Hs.eg.db, dplyr, stringr, ReactomePA, msigdbr, RColorBrewer)

# --- Interactive Volcano Plot ---
generate_interactive_volcano <- function(top_table, contrast_name, fc_cutoff, p_cutoff, output_dir) {
  # Ensure columns exist
  # Handle potential column name variations
  if("GeneSymbol" %in% colnames(top_table) && !"SYMBOL" %in% colnames(top_table)) {
    top_table$SYMBOL <- top_table$GeneSymbol
  }
  
  req_cols <- c("logFC", "adj.P.Val", "SYMBOL")
  if (!all(req_cols %in% colnames(top_table))) {
    # If SYMBOL is still missing, try to use rownames
    if(! "SYMBOL" %in% colnames(top_table)) {
      top_table$SYMBOL <- rownames(top_table)
    }
    
    # Check again
    if (!all(req_cols %in% colnames(top_table))) {
      warning(paste("Missing columns for volcano plot:", contrast_name))
      return(NULL)
    }
  }
  
  # Categorize genes
  top_table$DiffExpressed <- "NO"
  top_table$DiffExpressed[top_table$logFC > fc_cutoff & top_table$adj.P.Val < p_cutoff] <- "UP"
  top_table$DiffExpressed[top_table$logFC < -fc_cutoff & top_table$adj.P.Val < p_cutoff] <- "DOWN"
  
  # Create ggplot
  p <- ggplot(top_table, aes(x = logFC, y = -log10(adj.P.Val), 
                             text = paste("Symbol:", SYMBOL, 
                                          "<br>LogFC:", round(logFC, 2), 
                                          "<br>Adj.P:", formatC(adj.P.Val, format = "e", digits = 2)))) +
    geom_point(aes(color = DiffExpressed), alpha = 0.6, size = 1.5) +
    scale_color_manual(values = c("DOWN" = "blue", "NO" = "grey", "UP" = "red")) +
    theme_minimal() +
    labs(title = paste("Volcano Plot:", contrast_name), x = "Log2 Fold Change", y = "-Log10 Adj. P-Value") +
    geom_vline(xintercept = c(-fc_cutoff, fc_cutoff), linetype = "dashed", color = "black") +
    geom_hline(yintercept = -log10(p_cutoff), linetype = "dashed", color = "black")
  
  # Convert to Plotly
  p_interactive <- ggplotly(p, tooltip = "text")
  
  # Save
  # UPDATED: selfcontained = FALSE to avoid Pandoc error
  saveWidget(p_interactive, file = file.path(output_dir, paste0("Volcano_", contrast_name, ".html")), selfcontained = FALSE)
}

# --- Helper: Save GSEA Plots ---
save_gsea_plots <- function(gsea_result, type_name) {
  # 1. Enhanced Dotplot with Split Panels (Activated vs Suppressed)
  tryCatch({
    # The split=".sign" argument adds a .sign column (activated/suppressed) based on NES
    # facet_grid separates them visually
    p_dot <- dotplot(gsea_result, showCategory = 15, title = paste(type_name, "Enrichment"), split=".sign") + 
      facet_grid(.~.sign) +
      theme_bw(base_size = 12) +
      theme(strip.text = element_text(size=12, face="bold"))
    
    ggsave(file.path(base_enrich_dir, paste0("GSEA_", type_name, "_Dotplot_Split.png")), p_dot, width = 14, height = 10)
  }, error = function(e) {
    # Fallback if split fails (e.g. only one direction enriched)
    p_dot <- dotplot(gsea_result, showCategory = 20, title = paste(type_name, "Enrichment"))
    ggsave(file.path(base_enrich_dir, paste0("GSEA_", type_name, "_Dotplot.png")), p_dot, width = 10, height = 8)
  })
  
  # 2. Ridgeplot (Visualizes density of fold changes in enriched terms)
  # DISABLED: Known to cause uncatchable fatal R crashes during "bandwidth" calculation in certain datasets.
  # tryCatch({
  #   p_ridge <- ridgeplot(gsea_result, showCategory = 20) + labs(x = "NES", title = paste(type_name, "Ridgeplot"))
  #   ggsave(file.path(base_enrich_dir, paste0("GSEA_", type_name, "_Ridgeplot.png")), p_ridge, width = 10, height = 8)
  # }, error = function(e) { message("  Skipping Ridgeplot for ", type_name) })
  message("  Ridgeplot skipped (disabled for stability).")
  
  # 3. Enrichment Plot (Top Pathway) - Restored 'pvalue_table' and aesthetics
  if (nrow(gsea_result) > 0) {
    top_id <- gsea_result@result$ID[1]
    
    # Enhanced gseaplot2 with table and title
    p_gsea <- gseaplot2(gsea_result, geneSetID = 1, title = paste(type_name, ":", top_id), pvalue_table = TRUE)
    ggsave(file.path(base_enrich_dir, paste0("GSEA_", type_name, "_EnrichmentPlot_Top1.png")), p_gsea, width = 12, height = 8)
    
    # Optional: Plot multiple top pathways (Top 3) if available
    if(nrow(gsea_result) >= 3) {
      p_gsea_multi <- gseaplot2(gsea_result, geneSetID = 1:3, pvalue_table = TRUE)
      ggsave(file.path(base_enrich_dir, paste0("GSEA_", type_name, "_EnrichmentPlot_Top3_Combined.png")), p_gsea_multi, width = 14, height = 10)
    }
    
    # 4. Gene-Concept Network (Top 5 categories) with LogFC coloring
    tryCatch({
      p_cnet <- cnetplot(gsea_result, showCategory = 5, foldChange = gsea_vector, circular = FALSE, colorEdge = TRUE) +
        scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "LogFC")
      ggsave(file.path(base_enrich_dir, paste0("GSEA_", type_name, "_Cnetplot.png")), p_cnet, width = 14, height = 12)
    }, error = function(e) { message("  Skipping Cnetplot for ", type_name, ": ", e$message) })
    
    # 5. Heatplot (Alternative to Cnetplot)
    tryCatch({
      p_heat <- heatplot(gsea_result, foldChange = gsea_vector, showCategory = 10) + 
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "LogFC") +
        coord_fixed(ratio = 0.5)
      ggsave(file.path(base_enrich_dir, paste0("GSEA_", type_name, "_Heatplot.png")), p_heat, width = 16, height = 8)
    }, error = function(e) { message("  Skipping Heatplot for ", type_name) })
  }
}

# --- Main GSEA Analysis Function ---
run_gsea_analysis <- function(input_data, contrast_name, output_dir) {
  message("Starting GSEA Analysis for: ", contrast_name)
  
  # Prepare Directory
  base_enrich_dir <- file.path(output_dir, "Enrichment_GSEA")
  if (!dir.exists(base_enrich_dir)) dir.create(base_enrich_dir, recursive = TRUE)
  
  # --- STEP 1: NORMALIZE INPUT ---
  # Can accept either 'top_table' (data.frame) OR 'gsea_vector' (named numeric)
  top_table <- NULL
  
  if (is.vector(input_data) && is.numeric(input_data) && !is.data.frame(input_data)) {
    message("  Input is a named numeric vector. Converting to standard dataframe format...")
    # Ensure names exist
    if (is.null(names(input_data))) {
      message("  Skipping GSEA: Numeric vector has no gene names.")
      return(NULL)
    }
    top_table <- data.frame(SYMBOL = names(input_data), logFC = input_data, stringsAsFactors = FALSE)
  } else if (is.data.frame(input_data)) {
    top_table <- input_data
  } else {
    message("  Skipping GSEA: Input format not recognized (must be data.frame or named numeric vector).")
    return(NULL)
  }
  
  # --- STEP 2: ENSURE ID MAPPING (ENTREZID) ---
  if (!"ENTREZID" %in% colnames(top_table)) {
    
    # Normalize Symbol Column Name
    if ("GeneSymbol" %in% colnames(top_table)) {
      top_table$SYMBOL <- top_table$GeneSymbol
    } else if (!"SYMBOL" %in% colnames(top_table)) {
      # If rownames are symbols (common in limma output)
      if(!is.null(rownames(top_table)) && !grepl("^\\d+$", rownames(top_table)[1])) {
        top_table$SYMBOL <- rownames(top_table)
      }
    }
    
    # Check if we have a valid SYMBOL column now
    if ("SYMBOL" %in% colnames(top_table)) {
      # Remove invalid symbols/NAs
      top_table <- top_table[!is.na(top_table$SYMBOL) & top_table$SYMBOL != "", ]
      
      message(paste0("  Mapping ", nrow(top_table), " gene symbols to Entrez IDs..."))
      
      ids <- tryCatch({
        bitr(top_table$SYMBOL, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
      }, error = function(e) {
        message("  bitr mapping failed: ", e$message)
        return(NULL)
      })
      
      if (!is.null(ids)) {
        # Merge while keeping all rows (some might not map)
        top_table <- merge(top_table, ids, by="SYMBOL", all.x=TRUE)
        # Remove unmapped
        top_table <- top_table[!is.na(top_table$ENTREZID),]
        message(paste0("  Successfully mapped ", nrow(top_table), " genes."))
      } else {
        message("  Skipping GSEA: Could not map any symbols to Entrez IDs.")
        return(NULL)
      }
    } else {
      message("  Skipping GSEA: No ENTREZID or SYMBOL/GeneSymbol column found.")
      return(NULL)
    }
  }
  
  # Ensure we have enough data after mapping
  if (nrow(top_table) < 10) {
    message("  Skipping GSEA: Not enough mapped genes (< 10) for analysis.")
    return(NULL)
  }
  
  # --- STEP 3: CREATE RANKED VECTOR ---
  # Create sorted named vector
  gene_list <- top_table$logFC
  names(gene_list) <- top_table$ENTREZID
  
  # Sort descending
  gene_list <- sort(gene_list, decreasing = TRUE)
  
  # Remove NAs and Duplicates
  gene_list <- gene_list[!is.na(names(gene_list))]
  gene_list <- gene_list[!duplicated(names(gene_list))]
  
  # Make available globally for helpers if needed, though passing is better. 
  # For save_gsea_plots/cnetplot we need it.
  gsea_vector <<- gene_list 
  base_enrich_dir <<- base_enrich_dir # Make global for helper
  
  # --- STEP 4: RUN ANALYSES ---
  
  # 1. GO GSEA
  receipt_go_success <- file.path(base_enrich_dir, ".SUCCESS_GSEA_GO")
  receipt_go_fail <- file.path(base_enrich_dir, ".NO_RESULTS_GSEA_GO")
  
  if (file.exists(receipt_go_success) || file.exists(receipt_go_fail)) {
    message("  SKIPPED: GSEA GO (already analyzed).")
  } else {
    message("  Running GO GSEA...")
    tryCatch({
      gse_go <- gseGO(geneList = gsea_vector, OrgDb = org.Hs.eg.db, ont = "BP", 
                      minGSSize = 10, maxGSSize = 500, pvalueCutoff = 1.0, verbose = FALSE)
      
      if (!is.null(gse_go) && nrow(gse_go) > 0) {
        gse_go_readable <- setReadable(gse_go, OrgDb = org.Hs.eg.db, keyType = "ENTREZID")
        write.csv(gse_go_readable@result, file.path(base_enrich_dir, paste0("GSEA_GO_Results_", contrast_name, ".csv")))
        save_gsea_plots(gse_go_readable, "GO")
        file.create(receipt_go_success)
      } else {
        message("  No GO GSEA results found (even with p=1.0).")
        file.create(receipt_go_fail)
      }
    }, error = function(e) { message("  GSEA GO failed: ", e$message) })
  }
  
  # 2. KEGG GSEA
  receipt_kegg_success <- file.path(base_enrich_dir, ".SUCCESS_GSEA_KEGG")
  receipt_kegg_fail <- file.path(base_enrich_dir, ".NO_RESULTS_GSEA_KEGG")
  
  if (file.exists(receipt_kegg_success) || file.exists(receipt_kegg_fail)) {
    message("  SKIPPED: GSEA KEGG (already analyzed).")
  } else {
    message("  Running KEGG GSEA...")
    tryCatch({
      gse_kegg <- gseKEGG(geneList = gsea_vector, organism = 'hsa', 
                          minGSSize = 10, pvalueCutoff = 1.0, verbose = FALSE)
      
      if (!is.null(gse_kegg) && nrow(gse_kegg) > 0) {
        gse_kegg_readable <- setReadable(gse_kegg, OrgDb = org.Hs.eg.db, keyType = "ENTREZID")
        write.csv(gse_kegg_readable@result, file.path(base_enrich_dir, paste0("GSEA_KEGG_Results_", contrast_name, ".csv")))
        save_gsea_plots(gse_kegg_readable, "KEGG")
        file.create(receipt_kegg_success)
      } else {
        message("  No KEGG GSEA results found.")
        file.create(receipt_kegg_fail)
      }
    }, error = function(e) { message("  GSEA KEGG failed: ", e$message) })
  }
  
  # 3. REACTOME GSEA (ADDED)
  receipt_reactome_success <- file.path(base_enrich_dir, ".SUCCESS_GSEA_REACTOME")
  receipt_reactome_fail <- file.path(base_enrich_dir, ".NO_RESULTS_GSEA_REACTOME")
  
  if (file.exists(receipt_reactome_success) || file.exists(receipt_reactome_fail)) {
    message("  SKIPPED: GSEA Reactome (already analyzed).")
  } else {
    message("  Running Reactome GSEA...")
    tryCatch({
      # Reactome uses gsePathway
      gse_reactome <- ReactomePA::gsePathway(geneList = gsea_vector, 
                                             organism = "human",
                                             pvalueCutoff = 1.0,
                                             verbose = FALSE)
      
      if (!is.null(gse_reactome) && nrow(gse_reactome) > 0) {
        # Reactome results are Entrez IDs, making them readable
        gse_reactome_readable <- setReadable(gse_reactome, OrgDb = org.Hs.eg.db, keyType = "ENTREZID")
        write.csv(gse_reactome_readable@result, file.path(base_enrich_dir, paste0("GSEA_Reactome_Results_", contrast_name, ".csv")))
        save_gsea_plots(gse_reactome_readable, "Reactome")
        file.create(receipt_reactome_success)
      } else {
        message("  No Reactome GSEA results found.")
        file.create(receipt_reactome_fail)
      }
    }, error = function(e) { message("  GSEA Reactome failed: ", e$message) })
  }
  
  # 4. TRANSCRIPTION FACTOR (TF) GSEA (ADDED)
  receipt_tf_success <- file.path(base_enrich_dir, ".SUCCESS_GSEA_TF")
  receipt_tf_fail <- file.path(base_enrich_dir, ".NO_RESULTS_GSEA_TF")
  
  if (file.exists(receipt_tf_success) || file.exists(receipt_tf_fail)) {
    message("  SKIPPED: GSEA TF (already analyzed).")
  } else {
    message("  Running Transcription Factor GSEA (MSigDB C3:TFT)...")
    tryCatch({
      # Fetch gene sets for Transcription Factors (C3 -> TFT:GTRD for high quality)
      tft_term2gene <- msigdbr::msigdbr(species = "Homo sapiens", category = "C3", subcategory = "TFT:GTRD") %>% 
        dplyr::select(gs_name, entrez_gene)
      
      # Run generic GSEA with TERM2GENE
      gse_tf <- GSEA(gsea_vector, 
                     TERM2GENE = tft_term2gene,
                     pvalueCutoff = 1.0, 
                     verbose = FALSE)
      
      if (!is.null(gse_tf) && nrow(gse_tf) > 0) {
        # Convert Entrez IDs to Symbols for readability
        gse_tf_readable <- setReadable(gse_tf, OrgDb = org.Hs.eg.db, keyType = "ENTREZID")
        write.csv(gse_tf_readable@result, file.path(base_enrich_dir, paste0("GSEA_TF_Results_", contrast_name, ".csv")))
        save_gsea_plots(gse_tf_readable, "TF_Targets")
        file.create(receipt_tf_success)
      } else {
        message("  No TF GSEA results found.")
        file.create(receipt_tf_fail)
      }
    }, error = function(e) { message("  GSEA TF failed: ", e$message) })
  }
}