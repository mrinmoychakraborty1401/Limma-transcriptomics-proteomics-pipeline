# =======================================================
# Post-Hoc Analysis Pipeline for Pre-Computed Results
# =======================================================
# This script is designed to run downstream analyses (volcano, enrichment, PPI, etc.)
# on a file that already contains differential expression results (e.g., from a
# meta-analysis or another platform like microarray).

# --- Load Required Libraries ---
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggrepel, writexl,
  clusterProfiler, org.Hs.eg.db, ReactomePA, DOSE, enrichplot,
  STRINGdb, igraph, RColorBrewer, tools, scales, purrr, ggraph,
  enrichR, jsonlite, argparse
)

# === Load Analysis Functions ===
tryCatch({
  source("01_analysis_functions.R")
  source("04_enhanced_visualizations.R") # ADDED: Load enhanced visualization module
}, error = function(e) {
  stop("Error: The '01_analysis_functions.R' script is missing.", call. = FALSE)
})

# === Script Settings ===
plot_font_size <- 12
volcano_labels_to_show <- 20
fc_cutoff <- 1

# --- PPI Analysis Settings ---
ppi_confidence_score <- 900
ppi_num_hubs_to_label <- 25
min_cluster_size <- 5

# --- Function to send progress updates ---
send_progress <- function(...) {
  message(paste0("PROGRESS:", ...))
}

# --- Command Line Arguments ---
parser <- ArgumentParser()
parser$add_argument("--input", type="character", required=TRUE)
parser$add_argument("--output", type="character", required=TRUE)
parser$add_argument("--project_name", type="character", required=TRUE)
parser$add_argument("--analysis_config", type="character", required=TRUE)
parser$add_argument("--run_ppi", type="logical", required=TRUE)
parser$add_argument("--run_venn", type="logical", required=TRUE)
parser$add_argument("--run_signature", type="logical", required=TRUE)
args <- parser$parse_args()

# --- Read and Parse Config ---
analysis_config <- jsonlite::fromJSON(args$analysis_config)
meta_contrasts <- analysis_config$metaContrasts
venn_contrasts_to_make <- analysis_config$vennContrasts
signature_definitions <- analysis_config$signatureConfig

string_cache_dir <- tools::R_user_dir("proteomics_analyzer_string_cache", which = "cache")
dir.create(string_cache_dir, showWarnings = FALSE, recursive = TRUE)

# --- Read Input Data ---
send_progress("Reading pre-computed input file...")
full_data <- read.csv(args$input, stringsAsFactors = FALSE, check.names = TRUE)

# --- Main Processing Loop ---
all_dge_results <- list()

send_progress("Processing defined contrasts from input file...")
for (i in 1:nrow(meta_contrasts)) {
  contrast_info <- meta_contrasts[i, ]
  contrast_name <- contrast_info$name
  
  send_progress(paste("  Processing contrast:", contrast_name))
  
  contrast_dir_name <- paste0("Results_", gsub("[^A-Za-z0-9_]", "_", contrast_name))
  contrast_dir <- file.path(args$output, contrast_dir_name)
  dge_dir <- file.path(contrast_dir, "1_DGE_and_Plots")
  dir.create(dge_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Prepare the 'top_table' structure from the input columns
  top_table <- full_data %>%
    select(
      GeneSymbol = all_of(contrast_info$geneCol),
      logFC = all_of(contrast_info$logFCCol),
      adj.P.Val = all_of(contrast_info$pValCol)
    ) %>%
    filter(!is.na(GeneSymbol) & GeneSymbol != "") %>%
    distinct(GeneSymbol, .keep_all = TRUE) # Ensure unique gene symbols
  
  top_table$Significance <- "NS"
  top_table$Significance[top_table$adj.P.Val < 0.05 & top_table$logFC > fc_cutoff] <- "Up"
  top_table$Significance[top_table$adj.P.Val < 0.05 & top_table$logFC < -fc_cutoff] <- "Down"
  
  write.csv(top_table, file.path(dge_dir, paste0(args$project_name, "_DGE_Results_", contrast_name, ".csv")), row.names = FALSE)
  
  # --- Generate Volcano Plot ---
  send_progress("    Generating Volcano plot...")
  top_table$Label <- NA
  top_genes_for_label <- top_table %>%
    filter(Significance != "NS") %>%
    arrange(adj.P.Val, desc(abs(logFC))) %>%
    head(volcano_labels_to_show) %>%
    pull(GeneSymbol)
  top_table$Label[top_table$GeneSymbol %in% top_genes_for_label] <- top_table$GeneSymbol[top_table$GeneSymbol %in% top_genes_for_label]
  
  volcano_plot <- ggplot(top_table, aes(x=logFC, y=-log10(adj.P.Val), color=Significance, label=Label)) +
    geom_point(alpha=0.5) +
    geom_text_repel(max.overlaps = Inf, size = 3.5, na.rm = TRUE) +
    scale_color_manual(values=c("Up"="#ca0020", "Down"="#0571b0", "NS"="grey")) +
    labs(title=paste("Volcano Plot:", contrast_name), x="Log2 Fold Change", y="-Log10 Adjusted P-value") +
    theme_bw(base_size = plot_font_size) +
    geom_hline(yintercept = -log10(0.05), linetype="dashed") +
    geom_vline(xintercept = c(-fc_cutoff, fc_cutoff), linetype="dashed")
  
  ggsave(file.path(dge_dir, paste0(args$project_name, "_Volcano_Plot_", contrast_name, ".png")), plot=volcano_plot, width=10, height=8)
  
  # --- ADDED: Enhanced Visualizations for Meta-Analysis ---
  tryCatch({
    # 1. Interactive Volcano
    generate_interactive_volcano(top_table, contrast_name, fc_cutoff, 0.05, dge_dir)
    
    # 2. GSEA (Using logFC from meta-analysis)
    base_enrich_dir <- file.path(contrast_dir, "2_Enrichment_Analysis")
    run_gsea_analysis(setNames(top_table$logFC, top_table$GeneSymbol), contrast_name, base_enrich_dir)
    
  }, error = function(e) send_progress(paste("Warning: Enhanced visualization failed:", e$message)))
  # -----------------------------------------------------------
  
  all_dge_results[[contrast_name]] <- top_table
  
  # --- Run Enrichment Analysis ---
  base_enrich_dir <- file.path(contrast_dir, "2_Enrichment_Analysis")
  up_genes <- top_table %>% filter(Significance == "Up") %>% pull(GeneSymbol) %>% unique()
  down_genes <- top_table %>% filter(Significance == "Down") %>% pull(GeneSymbol) %>% unique()
  combined_genes <- c(up_genes, down_genes) %>% unique()
  gene_list_fc <- setNames(top_table$logFC, top_table$GeneSymbol)
  
  run_advanced_enrichment_core(up_genes, gene_list_fc, paste("Upregulated in", contrast_name), file.path(base_enrich_dir, "Upregulated"))
  run_advanced_enrichment_core(down_genes, gene_list_fc, paste("Downregulated in", contrast_name), file.path(base_enrich_dir, "Downregulated"))
}

# --- Step 6: PPI Analysis (Conditional) ---
if(args$run_ppi) {
  send_progress("--- 🛰️ Starting PPI Analysis Module ---")
  for (contrast in names(all_dge_results)) {
    all_sig_genes <- all_dge_results[[contrast]] %>% filter(Significance != "NS") %>% pull(GeneSymbol) %>% unique()
    if (length(all_sig_genes) >= min_cluster_size) {
      ppi_dir <- file.path(args$output, paste0("Results_", gsub("[^A-Za-z0-9_]", "_", contrast)), "3_PPI_Network_Analysis")
      dir.create(ppi_dir, showWarnings = FALSE, recursive = TRUE)
      gene_list_fc <- setNames(all_dge_results[[contrast]]$logFC, all_dge_results[[contrast]]$GeneSymbol)
      tryCatch({ run_ppi_analysis(all_sig_genes, gene_list_fc, contrast, ppi_dir, plot_font_size)
      }, error = function(e) { send_progress(paste0("❗️ ERROR: Failed to run PPI for ", contrast, ". Details: ", e$message)) })
    }
  }
}

# NOTE: Venn and Signature analysis are not implemented in this script yet.
# They would require adapting the `decideTests` logic to work on the list of data frames.

send_progress("--- ✅ Meta-analysis pipeline completed! ---")