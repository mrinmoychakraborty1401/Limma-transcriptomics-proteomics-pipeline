# ==============================================================================
# 03_run_meta_analysis.R
# Post-Hoc Analysis Pipeline for Pre-Computed Results
# ==============================================================================
# PROTOCOL: STRICT PRESERVATION MODE
# 1. DO NOT REMOVE EXISTING FEATURES for "cleanliness" or "minimalism".
# 2. IF EDITING: Only modify the specific logic requested.
# ==================================
# This script runs downstream analyses (Volcano, Enrichment, TF, PPI) on a 
# single file containing existing differential expression results.
# UPDATED: Compatible with standardized 01_analysis_functions.R
# UPDATED: Added Enhanced Visualizations (GSEA, NGEA, Interactive Volcano)
# UPDATED: Loads Survival Data to prioritize high HR genes in PPI
# UPDATED: Added KM Plotter / GEPIA2 Compatible List Generation
# ==============================================================================

if (!require("pacman", quietly = TRUE)) install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(
  tidyverse, ggrepel, writexl, readxl,
  clusterProfiler, org.Hs.eg.db, ReactomePA, DOSE, enrichplot,
  STRINGdb, igraph, RColorBrewer, tools, scales, purrr, ggraph,
  enrichR, jsonlite, argparse, plotly, htmlwidgets, grDevices, patchwork
)

# --- Define logging callback ---
send_progress <- function(msg) {
  cat(paste0("PROGRESS: ", msg, "\n"), file = stderr())
}

# --- GLOBAL SETTINGS ---
plot_font_size <- 12
volcano_labels_to_show <- 50 
fc_cutoff <- 1
p_cutoff <- 0.05

# --- PPI Analysis Settings ---
ppi_confidence_score <- 900
ppi_num_hubs_to_label <- 50 
min_cluster_size <- 5
string_cache_dir <- tools::R_user_dir("proteomics_analyzer_string_cache", which = "cache")
dir.create(string_cache_dir, showWarnings = FALSE, recursive = TRUE)

# --- Command Line Arguments ---
parser <- ArgumentParser()
parser$add_argument("--input", type="character", required=TRUE, help="Path to input CSV/Excel")
parser$add_argument("--output", type="character", required=TRUE, help="Output directory")
parser$add_argument("--project_name", type="character", required=TRUE)
parser$add_argument("--analysis_config", type="character", default=NULL)
parser$add_argument("--run_ppi", type="character", default="TRUE")
parser$add_argument("--run_venn", type="character", default="TRUE")
parser$add_argument("--run_signature", type="character", default="TRUE")
parser$add_argument("--scripts_dir", type="character", default=getwd())
parser$add_argument("--count_col", type="character", default=NULL)
parser$add_argument("--filter_op", type="character", default=">")
parser$add_argument("--filter_value", type="double", default=0)

# ADDED: New PPI Network Topology Arguments
parser$add_argument("--ppi_cluster", type="character", default="louvain")
parser$add_argument("--ppi_hub", type="character", default="pagerank")
parser$add_argument("--ppi_score", type="integer", default=900)

args <- parser$parse_args()
args$run_ppi <- as.logical(args$run_ppi)

# --- Load Helper Scripts ---
tryCatch({
  source(file.path(args$scripts_dir, "01_analysis_functions.R"))
  source(file.path(args$scripts_dir, "04_enhanced_visualizations.R"))
}, error = function(e) {
  tryCatch({
    source("01_analysis_functions.R")
    source("04_enhanced_visualizations.R")
  }, error = function(e2) {
    stop("Error: Analysis helper scripts (01_ or 04_) are missing.", call. = FALSE)
  })
})

# ==============================================================================
# 1. DATA LOADING & NORMALIZATION
# ==============================================================================
send_progress(paste("Reading input file:", args$input))

dir.create(args$output, showWarnings = FALSE, recursive = TRUE)

ext <- tolower(file_ext(args$input))
if (ext %in% c("xlsx", "xls")) {
  raw_data <- read_excel(args$input)
} else {
  raw_data <- read.csv(args$input, check.names = FALSE)
}
colnames(raw_data) <- make.names(colnames(raw_data))

# --- FILTERING MODULE ---
if (!is.null(args$count_col) && args$count_col != "") {
  safe_count_col <- args$count_col
  if(!safe_count_col %in% colnames(raw_data)) {
    safe_count_col <- make.names(args$count_col)
  }
  
  if (safe_count_col %in% colnames(raw_data)) {
    send_progress(paste0("Filtering data: ", args$count_col, " ", args$filter_op, " ", args$filter_value))
    val <- args$filter_value
    op <- args$filter_op
    col_vals <- suppressWarnings(as.numeric(as.character(raw_data[[safe_count_col]])))
    
    if(all(is.na(col_vals))) {
      send_progress("Warning: Filtering column is not numeric. Skipping filter.")
    } else {
      keep_rows <- tryCatch({
        switch(op, ">" = col_vals > val, ">=" = col_vals >= val, "<" = col_vals < val, "<=" = col_vals <= val, "==" = col_vals == val, "!=" = col_vals != val, rep(TRUE, nrow(raw_data))) 
      }, error = function(e) { rep(TRUE, nrow(raw_data)) })
      keep_rows[is.na(keep_rows)] <- FALSE 
      raw_data <- raw_data[keep_rows, , drop = FALSE]
      if (nrow(raw_data) == 0) stop("Error: Filtering removed all rows.", call. = FALSE)
    }
  }
}

# Column Mapping
col_map <- c(
  "GeneSymbol" = "GeneSymbol", "Symbol" = "GeneSymbol", "gene_name" = "GeneSymbol",
  "logFC" = "logFC", "LogFC" = "logFC", "log2FoldChange" = "logFC",
  "adj.P.Val" = "adj.P.Val", "padj" = "adj.P.Val", "FDR" = "adj.P.Val", "P.Value" = "P.Value"
)

for (pattern in names(col_map)) {
  if (pattern %in% colnames(raw_data) && !(col_map[pattern] %in% colnames(raw_data))) {
    colnames(raw_data)[colnames(raw_data) == pattern] <- col_map[pattern]
  }
}

if (!"GeneSymbol" %in% colnames(raw_data)) {
  gene_candidates <- grep("gene|symbol", colnames(raw_data), ignore.case=TRUE, value=TRUE)
  if(length(gene_candidates) > 0) colnames(raw_data)[colnames(raw_data) == gene_candidates[1]] <- "GeneSymbol"
}
if (!"logFC" %in% colnames(raw_data)) {
  fc_candidates <- grep("logfc|foldchange|log2fc", colnames(raw_data), ignore.case=TRUE, value=TRUE)
  if(length(fc_candidates) > 0) colnames(raw_data)[colnames(raw_data) == fc_candidates[1]] <- "logFC"
}

if (!all(c("GeneSymbol", "logFC") %in% colnames(raw_data))) {
  stop("Input file is missing required columns: GeneSymbol and logFC.", call. = FALSE)
}

if (!"adj.P.Val" %in% colnames(raw_data)) {
  if ("P.Value" %in% colnames(raw_data)) {
    raw_data$adj.P.Val <- p.adjust(raw_data$P.Value, method = "fdr")
  } else {
    raw_data$adj.P.Val <- 0.05
  }
}

# --- CLEAN GENE SYMBOLS ---
if (any(grepl("///|//", raw_data$GeneSymbol))) {
  send_progress("Detected multi-mapped gene symbols (e.g., 'GeneA /// GeneB'). Splitting and cleaning...")
  raw_data <- raw_data %>%
    separate_rows(GeneSymbol, sep = "\\s*///\\s*|\\s*//\\s*") %>%
    mutate(GeneSymbol = trimws(GeneSymbol)) %>%
    arrange(adj.P.Val, desc(abs(logFC))) %>% 
    distinct(GeneSymbol, .keep_all = TRUE)
}

raw_data$Significance <- "Not Significant"
raw_data$Significance[raw_data$adj.P.Val < p_cutoff & raw_data$logFC > fc_cutoff] <- "Upregulated"
raw_data$Significance[raw_data$adj.P.Val < p_cutoff & raw_data$logFC < -fc_cutoff] <- "Downregulated"

# ==============================================================================
# 2. VISUALIZATION MODULE
# ==============================================================================
send_progress("Generating Visualizations...")
vis_dir <- file.path(args$output, "1_Visualizations")
dir.create(vis_dir, showWarnings = FALSE)

# A. Static Volcano (Density-Aware Labeling)
raw_data$SigScore <- -log10(raw_data$adj.P.Val) * abs(raw_data$logFC)
label_candidates <- raw_data$GeneSymbol[raw_data$Significance != "Not Significant" & (rank(-raw_data$SigScore) <= 50 | raw_data$adj.P.Val < 1e-5)]
raw_data$Label <- ifelse(raw_data$GeneSymbol %in% label_candidates, raw_data$GeneSymbol, NA)

p_vol <- ggplot(raw_data, aes(x=logFC, y=-log10(adj.P.Val), color=Significance, label=Label)) +
  geom_point(alpha=0.6, aes(size = -log10(adj.P.Val))) + 
  scale_size_continuous(range = c(1, 4), guide = "none") +
  scale_color_manual(values=c("Upregulated"="red", "Downregulated"="blue", "Not Significant"="gray")) +
  geom_vline(xintercept=c(-fc_cutoff, fc_cutoff), linetype="dashed") +
  geom_hline(yintercept=-log10(p_cutoff), linetype="dashed") +
  ggrepel::geom_text_repel(max.overlaps = 20, size = 3.5, box.padding = 0.5, point.padding = 0.2, force = 2, na.rm = TRUE) +
  theme_minimal(base_size = 14) +
  labs(title=paste("Volcano Plot:", args$project_name), x="Log2 Fold Change", y="-Log10 FDR")

ggsave(file.path(vis_dir, "Volcano_Plot_Static.png"), p_vol, width=10, height=8)

# B. Interactive Volcano (Enhanced)
# Calling robust function from 04_enhanced_visualizations.R
tryCatch({
  generate_interactive_volcano(raw_data, args$project_name, fc_cutoff, p_cutoff, vis_dir)
}, error = function(e) send_progress(paste("Interactive Volcano failed:", e$message)))

# C. Top Genes Bar Plot
top_up <- raw_data %>% filter(Significance == "Upregulated") %>% arrange(desc(logFC)) %>% head(20)
top_down <- raw_data %>% filter(Significance == "Downregulated") %>% arrange(logFC) %>% head(20)
top_combined <- bind_rows(top_up, top_down)

if(nrow(top_combined) > 0) {
  p_bar <- ggplot(top_combined, aes(x=reorder(GeneSymbol, logFC), y=logFC, fill=logFC > 0)) +
    geom_bar(stat="identity") + coord_flip() +
    scale_fill_manual(values=c("TRUE"="red", "FALSE"="blue"), labels=c("Downregulated", "Upregulated"), name="Direction") +
    theme_minimal() + labs(title="Top 20 Up & Downregulated Genes", x="Gene", y="Log2 Fold Change")
  ggsave(file.path(vis_dir, "Top_Express_Genes_Barplot.png"), p_bar, width=8, height=10)
}

# D. Heatmap
top_50_sig <- raw_data %>% arrange(adj.P.Val) %>% head(50)
if(nrow(top_50_sig) > 0) {
  p_heat <- ggplot(top_50_sig, aes(x="LogFC", y=reorder(GeneSymbol, logFC), fill=logFC)) +
    geom_tile(color="white") + scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
    theme_minimal() + labs(title="Heatmap of Top 50 Significant Genes", x="", y="Gene") + theme(axis.text.x = element_blank())
  ggsave(file.path(vis_dir, "Top_50_Genes_LogFC_Heatmap.png"), p_heat, width=5, height=12)
}

# E. Donut Plot
tryCatch({
  sig_counts <- table(raw_data$Significance)
  sig_df <- as.data.frame(sig_counts)
  colnames(sig_df) <- c("Category", "Count")
  if(nrow(sig_df) > 0) {
    sig_df$Fraction <- sig_df$Count / sum(sig_df$Count)
    sig_df$ymax <- cumsum(sig_df$Fraction)
    sig_df$ymin <- c(0, head(sig_df$ymax, n=-1))
    sig_df$labelPosition <- (sig_df$ymax + sig_df$ymin) / 2
    sig_df$label <- paste0(sig_df$Category, "\n", sig_df$Count, "\n(", scales::percent(sig_df$Fraction), ")")
    
    p_donut <- ggplot(sig_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
      geom_rect() + coord_polar(theta="y") + xlim(c(2, 4)) +
      scale_fill_manual(values=c("Upregulated"="red", "Downregulated"="blue", "Not Significant"="gray")) +
      theme_void() + geom_text(aes(x=3.5, y=labelPosition, label=label), size=3.5, fontface="bold") +
      labs(title = paste("Distribution of DEGs -", args$project_name))
    ggsave(file.path(vis_dir, "Significance_Donut_Plot.png"), p_donut, width=8, height=8)
  }
}, error = function(e) send_progress(paste("Donut Plot Error:", e$message)))

# ==============================================================================
# 3. ENRICHMENT ANALYSIS
# ==============================================================================
enrich_dir <- file.path(args$output, "2_Enrichment_Analysis")
dir.create(enrich_dir, showWarnings = FALSE)

# --- ADDED: GSEA / NGEA (Enhanced) ---
tryCatch({
  send_progress(paste("Running Enhanced Visualizations (GSEA/NGEA) for", args$project_name, "..."))
  
  # Prepare vector: logFC named by GeneSymbol
  gsea_input_vec <- setNames(raw_data$logFC, raw_data$GeneSymbol)
  gsea_input_vec <- sort(gsea_input_vec[!is.na(gsea_input_vec)], decreasing = TRUE)
  
  if(length(gsea_input_vec) > 10) {
    run_gsea_analysis(gsea_input_vec, args$project_name, enrich_dir)
  } else {
    send_progress("  Skipping GSEA: Not enough genes (< 10) with valid logFC.")
  }
}, error = function(e) send_progress(paste("Warning: Enhanced visualization failed:", e$message)))
# -------------------------------------

up_genes <- raw_data %>% filter(Significance == "Upregulated") %>% pull(GeneSymbol)
down_genes <- raw_data %>% filter(Significance == "Downregulated") %>% pull(GeneSymbol)
combined_genes <- unique(c(up_genes, down_genes))
gene_list_fc <- setNames(raw_data$logFC, raw_data$GeneSymbol)

if (length(up_genes) > 0) run_advanced_enrichment_core(up_genes, gene_list_fc, "Upregulated", file.path(enrich_dir, "Upregulated"))
if (length(down_genes) > 0) run_advanced_enrichment_core(down_genes, gene_list_fc, "Downregulated", file.path(enrich_dir, "Downregulated"))
if (length(combined_genes) > 0) run_advanced_enrichment_core(combined_genes, gene_list_fc, "Combined", file.path(enrich_dir, "Combined"))

# --- Compare Cluster Analysis (Up vs Down) ---
if(length(up_genes) >= 5 && length(down_genes) >= 5) {
  send_progress("Running Compare Cluster Analysis (Up vs Down)...")
  gene_list_list <- list(Upregulated = up_genes, Downregulated = down_genes)
  entrez_list <- lapply(gene_list_list, function(x) {
    tryCatch(bitr(x, fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db)$ENTREZID, error=function(e) NULL)
  })
  entrez_list <- entrez_list[sapply(entrez_list, length) > 2] 
  
  if(length(entrez_list) >= 1) {
    tryCatch({
      ck_go <- compareCluster(geneCluster = entrez_list, fun = "enrichGO", OrgDb = org.Hs.eg.db, ont="BP")
      if(!is.null(ck_go) && nrow(as.data.frame(ck_go)) > 0) {
        ck_go <- setReadable(ck_go, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
        p_ck_go <- dotplot(ck_go, title = "GO Enrichment Comparison: Up vs Down")
        ggsave(file.path(enrich_dir, "CompareCluster_GO_Dotplot.png"), p_ck_go, width=10, height=8)
      }
    }, error = function(e) send_progress(paste("CompareCluster GO failed:", e$message)))
    
    tryCatch({
      ck_kegg <- compareCluster(geneCluster = entrez_list, fun = "enrichKEGG", organism="hsa")
      if(!is.null(ck_kegg) && nrow(as.data.frame(ck_kegg)) > 0) {
        ck_kegg <- tryCatch(setReadable(ck_kegg, OrgDb = org.Hs.eg.db, keyType="ENTREZID"), error=function(e) ck_kegg)
        p_ck_kegg <- dotplot(ck_kegg, title = "KEGG Enrichment Comparison: Up vs Down")
        ggsave(file.path(enrich_dir, "CompareCluster_KEGG_Dotplot.png"), p_ck_kegg, width=10, height=8)
      }
    }, error = function(e) send_progress(paste("CompareCluster KEGG failed:", e$message)))
  }
}

# ==============================================================================
# 4. PPI ANALYSIS (WITH SURVIVAL RESCUE)
# ==============================================================================
if(args$run_ppi) {
  send_progress("Running PPI Analysis...")
  ppi_dir <- file.path(args$output, "3_PPI_Network_Analysis")
  dir.create(ppi_dir, showWarnings = FALSE)
  
  # --- LOAD SURVIVAL GENES FOR RESCUE ---
  priority_genes <- NULL
  hr_file_path <- "./data/extracted_hr_data.csv"
  
  if (file.exists(hr_file_path)) {
    send_progress(paste("Loading Survival Data from:", hr_file_path))
    tryCatch({
      hr_data <- read.csv(hr_file_path)
      # Assume columns: 'Gene', 'HR', 'P_Value' (adjust if needed based on your file)
      # If columns differ, standard cleaning might be needed
      
      # Filter for significant survival genes (P < 0.05 AND HR > 1.7)
      if ("Gene" %in% colnames(hr_data) && "P_Value" %in% colnames(hr_data) && "HR" %in% colnames(hr_data)) {
        priority_genes <- hr_data %>% 
          filter(P_Value < 0.05 & HR > 1.7) %>% 
          pull(Gene)
        
        send_progress(paste0("  Found ", length(priority_genes), " significant survival genes (P<0.05, HR>1.7) to prioritize."))
      } else {
        send_progress("  Warning: Survival file found but 'Gene', 'P_Value', or 'HR' columns missing.")
      }
    }, error = function(e) {
      send_progress(paste("  Error reading survival file:", e$message))
    })
  } else {
    send_progress("  Note: No survival data found at specified path. Proceeding without priority rescue.")
  }
  
  if(length(combined_genes) > 5) {
    tryCatch({
      # Pass priority_genes AND new topology params to the updated function
      run_ppi_analysis(
        gene_list = combined_genes, 
        gene_list_fc = gene_list_fc, 
        project_name = args$project_name, 
        output_dir = ppi_dir, 
        plot_font_size = plot_font_size, 
        priority_genes = priority_genes,
        score_threshold = args$ppi_score,
        cluster_alg = args$ppi_cluster,
        hub_alg = args$ppi_hub
      )
    }, error = function(e) send_progress(paste("PPI Analysis failed:", e$message)))
  }
}

# ==============================================================================
# 5. EXPORT COMPATIBLE GENE LIST (KM Plotter / GEPIA2)
# ==============================================================================
send_progress("Generating KM Plotter / GEPIA2 compatible gene list...")

tryCatch({
  # 1. Filter Significant Genes
  sig_genes_df <- raw_data %>%
    filter(Significance %in% c("Upregulated", "Downregulated")) %>%
    select(GeneSymbol, logFC, adj.P.Val, Significance) %>%
    arrange(adj.P.Val)
  
  if (nrow(sig_genes_df) > 0) {
    
    # 2. Map to Entrez ID then back to Official Symbol
    # This standardizes aliases (e.g., 'OCT4' -> 'POU5F1') which is usually required for tools
    mapped_genes <- bitr(sig_genes_df$GeneSymbol, fromType = "SYMBOL", toType = c("ENTREZID"), OrgDb = org.Hs.eg.db)
    
    # If mapping was successful for some genes
    if (nrow(mapped_genes) > 0) {
      # Map Entrez back to Official Symbol to get the canonical name
      official_symbols <- bitr(mapped_genes$ENTREZID, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = org.Hs.eg.db)
      
      # Join everything together
      # Input_Symbol -> Entrez -> Official_Symbol
      final_map <- mapped_genes %>%
        inner_join(official_symbols, by = "ENTREZID") %>%
        distinct(SYMBOL.x, .keep_all = TRUE) # Keep one mapping per input gene
      
      # Merge back with original data
      output_df <- sig_genes_df %>%
        left_join(final_map, by = c("GeneSymbol" = "SYMBOL.x")) %>%
        rename(Input_Gene = GeneSymbol, Official_Symbol = SYMBOL.y, Entrez_ID = ENTREZID) %>%
        mutate(
          # Use Official Symbol if found, otherwise keep Input
          Compatible_Symbol = ifelse(!is.na(Official_Symbol), Official_Symbol, Input_Gene),
          # Create a clean string for copy-pasting
          For_Copy_Paste = Compatible_Symbol
        ) %>%
        select(Input_Gene, Compatible_Symbol, Entrez_ID, logFC, adj.P.Val, Significance)
      
      # Save to Excel
      km_out_file <- file.path(args$output, paste0("KM_GEPIA2_Compatible_Genes_", args$project_name, ".xlsx"))
      write_xlsx(output_df, km_out_file)
      send_progress(paste("Saved KM Plotter/GEPIA2 compatible list to:", km_out_file))
      
    } else {
      send_progress("Warning: Could not map any genes to Entrez IDs. Saving original list.")
      write_xlsx(sig_genes_df, file.path(args$output, paste0("KM_GEPIA2_Raw_Genes_", args$project_name, ".xlsx")))
    }
  } else {
    send_progress("No significant genes found to export for KM Plotter.")
  }
  
}, error = function(e) {
  send_progress(paste("Error generating KM Plotter list:", e$message))
})


send_progress("Analysis Complete.")