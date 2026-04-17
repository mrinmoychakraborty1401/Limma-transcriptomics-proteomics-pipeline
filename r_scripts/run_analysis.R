# ==================================
# Advanced Unified Analysis Pipeline
# ==================================
# PROTOCOL: STRICT PRESERVATION MODE
# 1. DO NOT REMOVE EXISTING FEATURES for "cleanliness" or "minimalism".
# 2. IF EDITING: Only modify the specific logic requested.
# ==================================
# UPDATES:
# 1. Calls standardized enrichment functions (including TFs).
# 2. Volcano plot now uses density-aware labeling.
# 3. Retains full correction pipeline logic.
# 4. Added 'patchwork' for composite PPI visualization.
# 5. FIXED: Robust metadata sample matching (spaces/dots/hyphens).
# 6. FIXED: Added row.names=NULL to read.csv to prevent "duplicate row.names" errors.
# 7. ADDED: Enhanced Visualizations (Interactive Volcano + GSEA/NGEA) restored to main pipeline.
# 8. ADDED: Dynamic parameter acceptance (P-value, FC, PPI Score) from UI.
# 9. FIXED: Moved Argument Parsing to top to prevent "scripts_dir_arg not found" error.
# 10. REORDERED CORRECTION: RUV -> LOESS -> ComBat.
# 11. ADDED: Global Random Forest Analysis.
# 12. ADDED: PPI Cluster and Hub settings.
# 13. ADDED: Reinstated Compare Cluster Analysis (Up vs Down) for raw data pipeline.
# 14. UPDATED: Renamed LOESS log output to "Drift Correction (Injection Order Regression)".
# 15. REPLACED: Median Normalization with Quantile Normalization as Step 0.
# 16. ADDED: Mahalanobis Distance Outlier Detection (Global & Group-Centric) WITH dedicated PCA plotting.
# 17. FIXED: Bulletproofed PCA calculation in Outlier Detection against NA/Zero-Variance crashes.
# 18. ADDED: Advanced Contrast Parsing via JSON to support arbitrary Sample vs Sample comparisons (Localized Limma).
# 19. ADDED: Pairwise QC plotting for specific contrast sub-groups (Now Includes Pre-Log and Post-Log!).
# 20. ADDED: Log2FC-based DEG hierarchical clustering Heatmaps.
# 21. ADDED: Automated KEGG Mapper Input Generation inside the main DGE loop.

# --- GLOBAL WARNING SUPPRESSION ---
# Prevent non-fatal R warnings (like package build versions) from being 
# sent to stderr and crashing the strict Electron backend.
options(warn = -1)

# --- Load Required Libraries ---
if (!require("pacman", quietly = TRUE)) install.packages("pacman", repos = "http://cran.us.r-project.org")

suppressPackageStartupMessages({
  pacman::p_load(
    tidyverse, limma, ggrepel, pheatmap, impute, mixOmics, writexl,
    clusterProfiler, org.Hs.eg.db, ReactomePA, DOSE, enrichplot,
    STRINGdb, igraph, RColorBrewer, sva, tools, scales, purrr, ggraph,
    enrichR, jsonlite, matrixStats, argparse, mygene, patchwork, randomForest
  )
})

# --- Command Line Arguments (Moved to Top) ---
parser <- ArgumentParser()
parser$add_argument("--input", type="character", required=TRUE)
parser$add_argument("--metadata", type="character", required=TRUE)
parser$add_argument("--output", type="character", required=TRUE)
parser$add_argument("--project_name", type="character", required=TRUE)
parser$add_argument("--analysis_config", type="character", required=TRUE)
parser$add_argument("--scripts_dir", type="character", required=TRUE)
parser$add_argument("--run_ppi", type="character", default="FALSE")
parser$add_argument("--run_venn", type="character", default="FALSE")
parser$add_argument("--run_signature", type="character", default="FALSE")
parser$add_argument("--run_median", type="character", default="FALSE") # Used for Quantile Norm now
parser$add_argument("--run_loess", type="character", default="FALSE")
parser$add_argument("--run_combat", type="character", default="FALSE")
parser$add_argument("--run_ruv", type="character", default="FALSE")
parser$add_argument("--split_batches", type="character", default="FALSE")
# Significance parameters
parser$add_argument("--p_cutoff", type="character", default="0.05")
parser$add_argument("--fc_cutoff", type="character", default="1.0")
parser$add_argument("--ppi_score", type="character", default="900")
parser$add_argument("--outlier_method", type="character", default="none")
# PPI Settings
parser$add_argument("--ppi_cluster", type="character", default="louvain")
parser$add_argument("--ppi_hub", type="character", default="pagerank")
# NEW VISUALIZATION & CONTRAST ARGS
parser$add_argument("--contrasts_json", type="character", default="")
parser$add_argument("--run_pairwise_qc", type="character", default="FALSE")
parser$add_argument("--run_deg_heatmaps", type="character", default="FALSE")

args <- parser$parse_args()

# Extract scripts directory for sourcing
scripts_dir_arg <- args$scripts_dir

# Boolean conversion
run_ppi <- as.logical(args$run_ppi)
run_venn <- as.logical(args$run_venn)
run_signature <- as.logical(args$run_signature)
run_median <- as.logical(args$run_median) # Flag for Quantile Norm
run_loess <- as.logical(args$run_loess)
run_combat <- as.logical(args$run_combat)
run_ruv <- as.logical(args$run_ruv)
split_batches <- as.logical(args$split_batches)
run_pairwise_qc <- as.logical(args$run_pairwise_qc)
run_deg_heatmaps <- as.logical(args$run_deg_heatmaps)

# Numeric Parameter Conversion
p_cutoff <- as.numeric(args$p_cutoff)
fc_cutoff <- as.numeric(args$fc_cutoff)
ppi_confidence_score <- as.numeric(args$ppi_score)
ppi_cluster_algo <- args$ppi_cluster
ppi_hub_measure <- args$ppi_hub

# === User-Defined Settings (Defaults - overwritten by args) ===
plot_font_size <- 12
ppi_num_hubs_to_label <- 25
min_cluster_size <- 5

# --- Function to send progress updates ---
send_progress <- function(...) {
  message(paste0("PROGRESS:", ...))
}

# === Load Analysis Functions ===
tryCatch({
  source(file.path(scripts_dir_arg, "01_analysis_functions.R"))
  source(file.path(scripts_dir_arg, "04_enhanced_visualizations.R")) 
}, error = function(e) {
  tryCatch({
    source("01_analysis_functions.R")
    source("04_enhanced_visualizations.R")
  }, error = function(e2) {
    stop("Error: Analysis helper scripts are missing.", call. = FALSE)
  })
})

# --- Helper Function to Save Plots ---
save_plot_pub <- function(plot, filename_base, outdir, width=10, height=8, dpi=300) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  png_path <- file.path(outdir, paste0(filename_base, ".png"))
  if(file.exists(png_path)) return()
  tryCatch({
    ggplot2::ggsave(png_path, plot=plot, width=width, height=height, dpi=dpi)
  }, error = function(e) {
    send_progress(paste("Warning: Failed to save plot", filename_base, ":", e$message))
  })
}

# --- Helper: Shorten Sample Names ---
get_short_names <- function(names_vec) {
  gsm <- stringr::str_extract(names_vec, "GSM\\d+")
  fallback <- sapply(names_vec, function(x) {
    parts <- unlist(strsplit(x, "_"))
    return(tail(parts, 1))
  })
  final_names <- ifelse(!is.na(gsm), gsm, fallback)
  if(any(duplicated(final_names))) {
    dupes <- duplicated(final_names) | duplicated(final_names, fromLast=TRUE)
    final_names[dupes] <- names_vec[dupes]
  }
  return(final_names)
}

# --- Wrapper for Enrichment ---
run_advanced_enrichment <- function(genes, gene_list_fc, direction, contrast, base_enrich_dir) {
  enrich_dir <- file.path(base_enrich_dir, direction)
  run_advanced_enrichment_core( genes = genes, gene_list_fc = gene_list_fc, title_prefix = paste(direction, "in", contrast), base_enrich_dir = enrich_dir )
}

# Load Config
analysis_config <- jsonlite::fromJSON(args$analysis_config)
legacy_contrasts_to_make <- analysis_config$contrasts
venn_contrasts_to_make <- analysis_config$vennContrasts
signature_definitions <- analysis_config$signatureConfig

if (is.matrix(venn_contrasts_to_make)) {
  venn_contrasts_to_make <- split(venn_contrasts_to_make, row(venn_contrasts_to_make))
  names(venn_contrasts_to_make) <- NULL
}

string_cache_dir <- tools::R_user_dir("proteomics_analyzer_string_cache", which = "cache")
dir.create(string_cache_dir, showWarnings = FALSE, recursive = TRUE)

send_progress("--- Starting Full Analysis Pipeline ---")
send_progress(paste("Parameters: P-Val <", p_cutoff, "| Log2FC >", fc_cutoff, "| PPI Conf >", ppi_confidence_score))

# --- Step 1: Read, Prepare, and Impute Data ---
send_progress("Reading and preparing data...")
# FIX: Added row.names = NULL to prevent auto-detection of row names if header is malformed
df <- read.csv(args$input, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL)
metadata_df <- read.csv(args$metadata, stringsAsFactors = FALSE, row.names = NULL)
id_col <- "GeneSymbol"

if (!id_col %in% colnames(df)) stop("Input file must contain a 'GeneSymbol' column.", call. = FALSE)
if (any(duplicated(df[[id_col]]))) { df <- df[!duplicated(df[[id_col]]), ] }

# --- Keyword Configuration ---
sample_config_path <- file.path(dirname(args$metadata), "sample_config.json")
annotation_keywords <- NULL
if (file.exists(sample_config_path)) {
  sample_config_data <- jsonlite::fromJSON(sample_config_path)
  annotation_keywords <- sample_config_data$annotationKeywords
}

annotation_map <- data.frame(GeneSymbol = df[[id_col]]) 
if (!is.null(annotation_keywords) && length(annotation_keywords) > 0) {
  send_progress(paste("Processing", length(annotation_keywords), "annotation keywords..."))
  unique_genes <- unique(df[[id_col]])
  fields_to_fetch <- c("name", "summary", "go.BP.term", "go.CC.term", "go.MF.term", "interpro.desc")
  
  metadata_res <- tryCatch({
    if(exists("queryMany", where = asNamespace("mygene"), mode = "function")) {
      queryMany(unique_genes, scopes="symbol", fields=fields_to_fetch, species="human")
    } else {
      mygene::queryMany(unique_genes, scopes="symbol", fields=fields_to_fetch, species="human")
    }
  }, error = function(e) { return(NULL) })
  
  if (!is.null(metadata_res)) {
    meta_df <- as.data.frame(metadata_res)
    check_keyword <- function(row_idx, keyword) {
      kw <- tolower(keyword)
      row_string <- tolower(paste(unlist(meta_df[row_idx, ]), collapse = " "))
      return(grepl(kw, row_string, fixed=TRUE))
    }
    
    for (kw in annotation_keywords) {
      col_name <- paste0("Is_", gsub("[^A-Za-z0-9]", "_", kw))
      matches <- sapply(1:nrow(meta_df), function(i) check_keyword(i, kw))
      lookup_df <- data.frame(GeneSymbol = meta_df$query, Match = matches) %>% dplyr::distinct(GeneSymbol, .keep_all = TRUE)
      annotation_map <- annotation_map %>% dplyr::left_join(lookup_df, by = "GeneSymbol") %>% dplyr::mutate(!!col_name := ifelse(is.na(Match), FALSE, Match)) %>% dplyr::select(-Match)
    }
  }
}

# --- ROBUST COLUMN ALIGNMENT (FIXED) ---
# Metadata Sample names might have spaces/hyphens, but DF columns might have dots (via make.names upstream)
# We prioritize exact match, then sanitized match.
cols_in_data <- colnames(df)
valid_cols <- c()

for(s in metadata_df$Sample) {
  if(s %in% cols_in_data) {
    # Exact match
    valid_cols <- c(valid_cols, s)
  } else {
    # Try sanitized (replace hyphens/spaces with dots)
    s_clean <- make.names(s)
    if(s_clean %in% cols_in_data) {
      # Update metadata to match file
      metadata_df$Sample[metadata_df$Sample == s] <- s_clean
      valid_cols <- c(valid_cols, s_clean)
    }
  }
}

metadata_df <- metadata_df %>% filter(Sample %in% valid_cols)
if (nrow(metadata_df) == 0) stop("Error: No metadata samples matched columns in the input file.")

final_cols_to_select <- metadata_df$Sample
missing_cols <- setdiff(final_cols_to_select, colnames(df))
if (length(missing_cols) > 0) stop(paste("Error: Missing sample columns:", paste(missing_cols, collapse=", ")))

expr_raw <- as.matrix(df[, final_cols_to_select])
rownames(expr_raw) <- df[[id_col]]

non_empty_rows <- apply(expr_raw, 1, function(row) !all(is.na(row)) && !all(row[!is.na(row)] == 0))
if (sum(non_empty_rows) == 0) stop("No proteins met the detection criteria.")
expr_filtered <- expr_raw[non_empty_rows, ]

# Imputation
send_progress("Imputing missing values within each batch...")
imputed_batches <- list()
batches <- unique(metadata_df$Batch) 
for (batch in batches) {
  batch_samples <- metadata_df$Sample[metadata_df$Batch == batch]
  batch_expr <- expr_filtered[, batch_samples, drop = FALSE]
  if (ncol(batch_expr) > 0) {
    na_counts <- colSums(is.na(batch_expr) | batch_expr == 0)
    na_percentages <- na_counts / nrow(batch_expr)
    
    # RELAXED THRESHOLD: Keep samples with up to 95% missingness to prevent dropping everything in sparse datasets
    samples_to_keep <- names(na_percentages[na_percentages <= 0.95])
    
    if (length(samples_to_keep) >= 2) {
      clean_batch_expr <- batch_expr[, samples_to_keep, drop = FALSE]
      
      tryCatch({
        imputed_batch_expr <- impute::impute.knn(clean_batch_expr)$data
        imputed_batches[[batch]] <- imputed_batch_expr
      }, error = function(e) {
        send_progress(paste("Warning: Imputation failed for batch", batch, ":", e$message, "- Using zeros for NAs."))
        clean_batch_expr[is.na(clean_batch_expr)] <- 0
        imputed_batches[[batch]] <- clean_batch_expr
      })
    } else {
      send_progress(paste("Warning: Batch", batch, "dropped because too many samples had excessive missing values."))
    }
  }
}
if (length(imputed_batches) == 0) stop("No batches could be imputed. Check data sparsity.")
expr_imputed <- do.call(cbind, imputed_batches)
metadata_filtered <- metadata_df[match(colnames(expr_imputed), metadata_df$Sample), ]

# --- QC and Correction Logic ---
qc_dir <- file.path(args$output, paste0(args$project_name, "_Quality_Control"))
dir.create(qc_dir, showWarnings = FALSE, recursive=TRUE)

# *** UPDATED CALL ***
.create_qc_plots(expr_imputed, metadata_filtered, "1_Post_Imputation_Pre_Log", qc_dir, args$project_name)

send_progress("Applying log2 transformation...")
min_val <- min(expr_imputed, na.rm=TRUE)
if (min_val < 0) {
  expr_log <- expr_imputed
} else {
  expr_log <- log2(expr_imputed + 1)
  expr_log[is.na(expr_log)] <- 0
}

# *** UPDATED CALL ***
.create_qc_plots(expr_log, metadata_filtered, "2_Post_Log_Pre_Correction", qc_dir, args$project_name)

current_expr <- expr_log
metadata_filtered$RunOrder <- as.numeric(metadata_filtered$RunOrder)

# --- REORDERED CORRECTION SEQUENCE: QUANTILE -> RUV -> LOESS -> COMBAT ---

# --- Step 0: Quantile Normalization (Loading Bias/Distribution Correction) ---
# Replaces 'Median Normalization' but uses same 'run_median' flag for compatibility
if (run_median) {
  send_progress("--- Normalization: Quantile Normalization (Distribution Alignment) ---")
  tryCatch({
    current_expr <- limma::normalizeQuantiles(current_expr)
    send_progress("Quantile Normalization applied.")
  }, error = function(e) send_progress(paste("Quantile Normalization Failed:", e$message)))
}

# 1. RUV (Now Step 1)
if (run_ruv) {
  send_progress("--- Correction Step 1/3: RUV-III (Replicates) ---")
  if ("ReplicateID" %in% colnames(metadata_filtered) && length(unique(metadata_filtered$ReplicateID)) < nrow(metadata_filtered)) {
    M <- model.matrix(~0 + factor(metadata_filtered$ReplicateID))
    colnames(M) <- gsub("factor\\(metadata_filtered\\$ReplicateID\\)", "", colnames(M))
    ruv_fit <- tryCatch({ ruv::RUVIII(Y = t(current_expr), M = M, ctl = rep(TRUE, nrow(current_expr)), k = 1) }, error = function(e) NULL)
    if (!is.null(ruv_fit)) { current_expr <- t(ruv_fit); send_progress("RUV correction applied.") }
  } else {
    send_progress("WARNING: RUV skipped. ReplicateID missing or no technical replicates found.")
  }
}

# 2. LOESS (Now Step 2 - Drift Correction)
if (run_loess) {
  send_progress("--- Correction Step 2/3: Drift Correction (Injection Order Regression - LOESS) ---")
  if (!"RunOrder" %in% colnames(metadata_filtered) || any(is.na(metadata_filtered$RunOrder))) {
    send_progress("WARNING: Drift Correction skipped. 'RunOrder' column missing or contains NAs.")
  } else {
    if (requireNamespace("proBatch", quietly = TRUE)) {
      formatted_meta <- metadata_filtered %>% mutate(sample_name = Sample, batch = Batch, order = RunOrder)
      corrected_matrix <- current_expr
      for(b in unique(formatted_meta$batch)){
        samps <- formatted_meta$sample_name[formatted_meta$batch == b]
        if(length(samps) > 4) { 
          sub_expr <- current_expr[, samps]
          corrected_sub <- t(apply(sub_expr, 1, function(y) {
            df <- data.frame(order = formatted_meta$order[formatted_meta$batch == b], int = y)
            fit <- loess(int ~ order, data=df, span=0.75)
            resid <- residuals(fit)
            return(mean(y, na.rm=TRUE) + resid)
          }))
          corrected_matrix[, samps] <- corrected_sub
        }
      }
      current_expr <- corrected_matrix
      send_progress("Drift Correction applied (proBatch).")
    } else {
      # Native fallback
      batches <- unique(metadata_filtered$Batch)
      corrected_matrix <- current_expr
      for(b in batches) {
        indices <- which(metadata_filtered$Batch == b)
        if(length(indices) >= 5) {
          sub_expr <- current_expr[, indices, drop=FALSE]
          orders <- metadata_filtered$RunOrder[indices]
          corrected_sub <- t(apply(sub_expr, 1, function(y) {
            df <- data.frame(val = y, ord = orders)
            df <- df[!is.na(df$val), ]
            if(nrow(df) >= 5) {
              fit <- tryCatch(loess(val ~ ord, data=df, span=0.75), error=function(e) NULL)
              if(!is.null(fit)) {
                trend <- predict(fit, newdata=data.frame(ord=orders))
                resid <- y - trend
                return(resid + median(y, na.rm=TRUE))
              }
            }
            return(y)
          }))
          corrected_matrix[, indices] <- corrected_sub
        }
      }
      current_expr <- corrected_matrix
      send_progress("Drift Correction applied (Native).")
    }
  }
}

# 3. ComBat (Now Step 3)
if (run_combat) {
  send_progress("--- Correction Step 3/3: ComBat (Batches) ---")
  if (length(unique(metadata_filtered$Batch)) > 1) {
    mod_combat <- if(identical(as.factor(metadata_filtered$Batch), as.factor(metadata_filtered$Group))) model.matrix(~1, data=metadata_filtered) else model.matrix(~as.factor(Group), data=metadata_filtered)
    
    tryCatch({
      combat_data <- as.matrix(current_expr)
      vars <- matrixStats::rowVars(combat_data)
      keep <- vars > 1e-6 & !is.na(vars)
      if(sum(!keep) > 0) combat_data <- combat_data[keep, , drop=FALSE]
      current_expr <- sva::ComBat(dat=combat_data, batch=as.factor(metadata_filtered$Batch), mod=mod_combat, par.prior=FALSE)
      send_progress("ComBat correction applied.")
    }, error = function(e) { send_progress(paste("WARNING: ComBat failed:", e$message)) })
  }
}

# --- NEW: Outlier Detection (Mahalanobis Distance) ---
if (!is.null(args$outlier_method) && args$outlier_method %in% c("mahalanobis", "mahalanobis_group")) {
  send_progress(paste0("--- Outlier Detection: ", ifelse(args$outlier_method == "mahalanobis_group", "Group-Centric", "Global"), " ---"))
  tryCatch({
    # BULLETPROOF PCA: Remove NAs and zero-variance genes to prevent prcomp crashes
    valid_genes_idx <- complete.cases(current_expr)
    expr_clean <- current_expr[valid_genes_idx, , drop = FALSE]
    row_variances <- apply(expr_clean, 1, var, na.rm = TRUE)
    keep_vars <- !is.na(row_variances) & row_variances > 1e-6
    expr_var <- expr_clean[keep_vars, , drop = FALSE]
    
    if (nrow(expr_var) < 2) {
      send_progress("  Skipping Outlier Detection: Not enough variable genes for PCA.")
    } else {
      pca_check <- prcomp(t(expr_var), scale. = TRUE)
      pca_scores <- pca_check$x[, 1:2] # Use first 2 PCs
      
      md_vals <- numeric(nrow(pca_scores))
      names(md_vals) <- rownames(pca_scores)
      potential_outliers <- c()
      misclassified <- c()
      
      if (args$outlier_method == "mahalanobis_group") {
        # Calculate GLOBAL covariance matrix to avoid singular matrices in small groups
        cov_mat <- cov(pca_scores)
        
        # Calculate distance to GROUP centroids
        for (s in rownames(pca_scores)) {
          grp <- metadata_filtered$Group[metadata_filtered$Sample == s]
          grp_samples <- metadata_filtered$Sample[metadata_filtered$Group == grp]
          grp_scores <- pca_scores[grp_samples, , drop=FALSE]
          grp_center <- colMeans(grp_scores)
          
          # Distance of sample 's' to its own group's centroid
          md_vals[s] <- mahalanobis(pca_scores[s, , drop=FALSE], grp_center, cov_mat)
        }
        
        # Threshold: Chi-square with 2 DF at p=0.05
        cutoff <- qchisq(0.95, df = 2) 
        potential_outliers <- names(md_vals[md_vals > cutoff])
        
        # Cross-Group Check: Is it closer to another group than its own?
        for (s in rownames(pca_scores)) {
          my_grp <- metadata_filtered$Group[metadata_filtered$Sample == s]
          my_dist <- md_vals[s]
          
          for (other_grp in unique(metadata_filtered$Group)) {
            if (other_grp != my_grp) {
              other_samples <- metadata_filtered$Sample[metadata_filtered$Group == other_grp]
              if (length(other_samples) > 0) {
                other_center <- colMeans(pca_scores[other_samples, , drop=FALSE])
                dist_to_other <- mahalanobis(pca_scores[s, , drop=FALSE], other_center, cov_mat)
                
                # If strictly closer to another group than its own, flag it!
                if (dist_to_other < my_dist) { 
                  misclassified <- c(misclassified, s)
                }
              }
            }
          }
        }
        potential_outliers <- unique(c(potential_outliers, misclassified))
        
      } else {
        # Global Mahalanobis
        center <- colMeans(pca_scores)
        cov_mat <- cov(pca_scores)
        # Re-assign md_vals to actual distances
        md_dist <- mahalanobis(pca_scores, center, cov_mat)
        for (s in names(md_dist)) md_vals[s] <- md_dist[s]
        
        # Threshold: Chi-square with 2 DF at p=0.01 (very strict)
        cutoff <- qchisq(0.99, df = 2) 
        potential_outliers <- names(md_vals[md_vals > cutoff])
      }
      
      samples_to_exclude <- c()
      if (length(potential_outliers) > 0) {
        # Check group sizes before excluding
        for (s in potential_outliers) {
          grp <- metadata_filtered$Group[metadata_filtered$Sample == s]
          count_in_grp <- sum(metadata_filtered$Group == grp)
          
          min_required <- ifelse(args$outlier_method == "mahalanobis_group", 2, 3)
          
          if (count_in_grp > min_required) {
            samples_to_exclude <- c(samples_to_exclude, s)
          } else {
            send_progress(paste0("  Skipping exclusion of outlier ", s, " (Group ", grp, " would drop below ", min_required + 1, " samples)."))
          }
        }
      }
      
      # --- PLOT: Generate Outlier Detection PCA Plot ---
      send_progress("  Generating Outlier Detection PCA Plot...")
      
      percent_var <- pca_check$sdev^2 / sum(pca_check$sdev^2)
      
      # Create plotting dataframe
      plot_df <- as.data.frame(pca_scores) %>%
        tibble::rownames_to_column("Sample") %>%
        dplyr::left_join(metadata_filtered, by = "Sample") %>%
        dplyr::mutate(
          Outlier_Status = dplyr::case_when(
            Sample %in% samples_to_exclude ~ "Excluded Outlier",
            Sample %in% potential_outliers ~ "Flagged (Spared)",
            TRUE ~ "Normal"
          )
        )
      
      outlier_pca_plot <- ggplot(plot_df, aes(x=PC1, y=PC2))
      
      # 1. Base points mimicking final PCA (colored by group, shaped by batch)
      if (length(unique(metadata_filtered$Batch)) > 1) {
        outlier_pca_plot <- outlier_pca_plot + geom_point(aes(color=Group, shape=Batch), size=5, alpha=0.8)
      } else {
        outlier_pca_plot <- outlier_pca_plot + geom_point(aes(color=Group), size=5, alpha=0.8)
      }
      
      # 2. Add group ellipses based on method
      if (args$outlier_method == "mahalanobis_group") {
        group_counts <- table(plot_df$Group)
        if (all(group_counts >= 3)) {
          outlier_pca_plot <- outlier_pca_plot + stat_ellipse(aes(group = Group, color = Group), type = "t", level = 0.95, linetype = 2, linewidth = 1, show.legend = FALSE)
        }
      } else {
        # Global Ellipse
        outlier_pca_plot <- outlier_pca_plot + stat_ellipse(type = "norm", level = 0.99, color = "black", linetype = "dashed", alpha = 0.5)
      }
      
      # 3. Override outliers to be black
      if (any(plot_df$Outlier_Status != "Normal")) {
        if (length(unique(metadata_filtered$Batch)) > 1) {
          outlier_pca_plot <- outlier_pca_plot + geom_point(data = subset(plot_df, Outlier_Status != "Normal"), aes(shape=Batch), color="black", size=5, alpha=1)
        } else {
          outlier_pca_plot <- outlier_pca_plot + geom_point(data = subset(plot_df, Outlier_Status != "Normal"), color="black", size=5, alpha=1)
        }
      }
      
      # 4. Labels and theme
      sub_title_text <- ifelse(args$outlier_method == "mahalanobis_group",
                               "Evaluates distance to group centroid & cross-group similarity. Outliers marked in black.",
                               "Dashed line represents global 99% confidence boundary. Outliers marked in black.")
      
      outlier_pca_plot <- outlier_pca_plot +
        ggrepel::geom_text_repel(aes(label = Sample), size = 3.5, max.overlaps = 15, box.padding = 0.5) +
        theme_bw(base_size = plot_font_size) +
        labs(
          title = paste("Outlier Detection (", ifelse(args$outlier_method == "mahalanobis_group", "Group-Centric", "Global"), ") -", args$project_name),
          subtitle = sub_title_text,
          x = paste0("PC1: ", round(percent_var[1] * 100), "% variance"), 
          y = paste0("PC2: ", round(percent_var[2] * 100), "% variance")
        ) +
        theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
      
      ggsave(file.path(qc_dir, paste0(args$project_name, "_Outlier_Detection_PCA.png")), plot = outlier_pca_plot, width = 10, height = 8, dpi = 300)
      
      # --- EXECUTE EXCLUSION ---
      if (length(samples_to_exclude) > 0) {
        send_progress(paste0("  EXCLUDING Outliers: ", paste(samples_to_exclude, collapse=", ")))
        
        # Filter Expression Matrix
        current_expr <- current_expr[, !colnames(current_expr) %in% samples_to_exclude, drop=FALSE]
        
        # Filter Metadata
        metadata_filtered <- metadata_filtered %>% filter(!Sample %in% samples_to_exclude)
        
      } else {
        send_progress("  No outliers excluded due to sample size constraints or perfect clustering.")
      }
      
      # Save outlier report
      if (args$outlier_method == "mahalanobis_group") {
        outlier_df <- data.frame(Sample = names(md_vals), Group_Distance = md_vals, Is_Outlier = names(md_vals) %in% potential_outliers, Misclassified = names(md_vals) %in% misclassified, Excluded = names(md_vals) %in% samples_to_exclude)
      } else {
        outlier_df <- data.frame(Sample = names(md_vals), Distance = md_vals, Is_Outlier = names(md_vals) %in% potential_outliers, Excluded = names(md_vals) %in% samples_to_exclude)
      }
      write.csv(outlier_df, file.path(qc_dir, paste0(args$project_name, "_Outlier_Report.csv")), row.names=FALSE)
    }
  }, error = function(e) send_progress(paste("Outlier detection skipped:", e$message)))
}


analysis_expr <- current_expr
write.csv(analysis_expr, file.path(qc_dir, paste0(args$project_name, "_Final_Corrected_Expression.csv")))

# *** UPDATED CALL ***
.create_qc_plots(analysis_expr, metadata_filtered, "3_Post_Correction_Final", qc_dir, args$project_name)

# Global Heatmap
all_groups <- unique(metadata_filtered$Group)
group_colors <- colorRampPalette(RColorBrewer::brewer.pal(min(length(all_groups), 9), "Set1"))(length(all_groups))
names(group_colors) <- all_groups
anno_colors <- list(Group = group_colors)

global_heatmap_file <- file.path(qc_dir, paste0(args$project_name, "_Global_Heatmap_Top100_Variable.png"))
if(!file.exists(global_heatmap_file)){
  row_vars <- rowVars(analysis_expr, na.rm = TRUE) 
  if (length(row_vars) >= 2) {
    top_variable_genes <- rownames(analysis_expr)[order(row_vars, decreasing = TRUE)[1:min(100, length(row_vars))]]
    heatmap_matrix <- analysis_expr[top_variable_genes, , drop = FALSE]
    
    tryCatch({
      pheatmap::pheatmap(t(scale(t(heatmap_matrix))), main = paste("Top Variable Proteins -", args$project_name),
                         annotation_col = data.frame(Group = metadata_filtered$Group, row.names=metadata_filtered$Sample),
                         annotation_colors = anno_colors, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
                         show_rownames = TRUE, show_colnames = TRUE, filename = global_heatmap_file, width = 10, height = 14)
    }, error = function(e) send_progress(paste("Global Heatmap failed:", e$message)))
  }
}

send_progress("Running Differential Expression analysis...")

# ============================================================================
# NEW: ADVANCED CONTRAST PARSING & LOCALIZED LIMMA
# ============================================================================

advanced_contrasts <- list()

if (args$contrasts_json != "" && file.exists(args$contrasts_json)) {
  send_progress("Loading Advanced Contrasts (JSON)...")
  tryCatch({
    advanced_contrasts <- jsonlite::fromJSON(args$contrasts_json, simplifyDataFrame = FALSE)
    
    # --- SMART RECOVERY FIX FOR MANGLED LEGACY OBJECTS ---
    for (i in seq_along(advanced_contrasts)) {
      c_obj <- advanced_contrasts[[i]]
      if (!is.null(c_obj$id) && grepl("^legacy_", c_obj$id)) {
        orig_str <- sub("^legacy_", "", c_obj$id)
        
        # Exact group string recovery by cross-referencing available groups
        matched_groups <- c()
        for (g in unique(metadata_filtered$Group)) {
          if (grepl(g, orig_str, fixed=TRUE)) {
            matched_groups <- c(matched_groups, g)
          }
        }
        
        if (length(matched_groups) >= 2) {
          # Sort descending so '486-Control' matches before 'Control'
          matched_groups <- matched_groups[order(-nchar(matched_groups))]
          
          advanced_contrasts[[i]]$cond1$value <- matched_groups[1]
          advanced_contrasts[[i]]$cond2$value <- matched_groups[2]
          advanced_contrasts[[i]]$name <- paste(matched_groups[1], "vs", matched_groups[2])
        }
      }
    }
    # -----------------------------------------------------
    
  }, error = function(e) send_progress(paste("JSON Parse Error:", e$message)))
} else {
  send_progress("Loading Legacy Contrasts...")
  # Fallback: Convert simple string "GroupA-GroupB" into the new advanced format
  if (length(legacy_contrasts_to_make) > 0) {
    for (c_str in legacy_contrasts_to_make) {
      # SMART MATCH: Avoid blind splitting on hyphens. Detect based on actual groups.
      matched_groups <- c()
      for (g in unique(metadata_filtered$Group)) {
        if (grepl(g, c_str, fixed=TRUE)) {
          matched_groups <- c(matched_groups, g)
        }
      }
      
      if (length(matched_groups) >= 2) {
        # Sort by string length descending to match longest possible group name first
        matched_groups <- matched_groups[order(-nchar(matched_groups))]
        
        advanced_contrasts[[length(advanced_contrasts) + 1]] <- list(
          id = paste0("legacy_", c_str),
          name = paste(matched_groups[1], "vs", matched_groups[2]),
          cond1 = list(type = "group", value = matched_groups[1]),
          cond2 = list(type = "group", value = matched_groups[2])
        )
      }
    }
  }
}

all_dge_results <- list() 

# --- Global Design Preparation (For pure Group vs Group optimization) ---
limma_metadata <- metadata_filtered
limma_metadata$Group <- make.names(limma_metadata$Group)
original_groups <- unique(metadata_filtered$Group)
sanitized_groups <- unique(limma_metadata$Group)
group_name_map <- setNames(sanitized_groups, original_groups)

global_fit_ebayes <- NULL
if (!split_batches && length(original_groups) > 1) {
  group_factors <- factor(limma_metadata$Group, levels = sanitized_groups)
  global_design <- model.matrix(~0 + group_factors)
  colnames(global_design) <- levels(group_factors)
  
  analysis_expr_filtered <- analysis_expr[matrixStats::rowVars(analysis_expr) > 1e-6, , drop = FALSE]
  global_fit <- tryCatch(lmFit(analysis_expr_filtered, global_design), error=function(e) NULL)
}

# Helper to run completely localized limma for Manual Samples or Batches
run_local_limma <- function(s1_vec, s2_vec, b_id = NULL) {
  if (!is.null(b_id)) {
    meta_sub <- metadata_filtered %>% filter(Batch == b_id, Sample %in% c(s1_vec, s2_vec))
    if (nrow(meta_sub) < 3) return(NULL)
    s1_vec <- intersect(s1_vec, meta_sub$Sample)
    s2_vec <- intersect(s2_vec, meta_sub$Sample)
  }
  
  if (length(s1_vec) == 0 || length(s2_vec) == 0) return(NULL)
  
  local_expr <- analysis_expr[, c(s1_vec, s2_vec), drop=FALSE]
  rv <- matrixStats::rowVars(local_expr)
  local_expr <- local_expr[rv > 1e-6 & !is.na(rv), , drop=FALSE]
  if (nrow(local_expr) < 2) return(NULL)
  
  cond_factor <- factor(c(rep("G1", length(s1_vec)), rep("G2", length(s2_vec))), levels=c("G1", "G2"))
  if (length(unique(cond_factor)) < 2) return(NULL)
  
  design_local <- model.matrix(~0 + cond_factor)
  colnames(design_local) <- levels(cond_factor)
  
  tryCatch({
    fit_local <- lmFit(local_expr, design_local)
    cont_local <- makeContrasts(G1 - G2, levels=design_local)
    fit2_local <- contrasts.fit(fit_local, cont_local)
    fit_ebayes_local <- eBayes(fit2_local, robust=TRUE)
    
    res <- topTable(fit_ebayes_local, coef=1, number=Inf, adjust.method="none")
    res$GeneSymbol <- rownames(res)
    return(res[, c("GeneSymbol", "logFC", "P.Value")])
  }, error = function(e) return(NULL))
}

# --- PROCESS EACH ADVANCED CONTRAST ---
if (length(advanced_contrasts) > 0) {
  
  for (c_obj in advanced_contrasts) {
    c_id <- c_obj$id
    c_name <- c_obj$name
    pretty_contrast_name <- gsub("[^A-Za-z0-9_]", "_", c_name)
    
    send_progress(paste("Processing Contrast:", c_name))
    
    contrast_dir <- file.path(args$output, paste0("Results_", pretty_contrast_name))
    dge_dir <- file.path(contrast_dir, "1_DGE_and_Plots")
    dir.create(dge_dir, showWarnings = FALSE, recursive = TRUE)
    
    # 1. Resolve Sample Vectors
    s1_samples <- if (c_obj$cond1$type == "group") {
      metadata_filtered$Sample[metadata_filtered$Group == c_obj$cond1$value]
    } else { unlist(c_obj$cond1$value) }
    
    s2_samples <- if (c_obj$cond2$type == "group") {
      metadata_filtered$Sample[metadata_filtered$Group == c_obj$cond2$value]
    } else { unlist(c_obj$cond2$value) }
    
    s1_samples <- intersect(s1_samples, colnames(analysis_expr))
    s2_samples <- setdiff(intersect(s2_samples, colnames(analysis_expr)), s1_samples) # Enforce disjoint
    
    if (length(s1_samples) == 0 || length(s2_samples) == 0) {
      send_progress(paste("  -> SKIPPED: Insufficient valid samples for condition."))
      next
    }
    
    top_table <- NULL
    
    # 2. Compute Top Table
    if (split_batches) {
      batch_results <- lapply(unique(metadata_filtered$Batch), function(b) run_local_limma(s1_samples, s2_samples, b))
      batch_results <- Filter(Negate(is.null), batch_results)
      if(length(batch_results) > 0) {
        meta_df <- Reduce(function(x, y) merge(x, y, by="GeneSymbol", all=FALSE), batch_results)
        if(nrow(meta_df) > 0) {
          logfc_cols <- grep("logFC", colnames(meta_df))
          pval_cols <- grep("P.Value", colnames(meta_df))
          meta_df$logFC <- rowMeans(meta_df[, logfc_cols, drop=FALSE], na.rm=TRUE)
          meta_df$P.Value <- apply(meta_df[, pval_cols, drop=FALSE], 1, function(p) pchisq(-2 * sum(log(p[p>0])), df=2*length(p[p>0]), lower.tail=FALSE))
          meta_df$adj.P.Val <- p.adjust(meta_df$P.Value, method="fdr")
          top_table <- meta_df; rownames(top_table) <- top_table$GeneSymbol
        }
      }
    } else {
      # Use Global Limma if Pure Group vs Group, else use Local Limma
      is_pure_group <- c_obj$cond1$type == "group" && c_obj$cond2$type == "group" && !is.null(global_fit)
      
      if (is_pure_group) {
        g1_s <- make.names(c_obj$cond1$value); g2_s <- make.names(c_obj$cond2$value)
        if (g1_s %in% colnames(global_design) && g2_s %in% colnames(global_design)) {
          tryCatch({
            c_mat <- makeContrasts(contrasts = paste(g1_s, g2_s, sep="-"), levels = global_design)
            fit2 <- eBayes(contrasts.fit(global_fit, c_mat), robust=TRUE)
            top_table <- topTable(fit2, coef=1, number=Inf, adjust.method="fdr")
            top_table$GeneSymbol <- rownames(top_table)
          }, error = function(e) NULL)
        }
      } 
      
      if (is.null(top_table)) {
        # Fallback / Local Limma for Manual Samples
        local_res <- run_local_limma(s1_samples, s2_samples, NULL)
        if (!is.null(local_res)) {
          local_res$adj.P.Val <- p.adjust(local_res$P.Value, method="fdr")
          top_table <- local_res; rownames(top_table) <- top_table$GeneSymbol
        }
      }
    }
    
    if (is.null(top_table) || nrow(top_table) == 0) next
    
    top_table$Significance <- "NS"
    top_table$Significance[top_table$adj.P.Val < p_cutoff & top_table$logFC > fc_cutoff] <- "Up"
    top_table$Significance[top_table$adj.P.Val < p_cutoff & top_table$logFC < -fc_cutoff] <- "Down"
    write.csv(top_table, file.path(dge_dir, paste0(args$project_name, "_DGE_Results_", pretty_contrast_name, ".csv")), row.names = FALSE)
    
    # Store globally for Venn
    all_dge_results[[c_id]] <- list(stats = top_table, dir = contrast_dir, pretty_name = c_name)
    
    # --- NEW: Automated KEGG Mapper Input Generation ---
    tryCatch({
      send_progress("    Generating KEGG Mapper input file...")
      
      # Filter for significant genes
      sig_for_kegg <- top_table %>% filter(Significance != "NS")
      
      if(nrow(sig_for_kegg) > 0) {
        # Translate to Entrez
        mapped_entrez <- suppressMessages(bitr(sig_for_kegg$GeneSymbol, fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db))
        
        # Format for KEGG: `hsa:EntrezID \t logFC`
        kegg_df <- sig_for_kegg %>% 
          inner_join(mapped_entrez, by=c("GeneSymbol"="SYMBOL")) %>%
          group_by(ENTREZID) %>%
          summarise(logFC = mean(logFC, na.rm=TRUE)) %>% # Average logFC if symbols map to same Entrez
          ungroup() %>%
          mutate(KEGG_ID = paste0("hsa:", ENTREZID)) %>% 
          select(KEGG_ID, logFC)
        
        kegg_file <- file.path(dge_dir, paste0(args$project_name, "_KEGG_Mapper_Input_", pretty_contrast_name, ".txt"))
        
        write.table(kegg_df, kegg_file, sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE)
        send_progress(paste("      Saved KEGG Mapper input."))
      } else {
        send_progress("      Skipping KEGG Mapper input: No significant DEGs found.")
      }
    }, error = function(e) send_progress(paste("      KEGG Mapper input generation failed:", e$message)))
    
    # Keyword Report
    if (ncol(annotation_map) > 1) {
      keyword_hits <- top_table %>% left_join(annotation_map, by = "GeneSymbol") %>% filter(if_any(starts_with("Is_"), ~ . == TRUE))
      if (nrow(keyword_hits) > 0) write.csv(keyword_hits, file.path(dge_dir, paste0(args$project_name, "_Keyword_Analysis_", pretty_contrast_name, ".csv")), row.names = FALSE)
    }
    
    # --- UPDATED: Pairwise QC Module (Now with Pre-Log, Post-Log, and Final) ---
    if (run_pairwise_qc) {
      send_progress(paste("    Generating Pairwise QC Plots (Pre-log, Post-log, and Final)..."))
      qc_pair_dir <- file.path(contrast_dir, "0_Pairwise_QC")
      dir.create(qc_pair_dir, showWarnings=FALSE, recursive=TRUE)
      
      sub_meta <- metadata_filtered %>% filter(Sample %in% c(s1_samples, s2_samples))
      # Ensure clean group labeling for the PCA
      sub_meta$Group <- ifelse(sub_meta$Sample %in% s1_samples, "Condition_1", "Condition_2")
      if (c_obj$cond1$type == "group") sub_meta$Group[sub_meta$Sample %in% s1_samples] <- c_obj$cond1$value
      if (c_obj$cond2$type == "group") sub_meta$Group[sub_meta$Sample %in% s2_samples] <- c_obj$cond2$value
      
      # 1. Pre-Log QC (using raw imputed data)
      sub_expr_imputed <- expr_imputed[, sub_meta$Sample, drop=FALSE]
      tryCatch({
        .create_qc_plots(sub_expr_imputed, sub_meta, paste0("1_Pre_Log_Pairwise_", pretty_contrast_name), qc_pair_dir, args$project_name)
      }, error = function(e) send_progress(paste("      Pairwise Pre-Log QC failed:", e$message)))
      
      # 2. Post-Log QC (using log-transformed data, pre-correction)
      sub_expr_log <- expr_log[, sub_meta$Sample, drop=FALSE]
      tryCatch({
        .create_qc_plots(sub_expr_log, sub_meta, paste0("2_Post_Log_Pairwise_", pretty_contrast_name), qc_pair_dir, args$project_name)
      }, error = function(e) send_progress(paste("      Pairwise Post-Log QC failed:", e$message)))
      
      # 3. Final Corrected QC
      sub_expr_final <- analysis_expr[, sub_meta$Sample, drop=FALSE]
      tryCatch({
        .create_qc_plots(sub_expr_final, sub_meta, paste0("3_Final_Pairwise_", pretty_contrast_name), qc_pair_dir, args$project_name)
      }, error = function(e) send_progress(paste("      Pairwise Final QC failed:", e$message)))
    }
    
    # --- NEW: DEG Hierarchical Heatmap Module ---
    if (run_deg_heatmaps) {
      send_progress(paste("    Generating DEG Heatmap (log2FC) for", pretty_contrast_name, "..."))
      # Using log2FC directly from Top Table as requested
      hm_data <- top_table %>% filter(Significance != "NS") %>% arrange(adj.P.Val) %>% head(50)
      
      if (nrow(hm_data) >= 2) {
        hm_mat <- matrix(hm_data$logFC, ncol = 1)
        rownames(hm_mat) <- hm_data$GeneSymbol
        colnames(hm_mat) <- c("Log2FC")
        
        max_val <- max(abs(hm_mat), na.rm = TRUE)
        # Safeguard if max_val is 0
        if(max_val == 0) max_val <- 1 
        breaks <- seq(-max_val, max_val, length.out = 100)
        colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(100)
        
        hm_file <- file.path(dge_dir, paste0(args$project_name, "_DEG_Heatmap_", pretty_contrast_name, ".png"))
        
        tryCatch({
          pheatmap::pheatmap(hm_mat, 
                             cluster_cols = FALSE, 
                             cluster_rows = TRUE, 
                             color = colors, 
                             breaks = breaks,
                             show_rownames = TRUE,
                             show_colnames = TRUE,
                             main = paste("Top DEGs Log2FC -", pretty_contrast_name),
                             filename = hm_file,
                             width = 5, 
                             height = max(6, nrow(hm_mat) * 0.2))
        }, error = function(e) send_progress(paste("      Heatmap failed:", e$message)))
      } else {
        send_progress("      Warning: Not enough DEGs to generate Heatmap.")
      }
    }
    
    # --- UPDATED VOLCANO PLOT (DENSITY-AWARE LABELING) ---
    volcano_file <- file.path(dge_dir, paste0(args$project_name, "_Volcano_Plot_", pretty_contrast_name, ".png"))
    if(!file.exists(volcano_file)){
      top_table$SigScore <- -log10(top_table$adj.P.Val) * abs(top_table$logFC)
      label_candidates <- top_table$GeneSymbol[top_table$Significance != "NS" & (rank(-top_table$SigScore) <= 50 | top_table$adj.P.Val < 1e-5)]
      top_table$Label <- ifelse(top_table$GeneSymbol %in% label_candidates, top_table$GeneSymbol, NA)
      
      volcano_plot <- ggplot(top_table, aes(x=logFC, y=-log10(adj.P.Val), color=Significance, label=Label)) +
        geom_point(alpha=0.6, aes(size = -log10(adj.P.Val))) + 
        scale_size_continuous(range = c(1, 4), guide = "none") +
        ggrepel::geom_text_repel(max.overlaps = 20, size = 3, box.padding = 0.5, point.padding = 0.2, force = 2, na.rm = TRUE) +
        scale_color_manual(values=c("Up"="#ca0020", "Down"="#0571b0", "NS"="grey")) +
        theme_bw(base_size = plot_font_size) +
        geom_hline(yintercept = -log10(p_cutoff), linetype="dashed") +
        geom_vline(xintercept = c(-fc_cutoff, fc_cutoff), linetype="dashed") +
        labs(title=paste("Volcano:", c_name), subtitle=paste("Cutoffs: P <", p_cutoff, ", FC >", fc_cutoff))
      
      save_plot_pub(volcano_plot, paste0(args$project_name, "_Volcano_Plot_", pretty_contrast_name), dge_dir)
    }
    
    # Bar Plot
    bar_plot_file <- file.path(dge_dir, paste0(args$project_name, "_BarPlot_Top20_DEGs_", pretty_contrast_name, ".png"))
    if(!file.exists(bar_plot_file)){
      top_20 <- bind_rows(
        top_table %>% filter(Significance == "Up") %>% arrange(adj.P.Val) %>% head(10),
        top_table %>% filter(Significance == "Down") %>% arrange(adj.P.Val) %>% head(10)
      ) %>% arrange(logFC)
      
      if(nrow(top_20) > 0) {
        bar_plot <- ggplot(top_20, aes(x = reorder(GeneSymbol, logFC), y = logFC, fill = logFC > 0)) +
          geom_col() + coord_flip() +
          scale_fill_manual(values = c("TRUE" = "#ca0020", "FALSE" = "#0571b0"), name = "Regulation", labels = c("Upregulated", "Downregulated")) +
          labs(title = paste("Top DEGs:", c_name), x = "Gene Symbol", y = "Log2 Fold Change") +
          theme_bw(base_size = plot_font_size)
        save_plot_pub(bar_plot, paste0(args$project_name, "_BarPlot_Top20_DEGs_", pretty_contrast_name), dge_dir)
      }
    }
    
    # --- ADDED: Enhanced Visualizations for Meta-Analysis ---
    tryCatch({
      send_progress(paste("    Running Enhanced Visualizations (Interactive Volcano & GSEA) for", c_name, "..."))
      
      # 1. Interactive Volcano
      generate_interactive_volcano(top_table, pretty_contrast_name, fc_cutoff, p_cutoff, dge_dir)
      
      # 2. GSEA 
      base_enrich_dir <- file.path(contrast_dir, "2_Enrichment_Analysis")
      gsea_input_vec <- setNames(top_table$logFC, top_table$GeneSymbol)
      gsea_input_vec <- sort(gsea_input_vec[!is.na(gsea_input_vec)], decreasing = TRUE)
      
      if(length(gsea_input_vec) > 10) {
        run_gsea_analysis(gsea_input_vec, pretty_contrast_name, base_enrich_dir)
      } else {
        send_progress("    Skipping GSEA: Not enough genes (< 10) with valid logFC.")
      }
    }, error = function(e) send_progress(paste("Warning: Enhanced visualization failed:", e$message)))
    
    # Run Enrichment (Now standardized with TFs)
    base_enrich_dir <- file.path(contrast_dir, "2_Enrichment_Analysis")
    gene_list_fc <- setNames(top_table$logFC, top_table$GeneSymbol)
    
    run_advanced_enrichment(top_table %>% filter(Significance == "Up") %>% pull(GeneSymbol), gene_list_fc, "Upregulated", pretty_contrast_name, base_enrich_dir)
    run_advanced_enrichment(top_table %>% filter(Significance == "Down") %>% pull(GeneSymbol), gene_list_fc, "Downregulated", pretty_contrast_name, base_enrich_dir)
    run_advanced_enrichment(top_table %>% filter(Significance != "NS") %>% pull(GeneSymbol), gene_list_fc, "Combined", pretty_contrast_name, base_enrich_dir)
    
    # --- NEW: Compare Cluster Analysis (Up vs Down) ---
    up_genes <- top_table %>% filter(Significance == "Up") %>% pull(GeneSymbol)
    down_genes <- top_table %>% filter(Significance == "Down") %>% pull(GeneSymbol)
    
    if(length(up_genes) >= 5 && length(down_genes) >= 5) {
      send_progress("    Running Compare Cluster Analysis (Up vs Down)...")
      gene_list_list <- list(Upregulated = up_genes, Downregulated = down_genes)
      
      entrez_list <- lapply(gene_list_list, function(x) {
        tryCatch(bitr(x, fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db)$ENTREZID, error=function(e) NULL)
      })
      entrez_list <- entrez_list[sapply(entrez_list, length) > 2] 
      
      if(length(entrez_list) >= 2) {
        tryCatch({
          ck_go <- compareCluster(geneCluster = entrez_list, fun = "enrichGO", OrgDb = org.Hs.eg.db, ont="BP")
          if(!is.null(ck_go) && nrow(as.data.frame(ck_go)) > 0) {
            ck_go <- setReadable(ck_go, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
            p_ck_go <- dotplot(ck_go, title = paste("GO Enrichment Comparison:", pretty_contrast_name)) + theme_bw()
            ggsave(file.path(base_enrich_dir, "CompareCluster_GO_Dotplot.png"), p_ck_go, width=10, height=8)
          }
        }, error = function(e) send_progress(paste("CompareCluster GO failed:", e$message)))
        
        tryCatch({
          ck_kegg <- compareCluster(geneCluster = entrez_list, fun = "enrichKEGG", organism="hsa")
          if(!is.null(ck_kegg) && nrow(as.data.frame(ck_kegg)) > 0) {
            ck_kegg <- setReadable(ck_kegg, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
            p_ck_kegg <- dotplot(ck_kegg, title = paste("KEGG Enrichment Comparison:", pretty_contrast_name)) + theme_bw()
            ggsave(file.path(base_enrich_dir, "CompareCluster_KEGG_Dotplot.png"), p_ck_kegg, width=10, height=8)
          }
        }, error = function(e) send_progress(paste("CompareCluster KEGG failed:", e$message)))
        
        tryCatch({
          if(require("ReactomePA", quietly=TRUE)) {
            ck_react <- compareCluster(geneCluster = entrez_list, fun = "enrichPathway", organism="human")
            if(!is.null(ck_react) && nrow(as.data.frame(ck_react)) > 0) {
              ck_react <- setReadable(ck_react, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
              p_ck_react <- dotplot(ck_react, title = paste("Reactome Comparison:", pretty_contrast_name)) + theme_bw()
              ggsave(file.path(base_enrich_dir, "CompareCluster_Reactome_Dotplot.png"), p_ck_react, width=10, height=8)
            }
          }
        }, error = function(e) send_progress(paste("CompareCluster Reactome failed:", e$message)))
      }
    }
  }
}

# --- ADDED: Global Random Forest Analysis ---
if(length(unique(metadata_filtered$Group)) > 1) {
  send_progress("--- 🌳 Running Global Random Forest Analysis ---")
  tryCatch({
    # Feature selection: Top 500 variable genes
    row_vars <- apply(analysis_expr, 1, var, na.rm=TRUE)
    top_rf_genes <- names(head(sort(row_vars, decreasing = TRUE), 500))
    
    if(length(top_rf_genes) > 10) {
      rf_data <- t(analysis_expr[top_rf_genes, ])
      rf_data <- as.data.frame(rf_data)
      rf_data$Group <- as.factor(metadata_filtered$Group)
      
      # Handle column names for RF
      colnames(rf_data) <- make.names(colnames(rf_data))
      
      set.seed(123)
      rf_model <- randomForest::randomForest(Group ~ ., data=rf_data, ntree=500, importance=TRUE)
      
      # Save Variable Importance Plot
      rf_plot_file <- file.path(qc_dir, paste0(args$project_name, "_RandomForest_VarImp.png"))
      png(rf_plot_file, width=10, height=8, units="in", res=300)
      varImpPlot(rf_model, main=paste("Random Forest Variable Importance -", args$project_name))
      dev.off()
      
      send_progress("Random Forest analysis complete.")
    } else {
      send_progress("Skipping Random Forest: Not enough variable genes.")
    }
  }, error = function(e) send_progress(paste("Random Forest Failed:", e$message)))
}

# --- Step 6: PPI Analysis ---
if(run_ppi && length(all_dge_results) > 0) {
  send_progress("--- 🛰️ Starting PPI Analysis Module ---")
  for (c_id in names(all_dge_results)) {
    res <- all_dge_results[[c_id]]
    sig_genes <- res$stats %>% filter(Significance != "NS") %>% pull(GeneSymbol)
    
    if (length(sig_genes) >= min_cluster_size) {
      ppi_dir <- file.path(res$dir, "3_PPI_Network_Analysis")
      dir.create(ppi_dir, showWarnings = FALSE, recursive = TRUE)
      tryCatch({ 
        run_ppi_analysis(sig_genes, setNames(res$stats$logFC, res$stats$GeneSymbol), res$pretty_name, ppi_dir, plot_font_size, cluster_algo = ppi_cluster_algo, hub_measure = ppi_hub_measure) 
      }, error = function(e) send_progress(paste0("PPI Error: ", e$message)))
    }
  }
}

# --- REWRITTEN VENN DIAGRAM LOGIC (Agnostic to fit_ebayes) ---
if(run_venn && length(all_dge_results) > 0) {
  send_progress("--- 📊 Starting Venn Diagram Module ---")
  summary_dir <- file.path(args$output, paste0(args$project_name, "_Summary_Analysis"))
  dir.create(summary_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (length(venn_contrasts_to_make) > 0) {
    for (venn_group in venn_contrasts_to_make) {
      # Identify which contrasts in the Venn config actually exist in our results map
      valid_venn_keys <- intersect(venn_group, names(all_dge_results))
      if(length(valid_venn_keys) < 2) next
      
      # Build a manual decision matrix from the pre-computed stats
      all_genes <- unique(unlist(lapply(valid_venn_keys, function(k) all_dge_results[[k]]$stats$GeneSymbol)))
      
      bin_mat_up <- matrix(0, nrow=length(all_genes), ncol=length(valid_venn_keys))
      bin_mat_down <- matrix(0, nrow=length(all_genes), ncol=length(valid_venn_keys))
      bin_mat_combined <- matrix(0, nrow=length(all_genes), ncol=length(valid_venn_keys))
      
      rownames(bin_mat_up) <- all_genes
      rownames(bin_mat_down) <- all_genes
      rownames(bin_mat_combined) <- all_genes
      
      pretty_names <- sapply(valid_venn_keys, function(k) all_dge_results[[k]]$pretty_name)
      colnames(bin_mat_up) <- pretty_names
      colnames(bin_mat_down) <- pretty_names
      colnames(bin_mat_combined) <- pretty_names
      
      for(j in seq_along(valid_venn_keys)) {
        k <- valid_venn_keys[j]
        stats <- all_dge_results[[k]]$stats
        
        up_genes <- stats$GeneSymbol[stats$Significance == "Up"]
        down_genes <- stats$GeneSymbol[stats$Significance == "Down"]
        
        if(length(up_genes) > 0) {
          bin_mat_up[up_genes, j] <- 1
          bin_mat_combined[up_genes, j] <- 1
        }
        if(length(down_genes) > 0) {
          bin_mat_down[down_genes, j] <- 1
          bin_mat_combined[down_genes, j] <- 1
        }
      }
      
      for (dir_name in c("Up", "Down", "Combined")) {
        mat_to_use <- if(dir_name=="Up") bin_mat_up else if(dir_name=="Down") bin_mat_down else bin_mat_combined
        
        counts <- limma::vennCounts(mat_to_use)
        if(sum(counts[,"Counts"]) > 0) {
          png(file.path(summary_dir, paste0("Venn_", dir_name, "_", paste(pretty_names, collapse="_"), ".png")), width=1000, height=1000, res=300)
          limma::vennDiagram(counts, main=paste(dir_name, "Genes"), cex=0.9)
          dev.off()
        }
      }
    }
  }
}

send_progress("--- ✅ Full analysis pipeline completed! ---")