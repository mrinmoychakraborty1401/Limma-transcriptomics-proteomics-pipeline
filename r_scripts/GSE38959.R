# Load necessary libraries
library(GEOquery)
library(limma)
library(dplyr)
library(pheatmap)
library(ggplot2)
library(ggrepel)
library(plotly)
library(htmlwidgets)
library(umap)
library(tibble)

# Source utilities (Automated relative paths)
if (file.exists("standardization_utils.R")) {
  source("standardization_utils.R")
  source("visualization_utils.R")
} else {
  source("./scripts/standardization_utils.R")
  source("./scripts/visualization_utils.R")
}

# Define Dataset ID (FIXED)
gse_id <- "GSE38959"

# Paths (Automated relative paths)
results_dir <- file.path("./output/results", gse_id)
plots_dir <- file.path("./output/plots", gse_id)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# Download GEO dataset
gse <- getGEO(gse_id, GSEMatrix = TRUE)
gse <- gse[[1]]

exprs_data <- exprs(gse)
sample_info <- pData(gse)

# Check alignment
common_samples <- intersect(colnames(exprs_data), rownames(sample_info))
exprs_data <- exprs_data[, common_samples]
sample_info <- sample_info[common_samples, ]

# Normalize
expr_matrix <- normalizeBetweenArrays(exprs_data, method = "quantile")

# Annotation (FIXED Platform to GPL4133)
gpl <- getGEO("GPL4133", GSEMatrix = FALSE)
gpl_data <- Table(gpl)

# Dynamically find the gene symbol column for GPL4133
symbol_col_name <- grep("gene[ _]?symbol", colnames(gpl_data), ignore.case = TRUE, value = TRUE)[1]
if(is.na(symbol_col_name)) {
  symbol_col_name <- "GENE_SYMBOL" # Standard fallback for Agilent
}

gpl_annotation <- data.frame(ID = gpl_data[["ID"]], GeneSymbols = gpl_data[[symbol_col_name]])

# --- Clean Genes ---
gpl_annotation <- clean_gene_symbols(gpl_annotation, "GeneSymbols")

# Merge
expr_matrix_df <- as.data.frame(expr_matrix) %>% rownames_to_column("ID")
expr_matrix_annotated <- merge(expr_matrix_df, gpl_annotation, by = "ID", all.x = TRUE)

# Weighted Mean
expr_matrix_weighted <- expr_matrix_annotated %>%
  group_by(GeneSymbols) %>%
  summarise(across(starts_with("GSM"), ~ weighted.mean(., na.rm = TRUE)), .groups = 'drop') %>%
  filter(!is.na(GeneSymbols) & trimws(GeneSymbols) != "") %>%
  column_to_rownames("GeneSymbols")

expr_matrix_weighted <- as.matrix(expr_matrix_weighted)

# Metadata Logic (FIXED for GSE38959)
# Use standardize_metadata utility as primary, with robust fallback
sample_info <- standardize_metadata(sample_info, gse_id)

if(all(sample_info$Condition == "Other")) {
  message("Fallback metadata parsing for GSE38959...")
  sample_info$Condition <- "Other"
  
  # Search characteristics and title columns
  char_cols <- grep("characteristics", colnames(sample_info), value = TRUE)
  for(col in char_cols) {
    sample_info$Condition[grepl("triple negative|TNBC", sample_info[[col]], ignore.case=TRUE)] <- "TNBC"
    sample_info$Condition[grepl("normal|healthy", sample_info[[col]], ignore.case=TRUE)] <- "Normal"
  }
  
  sample_info$Condition[grepl("triple negative|TNBC", sample_info$title, ignore.case=TRUE)] <- "TNBC"
  sample_info$Condition[grepl("normal|healthy", sample_info$title, ignore.case=TRUE)] <- "Normal"
}

sample_info_filtered <- sample_info[sample_info$Condition %in% c("TNBC", "Normal"), ]

message("\n=== SAMPLES IDENTIFIED ===")
print(table(sample_info_filtered$Condition))

# --- PCA Outlier Removal (Overlap-Based) ---
filter_pca_overlaps <- function(expr_data, sample_info, group_col, target_groups, n_pcs=2) {
  message("\n--- Starting PCA-based Overlap Filtering ---")
  
  common <- intersect(colnames(expr_data), rownames(sample_info))
  expr_data <- expr_data[, common]
  sample_info <- sample_info[common, ]
  
  present_groups <- unique(sample_info[[group_col]])
  if (!all(target_groups %in% present_groups)) {
    message("Not all target groups present. Skipping overlap filtering.")
    return(list(exprs = expr_data, meta = sample_info))
  }
  
  pca <- prcomp(t(expr_data), scale. = TRUE)
  pca_df <- as.data.frame(pca$x[, 1:n_pcs])
  pca_df$Group <- sample_info[[group_col]]
  
  centroids <- list()
  for(grp in target_groups) {
    grp_data <- pca_df[pca_df$Group == grp, 1:n_pcs]
    centroids[[grp]] <- apply(grp_data, 2, median)
  }
  
  samples_to_remove <- c()
  
  for (i in 1:nrow(pca_df)) {
    sample_id <- rownames(pca_df)[i]
    sample_group <- pca_df$Group[i]
    
    if (sample_group %in% target_groups) {
      coords <- as.numeric(pca_df[i, 1:n_pcs])
      dist_own <- sqrt(sum((coords - centroids[[sample_group]])^2))
      other_groups <- setdiff(target_groups, sample_group)
      
      for (other in other_groups) {
        dist_other <- sqrt(sum((coords - centroids[[other]])^2))
        if (dist_other <= dist_own) {
          message(paste("Sample", sample_id, "(Group:", sample_group, ") closer to", other, ". Removing."))
          samples_to_remove <- c(samples_to_remove, sample_id)
        }
      }
    }
  }
  
  sample_info$is_outlier <- rownames(sample_info) %in% samples_to_remove
  
  if(length(samples_to_remove) > 0) {
    clean_samples <- setdiff(rownames(sample_info), samples_to_remove)
    sample_info_clean <- sample_info[clean_samples, ]
    expr_data_clean <- expr_data[, clean_samples]
    message(paste("Total removed:", length(samples_to_remove)))
  } else {
    message("No overlaps found.")
    sample_info_clean <- sample_info
    expr_data_clean <- expr_data
  }
  
  return(list(exprs = expr_data_clean, meta = sample_info_clean, full_meta = sample_info))
}

# Standard plot (Before)
plot_pca_standard(expr_matrix_weighted, sample_info_filtered, "Condition", "GSE38959 Before", file.path(plots_dir, "PCA_Before.png"))

# Create Clean Dataset
target_grps <- c("TNBC", "Normal")
filtered_res <- filter_pca_overlaps(expr_matrix_weighted, sample_info_filtered, group_col="Condition", target_groups = target_grps)
expr_weighted_clean <- filtered_res$exprs
sample_info_clean <- filtered_res$meta
sample_info_full_audit <- filtered_res$full_meta

# Standard plot (After)
plot_pca_standard(expr_weighted_clean, sample_info_clean, "Condition", "GSE38959 After", file.path(plots_dir, "PCA_After.png"))

message("\n=== SAMPLES IN ORIGINAL (No Exclusion) ===")
print(table(sample_info_filtered$Condition))
message("\n=== SAMPLES IN CLEAN (With Exclusion) ===")
print(table(sample_info_clean$Condition))

# --- Design & Fit ---
run_dge <- function(expr_matrix, meta_data, suffix, original_meta = NULL) {
  # Audit Save
  meta_to_save <- if(!is.null(original_meta)) original_meta else meta_data
  meta_to_save <- meta_to_save[, sapply(meta_to_save, is.atomic)]
  write.csv(meta_to_save, file.path(results_dir, paste0(gse_id, "_metadata", suffix, ".csv")), row.names = TRUE)
  
  meta_data$Condition <- factor(meta_data$Condition)
  design <- model.matrix(~ 0 + Condition, data = meta_data)
  colnames(design) <- make.names(colnames(design))
  
  fit <- lmFit(expr_matrix, design)
  
  if("TNBC" %in% colnames(design) && "Normal" %in% colnames(design)) {
    contrast_matrix <- makeContrasts(TNBC_vs_Normal = TNBC - Normal, levels = design)
    
    fit2 <- contrasts.fit(fit, contrast_matrix)
    fit2 <- eBayes(fit2)
    
    results <- topTable(fit2, coef = "TNBC_vs_Normal", adjust = "BH", number = Inf)
    results$GeneSymbol <- rownames(results)
    results$GeneSymbols <- rownames(results)
    
    # Exact mapping to orchestrator expected filenames
    if(suffix == "_NoExclusion") {
      file_name <- "GSE38959_differential_expression_results_NoExclusion.csv"
    } else {
      file_name <- "GSE38959_differential_expression_results_WithExclusion.csv"
    }
    
    write.csv(results, file = file.path(results_dir, file_name), row.names = FALSE)
    
    plot_title <- paste(gse_id, "TNBC_vs_Normal", suffix)
    plot_volcano_static(results, plot_title, file.path(plots_dir, paste0("TNBC_vs_Normal", suffix, "_volcano.png")))
    plot_volcano_interactive(results, plot_title, file.path(plots_dir, paste0("TNBC_vs_Normal", suffix, "_volcano.html")))
    plot_ma_interactive(results, plot_title, file.path(plots_dir, paste0("TNBC_vs_Normal", suffix, "_ma.html")))
  } else {
    message(" [!] Skipping DGE: TNBC or Normal group missing.")
  }
}

message("\n--- Running DGE WITHOUT Exclusion ---")
run_dge(expr_matrix_weighted, sample_info_filtered, "_NoExclusion")

if(nrow(sample_info_clean) < nrow(sample_info_filtered)) {
  message("\n--- Running DGE WITH Exclusion ---")
  run_dge(expr_weighted_clean, sample_info_clean, "_WithExclusion", original_meta = sample_info_full_audit)
}

# UMAP
set.seed(123)
umap_results <- umap(t(expr_matrix_weighted))
umap_df <- data.frame(UMAP1 = umap_results$layout[, 1], UMAP2 = umap_results$layout[, 2], Condition = sample_info_filtered$Condition)
umap_plot <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Condition)) + geom_point(size = 3) + theme_minimal()
ggsave(file.path(plots_dir, paste0(gse_id, "_umap_plot_weighted.png")), plot = umap_plot, width = 8, height = 6)