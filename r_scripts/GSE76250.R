library(GEOquery)
library(limma)
library(dplyr)
library(pheatmap)
library(ggplot2)
library(tibble)
library(plotly)
library(htmlwidgets)
library(umap)
library(ggrepel)

# Source the standardization utilities (Automated relative paths)
if (file.exists("standardization_utils.R")) {
  source("standardization_utils.R")
  source("visualization_utils.R")
} else {
  source("./scripts/standardization_utils.R")
  source("./scripts/visualization_utils.R")
}

# Define Dataset ID
gse_id <- "GSE76250"

# Paths (Automated relative paths)
results_dir <- file.path("./output/results", gse_id)
plots_dir <- file.path("./output/plots", gse_id)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# Step 1: Get and filter sample information
gse <- getGEO(gse_id, GSEMatrix = TRUE)
gse_data <- gse[[1]]
sample_info <- pData(gse_data)

# --- Manual Sample Identification ---
if ("source_name_ch1" %in% colnames(sample_info)) {
  sample_info$Condition <- "Other"
  sample_info$Condition[sample_info$source_name_ch1 == "breast_TNBC"] <- "TNBC"
  sample_info$Condition[sample_info$source_name_ch1 == "breast_normal tissue"] <- "Normal"
} else {
  warning("'source_name_ch1' not found. Attempting automatic standardization.")
  sample_info <- standardize_metadata(sample_info, gse_id)
}

sample_info_filtered <- sample_info[sample_info$Condition %in% c("Normal", "TNBC"), ]

message("--- Sample Identification Check ---")
print(table(sample_info_filtered$Condition))
message("-----------------------------------")

# Sample annotation data setup
gpl <- getGEO("GPL17586", GSEMatrix = FALSE) 
gpl_data <- Table(gpl)

# Extract gene assignment
gpl_annotation <- gpl_data %>% dplyr::select(ID, gene_assignment) 

# --- FIX for GPL17586 Format ---
gpl_annotation$GeneSymbols <- sapply(strsplit(as.character(gpl_annotation$gene_assignment), " // "), function(x) {
  if(length(x) >= 2) { return(x[2]) } else { return(NA) }
})

gpl_annotation$GeneSymbols[gpl_annotation$GeneSymbols == "---"] <- NA
gpl_annotation <- gpl_annotation[!is.na(gpl_annotation$GeneSymbols), ]

# --- Clean Gene Symbols ---
gpl_annotation <- clean_gene_symbols(gpl_annotation, "GeneSymbols")

# Step 2: Create expression matrix
expr_data <- exprs(gse_data)
expr_data <- expr_data[, rownames(sample_info_filtered)]
expr_data <- normalizeBetweenArrays(expr_data, method = "quantile")

expr_df <- as.data.frame(expr_data) %>% rownames_to_column("ID")

# Step 3: Merge
expr_annotated <- merge(expr_df, gpl_annotation, by = "ID", all.x = TRUE)

# Step 4: Calculate mean
expr_weighted <- expr_annotated %>%
  group_by(GeneSymbols) %>%
  summarize(across(starts_with("GSM"), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
  filter(!is.na(GeneSymbols) & GeneSymbols != "") %>%
  column_to_rownames("GeneSymbols")

# --- PCA Outlier Removal (Overlap-Based) ---
filter_pca_overlaps <- function(expr_data, sample_info, group_col, target_groups = c("TNBC", "Normal"), n_pcs=2) {
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
    message(paste("Removed", length(samples_to_remove), "overlapping samples."))
  } else {
    message("No overlapping samples found.")
    sample_info_clean <- sample_info
    expr_data_clean <- expr_data
  }
  
  return(list(exprs = expr_data_clean, meta = sample_info_clean, full_meta = sample_info))
}

plot_pca_standard(expr_weighted, sample_info_filtered, "Condition", "GSE76250 Before", file.path(plots_dir, "PCA_Before.png"))

# Create TWO datasets
filtered_res <- filter_pca_overlaps(expr_weighted, sample_info_filtered, group_col="Condition", target_groups = c("TNBC", "Normal"))
expr_weighted_clean <- filtered_res$exprs
sample_info_clean <- filtered_res$meta
sample_info_full_audit <- filtered_res$full_meta

plot_pca_standard(expr_weighted_clean, sample_info_clean, "Condition", "GSE76250 After", file.path(plots_dir, "PCA_After.png"))

message("\n=== SAMPLES IN ORIGINAL (No Exclusion) ===")
print(table(sample_info_filtered$Condition))
message("\n=== SAMPLES IN CLEAN (With Exclusion) ===")
print(table(sample_info_clean$Condition))

# --- Design & Fit ---
run_dge <- function(expr_matrix, meta_data, suffix, original_meta = NULL) {
  # Save Metadata for Audit
  meta_to_save <- if(!is.null(original_meta)) original_meta else meta_data
  write.csv(meta_to_save, file.path(results_dir, paste0(gse_id, "_metadata", suffix, ".csv")), row.names = TRUE)
  
  meta_data$Condition <- gsub(" ", "", meta_data$Condition)
  condition_factor <- factor(meta_data$Condition)
  design <- model.matrix(~ 0 + condition_factor)
  colnames(design) <- levels(condition_factor)
  rownames(design) <- meta_data$geo_accession
  
  fit <- lmFit(expr_matrix, design)
  contrast_matrix <- makeContrasts(TNBC_vs_Normal = TNBC - Normal, levels = design)
  fit2 <- eBayes(contrasts.fit(fit, contrast_matrix))
  
  results <- topTable(fit2, coef = "TNBC_vs_Normal", adjust.method = "BH", number = Inf)
  results$GeneSymbol <- rownames(results)
  results$GeneSymbols <- rownames(results) # Standardize output columns
  
  # Use suffix if provided, but default to standard name for "NoExclusion"
  if(suffix == "_NoExclusion") {
    file_name <- "GSE76250_differential_expression_results.csv" # Standard Name
  } else {
    file_name <- paste0("GSE76250_differential_expression_results", suffix, ".csv")
  }
  
  write.csv(results, file.path(results_dir, file_name), row.names = TRUE)
  
  # Plots
  plot_title <- paste(gse_id, "TNBC vs Normal", suffix)
  plot_volcano_static(results, plot_title, file.path(plots_dir, paste0("TNBC_vs_Normal", suffix, "_volcano.png")))
  plot_volcano_interactive(results, plot_title, file.path(plots_dir, paste0("TNBC_vs_Normal", suffix, "_volcano.html")))
  plot_ma_interactive(results, plot_title, file.path(plots_dir, paste0("TNBC_vs_Normal", suffix, "_ma.html")))
}

# Run DGE WITHOUT Exclusion
message("\n--- Running DGE WITHOUT Exclusion ---")
run_dge(expr_weighted, sample_info_filtered, "_NoExclusion")

# Run DGE WITH Exclusion
if(nrow(sample_info_clean) <= nrow(sample_info_filtered)) {
  message("\n--- Running DGE WITH Exclusion ---")
  run_dge(expr_weighted_clean, sample_info_clean, "_WithExclusion", original_meta = sample_info_full_audit)
}

# UMAP
umap_results <- umap(t(expr_weighted))
umap_df <- as.data.frame(umap_results$layout)
umap_df$conditions <- sample_info_filtered$Condition[match(rownames(umap_df), sample_info_filtered$geo_accession)]

umap_plot <- ggplot(umap_df, aes(x = V1, y = V2, color = conditions)) +
  geom_point(alpha = 0.6) +
  labs(title = "UMAP Plot", x = "UMAP 1", y = "UMAP 2", color = "Condition") +
  theme_minimal()

ggsave(file.path(plots_dir, paste0(gse_id, "_umap_plot.png")), plot = umap_plot, width = 8, height = 6)