# Load necessary libraries
library(GEOquery)
library(limma)
library(dplyr)
library(pheatmap)
library(ggplot2)
library(ggvenn)
library(biomaRt)
library(plotly)
library(ggrepel)
library(umap)
library(htmlwidgets)
library(VennDiagram)

# Source utilities (Automated relative paths)
if (file.exists("standardization_utils.R")) {
  source("standardization_utils.R")
  source("visualization_utils.R")
} else {
  source("./scripts/standardization_utils.R")
  source("./scripts/visualization_utils.R")
}

# Define Dataset ID
gse_id <- "GSE65194"

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

# --- Standardize Metadata ---
sample_info <- standardize_metadata(sample_info, gse_id)

# Align data
common_samples <- intersect(colnames(exprs_data), rownames(sample_info))
exprs_data <- exprs_data[, common_samples]
sample_info <- sample_info[common_samples, ]

# Normalize
expr_matrix <- normalizeBetweenArrays(exprs_data, method = "quantile")

# Annotation
gpl <- getGEO("GPL570", GSEMatrix = FALSE)
gpl_data <- Table(gpl)
gpl_annotation <- data.frame(ID = gpl_data[["ID"]], GeneSymbols = gpl_data[["Gene Symbol"]])

# --- Clean Genes ---
gpl_annotation <- clean_gene_symbols(gpl_annotation, "GeneSymbols")

# Merge
expr_matrix_df <- as.data.frame(expr_matrix)
expr_matrix_df$ID <- rownames(expr_matrix_df)
expr_matrix_annotated <- merge(expr_matrix_df, gpl_annotation, by = "ID", all.x = TRUE)

# Weighted Mean
expr_matrix_weighted <- expr_matrix_annotated %>%
  group_by(GeneSymbols) %>%
  summarise(across(starts_with("GSM"), ~ weighted.mean(., na.rm = TRUE)), .groups = 'drop') %>%
  filter(trimws(GeneSymbols) != "") %>%
  as.data.frame()

rownames(expr_matrix_weighted) <- expr_matrix_weighted$GeneSymbols
expr_matrix_weighted <- expr_matrix_weighted[, -which(names(expr_matrix_weighted) == "GeneSymbols")]
expr_matrix_weighted <- as.matrix(expr_matrix_weighted)

# Define Groups
sample_info <- sample_info %>%
  mutate(Sample_group = case_when(
    Condition == "TNBC" ~ "TNBC",
    Condition == "Normal" ~ "Healthy",
    grepl("CellLine", characteristics_ch1) ~ "CellLine",
    grepl("Luminal A", characteristics_ch1) ~ "Luminal A",
    grepl("Luminal B", characteristics_ch1) ~ "Luminal B",
    grepl("Her2", characteristics_ch1) ~ "Her2",
    TRUE ~ "Other"
  )) %>%
  filter(Sample_group != "Other")

# --- PCA Outlier Removal (Overlap-Based) ---
filter_pca_overlaps <- function(expr_data, sample_info, group_col, target_groups, n_pcs=2) {
  message("\n--- Starting PCA-based Overlap Filtering ---")
  common <- intersect(colnames(expr_data), rownames(sample_info))
  expr_data <- expr_data[, common]
  sample_info <- sample_info[common, ]
  
  present_groups <- unique(sample_info[[group_col]])
  if (!all(target_groups %in% present_groups)) {
    message("Skipping overlap filtering.")
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
          message(paste("Sample", sample_id, "(", sample_group, ") closer to", other, ". Removing."))
          samples_to_remove <- c(samples_to_remove, sample_id)
        }
      }
    }
  }
  
  # Audit tracking
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

# --- REPORTING PLOTS (BEFORE CLEANING) ---
target_report_groups <- c("TNBC", "Healthy")
report_meta_before <- sample_info %>% filter(Sample_group %in% target_report_groups)
report_expr_before <- expr_matrix_weighted[, rownames(report_meta_before)]
plot_pca_standard(report_expr_before, report_meta_before, "Sample_group", "GSE65194 Reporting (Before)", file.path(plots_dir, "PCA_Before_Reporting.png"))

# Standard plot
plot_pca_standard(expr_matrix_weighted, sample_info, "Sample_group", "GSE65194 Before", file.path(plots_dir, "PCA_Before.png"))

filtered_res <- filter_pca_overlaps(expr_matrix_weighted, sample_info, group_col="Sample_group", target_groups = c("TNBC", "Healthy"))
expr_matrix_clean <- filtered_res$exprs
sample_info_clean <- filtered_res$meta
sample_info_full_audit <- filtered_res$full_meta

# --- REPORTING PLOTS (AFTER CLEANING) ---
report_meta_after <- sample_info_clean %>% filter(Sample_group %in% target_report_groups)
report_expr_after <- expr_matrix_clean[, rownames(report_meta_after)]
plot_pca_standard(report_expr_after, report_meta_after, "Sample_group", "GSE65194 Reporting (After)", file.path(plots_dir, "PCA_After_Reporting.png"))

# UMAP Reporting Isolation
set.seed(123)
umap_rep <- umap::umap(t(report_expr_after))
umap_rep_df <- data.frame(UMAP1 = umap_rep$layout[, 1], UMAP2 = umap_rep$layout[, 2], Group = report_meta_after$Sample_group)
ggplot(umap_rep_df, aes(x = UMAP1, y = UMAP2, color = Group)) + geom_point(size = 3) + theme_minimal() + labs(title = "UMAP: TNBC vs Healthy Isolation")
ggsave(file.path(plots_dir, "UMAP_Reporting.png"), width = 8, height = 6)

# Standard plots
plot_pca_standard(expr_matrix_clean, sample_info_clean, "Sample_group", "GSE65194 After", file.path(plots_dir, "PCA_After.png"))

message("\n=== SAMPLES IN ORIGINAL ==="); print(table(sample_info$Sample_group))
message("\n=== SAMPLES IN CLEAN ==="); print(table(sample_info_clean$Sample_group))

# Design & Fit
run_dge <- function(expr_matrix, meta_data, suffix, original_meta = NULL) {
  # Save Metadata for Audit
  meta_to_save <- if(!is.null(original_meta)) original_meta else meta_data
  write.csv(meta_to_save, file.path(results_dir, paste0(gse_id, "_metadata", suffix, ".csv")), row.names = TRUE)
  
  meta_data$Sample_group <- factor(meta_data$Sample_group)
  design <- model.matrix(~ 0 + Sample_group, data = meta_data)
  colnames(design) <- make.names(levels(meta_data$Sample_group))
  
  fit <- lmFit(expr_matrix, design)
  
  contrast_matrix <- makeContrasts(
    TNBC_vs_Healthy = TNBC - Healthy,
    TNBC_vs_CellLine = TNBC - CellLine,
    TNBC_vs_Luminal_A = TNBC - Luminal.A,
    TNBC_vs_Luminal_B = TNBC - Luminal.B,
    TNBC_vs_Her2 = TNBC - Her2,
    levels = design
  )
  
  fit2 <- contrasts.fit(fit, contrast_matrix)
  fit2 <- eBayes(fit2)
  
  for (contrast_name in colnames(contrast_matrix)) {
    results <- topTable(fit2, coef = contrast_name, adjust = "BH", sort.by = "P", number = Inf)
    results$GeneSymbol <- rownames(results)
    results$GeneSymbols <- rownames(results) # Compat
    
    file_name <- paste0(gse_id, "_", contrast_name, suffix, "_results.csv")
    write.csv(results, file = file.path(results_dir, file_name), row.names = FALSE)
    
    plot_title <- paste(gse_id, contrast_name, suffix)
    plot_volcano_static(results, plot_title, file.path(plots_dir, paste0(contrast_name, suffix, "_volcano.png")))
    plot_volcano_interactive(results, plot_title, file.path(plots_dir, paste0(contrast_name, suffix, "_volcano.html")))
    plot_ma_interactive(results, plot_title, file.path(plots_dir, paste0(contrast_name, suffix, "_ma.html")))
  }
}

message("\n--- Running DGE WITHOUT Exclusion ---")
run_dge(expr_matrix_weighted, sample_info, "_NoExclusion")

message("\n--- Running DGE WITH Exclusion ---")
run_dge(expr_matrix_clean, sample_info_clean, "_WithExclusion", original_meta = sample_info_full_audit)

# Standard UMAP
set.seed(123)
umap_results <- umap::umap(t(expr_matrix_weighted))
umap_df <- data.frame(UMAP1 = umap_results$layout[, 1],
                      UMAP2 = umap_results$layout[, 2],
                      Sample_group = sample_info$Sample_group)

umap_plot <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Sample_group)) +
  geom_point(size = 3) +
  labs(title = "UMAP Plot of Weighted Expression Matrix", x = "UMAP 1", y = "UMAP 2") +
  theme_minimal()

ggsave(file.path(plots_dir, paste0(gse_id, "_umap_plot_weighted.png")), plot = umap_plot, width = 8, height = 6)