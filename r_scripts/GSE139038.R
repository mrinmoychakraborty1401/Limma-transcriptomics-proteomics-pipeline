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

# Source utilities (Automated relative paths)
if (file.exists("standardization_utils.R")) {
  source("standardization_utils.R")
  source("visualization_utils.R")
} else {
  source("./scripts/standardization_utils.R")
  source("./scripts/visualization_utils.R")
}

# Define Dataset ID
gse_id <- "GSE139038"

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

# Annotation
gpl <- getGEO("GPL27630", GSEMatrix = FALSE)
gpl_data <- Table(gpl)
gpl_annotation <- data.frame(ID = gpl_data[["ID"]], GeneSymbols = gpl_data[["Gene_Symbol"]])

# --- Clean Genes ---
gpl_annotation <- clean_gene_symbols(gpl_annotation, "GeneSymbols")

# Merge
expr_matrix_df <- as.data.frame(expr_matrix)
expr_matrix_df$ID <- rownames(expr_matrix_df)
expr_matrix_annotated <- merge(expr_matrix_df, gpl_annotation, by = "ID", all.x = TRUE)

expr_matrix_weighted <- expr_matrix_annotated %>%
  group_by(GeneSymbols) %>%
  summarise(across(starts_with("GSM"), ~ weighted.mean(., na.rm = TRUE)), .groups = 'drop') %>%
  filter(trimws(GeneSymbols) != "") %>%
  as.data.frame()

rownames(expr_matrix_weighted) <- expr_matrix_weighted$GeneSymbols
expr_matrix_weighted <- expr_matrix_weighted[, -which(names(expr_matrix_weighted) == "GeneSymbols")]
expr_matrix_weighted <- as.matrix(expr_matrix_weighted)

# Metadata Logic - Robust identification of subtypes and normal samples
sample_info <- sample_info %>%
  mutate(across(everything(), ~ trimws(tolower(as.character(.))))) %>%
  mutate(receptor_status = case_when(
    characteristics_ch1.4 == "er status: negative" & 
      characteristics_ch1.5 == "pr status: negative" &
      characteristics_ch1.6 == "her2 status: negative" ~ "TNBC",
    characteristics_ch1.4 == "er status: positive" & 
      characteristics_ch1.5 == "pr status: positive" &
      characteristics_ch1.6 == "her2 status: positive" ~ "TP",
    characteristics_ch1.4 == "er status: positive" & 
      characteristics_ch1.5 == "pr status: positive" &
      characteristics_ch1.6 == "her2 status: negative" ~ "Luminal_A",
    TRUE ~ "Other"
  ))

# Refine normal sample identification
if ("cancer stage:ch1" %in% colnames(sample_info)) {
  sample_info <- sample_info %>%
    mutate(receptor_status = case_when(
      receptor_status == "Other" & `cancer stage:ch1` %in% c("apparent normal", "paired normal") ~ `cancer stage:ch1`,
      TRUE ~ receptor_status
    ))
}
sample_info$receptor_status <- gsub(" ", "_", sample_info$receptor_status)

# PCA Outlier Removal (Overlap-Based)
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

plot_pca_standard(expr_matrix_weighted, sample_info, "receptor_status", "GSE139038 Before", file.path(plots_dir, "PCA_Before.png"))

# Create Clean Dataset (Only if groups exist)
target_grps <- c("TNBC", "apparent_normal")
filtered_res <- filter_pca_overlaps(expr_matrix_weighted, sample_info, group_col="receptor_status", target_groups = target_grps)
expr_weighted_clean <- filtered_res$exprs
sample_info_clean <- filtered_res$meta
sample_info_full_audit <- filtered_res$full_meta

plot_pca_standard(expr_weighted_clean, sample_info_clean, "receptor_status", "GSE139038 After", file.path(plots_dir, "PCA_After.png"))

# --- Design & Fit ---
run_dge <- function(expr_matrix, meta_data, suffix, original_meta = NULL) {
  # Audit Save
  meta_to_save <- if(!is.null(original_meta)) original_meta else meta_data
  meta_to_save <- meta_to_save[, sapply(meta_to_save, is.atomic)]
  write.csv(meta_to_save, file.path(results_dir, paste0(gse_id, "_metadata", suffix, ".csv")), row.names = TRUE)
  
  meta_data$receptor_status <- factor(meta_data$receptor_status)
  design <- model.matrix(~ 0 + receptor_status, data = meta_data)
  colnames(design) <- make.names(colnames(design))
  
  fit <- lmFit(expr_matrix, design)
  
  # Dynamic Contrast Generation
  valid_levels <- colnames(design)
  contrasts_list <- list()
  
  if ("receptor_statusTNBC" %in% valid_levels) {
    if ("receptor_statusapparent_normal" %in% valid_levels) {
      contrasts_list$TNBC_vs_Apparent_Normal <- "receptor_statusTNBC - receptor_statusapparent_normal"
    }
    if ("receptor_statuspaired_normal" %in% valid_levels) {
      contrasts_list$TNBC_vs_Paired_Normal <- "receptor_statusTNBC - receptor_statuspaired_normal"
    }
  }
  
  if (length(contrasts_list) > 0) {
    contrast_matrix <- makeContrasts(contrasts = unlist(contrasts_list), levels = design)
    fit2 <- contrasts.fit(fit, contrast_matrix)
    fit2 <- eBayes(fit2)
    
    for (contrast_name in colnames(contrast_matrix)) {
      results <- topTable(fit2, coef = contrast_name, adjust = "BH", number = Inf)
      results$GeneSymbol <- rownames(results)
      results$GeneSymbols <- rownames(results)
      
      file_name <- paste0(gse_id, "_", contrast_name, suffix, "_results.csv")
      write.csv(results, file = file.path(results_dir, file_name), row.names = FALSE)
      
      plot_title <- paste(gse_id, contrast_name, suffix)
      plot_volcano_static(results, plot_title, file.path(plots_dir, paste0(contrast_name, suffix, "_volcano.png")))
      plot_volcano_interactive(results, plot_title, file.path(plots_dir, paste0(contrast_name, suffix, "_volcano.html")))
      plot_ma_interactive(results, plot_title, file.path(plots_dir, paste0(contrast_name, suffix, "_ma.html")))
    }
  } else {
    message(" [!] Skipping DGE: No valid control groups found for contrasts.")
  }
}

message("\n--- Running DGE WITHOUT Exclusion ---")
run_dge(expr_matrix_weighted, sample_info, "_NoExclusion")

if(nrow(sample_info_clean) < nrow(sample_info)) {
  message("\n--- Running DGE WITH Exclusion ---")
  run_dge(expr_weighted_clean, sample_info_clean, "_WithExclusion", original_meta = sample_info_full_audit)
}

# UMAP
set.seed(123)
umap_results <- umap(t(expr_matrix_weighted))
umap_df <- data.frame(UMAP1 = umap_results$layout[, 1], UMAP2 = umap_results$layout[, 2], Sample_group = sample_info$receptor_status)
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Sample_group)) + geom_point(size = 3) + theme_minimal()
ggsave(file.path(plots_dir, paste0(gse_id, "_umap_plot_weighted.png")), width = 8, height = 6)