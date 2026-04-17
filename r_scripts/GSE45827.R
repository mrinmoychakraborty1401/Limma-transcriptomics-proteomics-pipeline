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

# Define Dataset ID
gse_id <- "GSE45827"

# Paths (Automated relative paths)
results_dir <- file.path("./output/results", gse_id)
plots_dir <- file.path("./output/plots", gse_id)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# Load data
gse <- getGEO(gse_id, GSEMatrix = TRUE)[[1]]
sample_info <- pData(gse)

# Identify Samples
get_conditions <- function(row) {
  conditions_list <- c()
  tumor_subtype <- trimws(as.character(row[["tumor subtype:ch1"]]))
  diagnosis <- trimws(as.character(row[["diagnosis:ch1"]]))
  cell_origin <- trimws(as.character(row[["cell origin:ch1"]]))
  
  if (!is.na(tumor_subtype) && tumor_subtype != "N/A") conditions_list <- c(conditions_list, tumor_subtype)
  if (!is.na(diagnosis) && diagnosis != "N/A") conditions_list <- c(conditions_list, diagnosis)
  if (!is.na(cell_origin) && cell_origin != "N/A") conditions_list <- c(conditions_list, cell_origin)
  
  return(unique(conditions_list))
}

# apply returns a list if vectors are of different lengths
conditions_list <- apply(sample_info, 1, get_conditions)
sample_info$Condition <- "Other"

for (i in 1:nrow(sample_info)) {
  conds <- conditions_list[[i]]
  if ("Basal" %in% conds) {
    sample_info$Condition[i] <- "TNBC"
  } else if ("None (normal)" %in% conds || "Healthy" %in% conds) {
    sample_info$Condition[i] <- "Normal"
  } else if ("Luminal A" %in% conds) {
    sample_info$Condition[i] <- "LuminalA"
  } else if ("Luminal B" %in% conds) {
    sample_info$Condition[i] <- "LuminalB"
  } else if ("Her2" %in% conds) {
    sample_info$Condition[i] <- "Her2"
  } else if ("Breast carcinoma" %in% conds) {
    sample_info$Condition[i] <- "BreastCarcinoma"
  } else if ("Breast mammary gland" %in% conds) {
    sample_info$Condition[i] <- "BreastMammaryGland"
  }
}

sample_info_filtered <- sample_info[!sample_info$Condition %in% c("Other", "N/A", "Unknown"), ]

# Annotation & Expression
gpl <- getGEO("GPL570", GSEMatrix = FALSE) %>%
  Table() %>%
  dplyr::select(ID, `Gene Symbol`) %>%
  filter(`Gene Symbol` != "") %>%
  distinct(ID, .keep_all = TRUE)

colnames(gpl)[colnames(gpl) == "Gene Symbol"] <- "GeneSymbols"
gpl <- clean_gene_symbols(gpl, "GeneSymbols")

expr_data <- exprs(gse)
expr_data <- expr_data[, rownames(sample_info_filtered)]
expr_df <- as.data.frame(expr_data) %>% rownames_to_column("ID")
expr_annotated <- merge(expr_df, gpl, by = "ID", all.x = TRUE)

expr_weighted <- expr_annotated %>%
  group_by(GeneSymbols) %>%
  summarize(across(starts_with("GSM"), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
  filter(!is.na(GeneSymbols)) %>%
  column_to_rownames("GeneSymbols") 

expr_weighted <- as.matrix(expr_weighted)
expr_weighted <- normalizeBetweenArrays(expr_weighted, method = "quantile")

# PCA Outlier Removal (Overlap-Based)
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

plot_pca_standard(expr_weighted, sample_info_filtered, "Condition", "GSE45827 Before", file.path(plots_dir, "PCA_Before.png"))

# Create Clean Dataset
filtered_res <- filter_pca_overlaps(expr_weighted, sample_info_filtered, group_col="Condition", target_groups = c("TNBC", "Normal"))
expr_weighted_clean <- filtered_res$exprs
sample_info_clean <- filtered_res$meta
sample_info_full_audit <- filtered_res$full_meta

plot_pca_standard(expr_weighted_clean, sample_info_clean, "Condition", "GSE45827 After", file.path(plots_dir, "PCA_After.png"))

# Design & Fit
run_dge <- function(expr_matrix, meta_data, suffix, original_meta = NULL) {
  # FIX: Drop list-columns or non-atomic columns that break write.csv
  meta_to_save <- if(!is.null(original_meta)) original_meta else meta_data
  meta_to_save <- meta_to_save[, sapply(meta_to_save, is.atomic)]
  
  write.csv(meta_to_save, file.path(results_dir, paste0(gse_id, "_metadata", suffix, ".csv")), row.names = TRUE)
  
  meta_data$Condition <- gsub(" ", "", meta_data$Condition)
  condition_factor <- factor(meta_data$Condition)
  design <- model.matrix(~ 0 + condition_factor)
  colnames(design) <- levels(condition_factor)
  rownames(design) <- meta_data$geo_accession
  
  fit <- lmFit(expr_matrix, design)
  
  existing_levels <- colnames(design)
  make_contrast_str <- function(g1, g2) {
    if(g1 %in% existing_levels & g2 %in% existing_levels) paste(g1, g2, sep = "-") else NULL
  }
  
  potential_contrasts <- c(
    TNBC_vs_Normal = make_contrast_str("TNBC", "Normal"),
    TNBC_vs_Her2 = make_contrast_str("TNBC", "Her2"),
    TNBC_vs_LuminalA = make_contrast_str("TNBC", "LuminalA"),
    TNBC_vs_LuminalB = make_contrast_str("TNBC", "LuminalB"),
    Normal_vs_Her2 = make_contrast_str("Normal", "Her2"),
    Normal_vs_LuminalA = make_contrast_str("Normal", "LuminalA")
  )
  valid_contrasts <- potential_contrasts[!sapply(potential_contrasts, is.null)]
  
  if(length(valid_contrasts) > 0) {
    contrasts_matrix <- makeContrasts(contrasts = valid_contrasts, levels = design)
    fit2 <- eBayes(contrasts.fit(fit, contrasts_matrix))
    
    for (contrast_name in colnames(contrasts_matrix)) {
      results <- topTable(fit2, coef = contrast_name, adjust.method = "BH", number = Inf)
      results$GeneSymbol <- rownames(results)
      results$GeneSymbols <- rownames(results)
      
      file_name <- paste0(gse_id, "_results_", contrast_name, suffix, ".csv")
      write.csv(results, file.path(results_dir, file_name), row.names = TRUE)
      
      plot_title <- paste(gse_id, contrast_name, suffix)
      plot_volcano_static(results, plot_title, file.path(plots_dir, paste0(contrast_name, suffix, "_volcano.png")))
      plot_volcano_interactive(results, plot_title, file.path(plots_dir, paste0(contrast_name, suffix, "_volcano.html")))
      plot_ma_interactive(results, plot_title, file.path(plots_dir, paste0(contrast_name, suffix, "_ma.html")))
    }
  }
}

message("\n--- Running DGE WITHOUT Exclusion ---")
run_dge(expr_weighted, sample_info_filtered, "_NoExclusion")

message("\n--- Running DGE WITH Exclusion ---")
run_dge(expr_weighted_clean, sample_info_clean, "_WithExclusion", original_meta = sample_info_full_audit)

# UMAP
umap_results <- umap(t(expr_weighted))
umap_df <- as.data.frame(umap_results$layout)
umap_df$conditions <- sample_info_filtered$Condition[match(rownames(umap_df), sample_info_filtered$geo_accession)]

umap_plot <- ggplot(umap_df, aes(x = V1, y = V2, color = conditions)) +
  geom_point(alpha = 0.6) +
  labs(title = "UMAP Plot", x = "UMAP 1", y = "UMAP 2", color = "Condition") +
  theme_minimal()

ggsave(file.path(plots_dir, paste0(gse_id, "_umap_plot.png")), plot = umap_plot, width = 8, height = 6)