library(ggplot2)
library(ggrepel)
library(plotly)
library(htmlwidgets)

# ==============================================================================
# 1. Standard Static Volcano Plot
# ==============================================================================
plot_volcano_static <- function(results, title_text, output_path) {
  # Ensure columns exist
  if(!"GeneSymbols" %in% colnames(results)) results$GeneSymbols <- rownames(results)
  
  # Categorize
  results$Significance <- "Not Significant"
  results$Significance[results$logFC > 1 & results$adj.P.Val < 0.05] <- "Upregulated"
  results$Significance[results$logFC < -1 & results$adj.P.Val < 0.05] <- "Downregulated"
  
  # Plot
  p <- ggplot(results, aes(x = logFC, y = -log10(adj.P.Val))) +
    geom_point(aes(colour = Significance), alpha = 0.6, size = 1.5) +
    scale_colour_manual(values = c("Upregulated" = "#E41A1C", # Red
                                   "Downregulated" = "#377EB8", # Blue
                                   "Not Significant" = "#999999")) + # Gray
    geom_vline(xintercept = c(-1, 1), linetype = "dashed", colour = "black", alpha=0.5) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "black", alpha=0.5) +
    theme_minimal() +
    theme(legend.position = "top", 
          legend.title = element_blank(),
          plot.title = element_text(face="bold", hjust=0.5)) +
    labs(title = title_text,
         x = "Log2 Fold Change",
         y = "-Log10 Adjusted P-value")
  
  # Add labels for top 20 genes (10 up, 10 down)
  top_up <- results %>% filter(Significance == "Upregulated") %>% arrange(adj.P.Val) %>% head(10)
  top_down <- results %>% filter(Significance == "Downregulated") %>% arrange(adj.P.Val) %>% head(10)
  labelled_genes <- rbind(top_up, top_down)
  
  if (nrow(labelled_genes) > 0) {
    p <- p + geom_text_repel(data = labelled_genes,
                             aes(label = GeneSymbols),
                             size = 3,
                             max.overlaps = 20,
                             box.padding = 0.5)
  }
  
  ggsave(filename = output_path, plot = p, width = 8, height = 6, dpi = 300)
}

# ==============================================================================
# 2. Interactive Volcano Plot
# ==============================================================================
plot_volcano_interactive <- function(results, title_text, output_path) {
  if(!"GeneSymbols" %in% colnames(results)) results$GeneSymbols <- rownames(results)
  
  results$Color <- case_when(
    results$adj.P.Val < 0.05 & results$logFC > 1 ~ "Upregulated",
    results$adj.P.Val < 0.05 & results$logFC < -1 ~ "Downregulated",
    TRUE ~ "Not Significant"
  )
  
  p <- plot_ly(data = results, 
               x = ~logFC, 
               y = ~-log10(P.Value), 
               text = ~paste("Gene:", GeneSymbols,
                             "<br>LogFC:", round(logFC, 2),
                             "<br>Adj P:", format(adj.P.Val, digits=3)),
               color = ~Color,
               colors = c("Upregulated" = "#E41A1C", "Downregulated" = "#377EB8", "Not Significant" = "#999999"),
               type = "scatter", 
               mode = "markers", 
               marker = list(size = 5, opacity = 0.6)) %>%
    layout(title = title_text,
           xaxis = list(title = "Log Fold Change"),
           yaxis = list(title = "-Log10 P-value"))
  
  saveWidget(p, output_path, selfcontained = TRUE)
}

# ==============================================================================
# 3. Interactive MA Plot
# ==============================================================================
plot_ma_interactive <- function(results, title_text, output_path) {
  if(!"GeneSymbols" %in% colnames(results)) results$GeneSymbols <- rownames(results)
  if(!"AveExpr" %in% colnames(results)) return(NULL) # Skip if no expression data
  
  results$Color <- case_when(
    results$adj.P.Val < 0.05 & results$logFC > 1 ~ "Upregulated",
    results$adj.P.Val < 0.05 & results$logFC < -1 ~ "Downregulated",
    TRUE ~ "Not Significant"
  )
  
  p <- plot_ly(data = results, 
               x = ~AveExpr, 
               y = ~logFC, 
               text = ~paste("Gene:", GeneSymbols,
                             "<br>LogFC:", round(logFC, 2),
                             "<br>Avg Expr:", round(AveExpr, 2)),
               color = ~Color,
               colors = c("Upregulated" = "#E41A1C", "Downregulated" = "#377EB8", "Not Significant" = "#999999"),
               type = "scatter", 
               mode = "markers", 
               marker = list(size = 4, opacity = 0.5)) %>%
    layout(title = title_text,
           xaxis = list(title = "Average Expression"),
           yaxis = list(title = "Log Fold Change"))
  
  saveWidget(p, output_path, selfcontained = TRUE)
}

# ==============================================================================
# 4. Standard PCA Plot
# ==============================================================================
plot_pca_standard <- function(expr_data, sample_info, group_col, title_text, output_path) {
  # Align samples
  common <- intersect(colnames(expr_data), rownames(sample_info))
  expr_subset <- expr_data[, common]
  meta_subset <- sample_info[common, ]
  
  # Compute PCA
  pca <- prcomp(t(expr_subset), scale. = TRUE)
  pca_df <- as.data.frame(pca$x[, 1:2])
  pca_df$Group <- meta_subset[[group_col]]
  
  # Calculate variance explained
  var_explained <- round(summary(pca)$importance[2, 1:2] * 100, 1)
  
  p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 3, alpha = 0.7) +
    stat_ellipse(level = 0.95, linetype = 2, show.legend = FALSE) +
    labs(title = title_text,
         subtitle = paste("Total Samples:", nrow(pca_df)),
         x = paste0("PC1 (", var_explained[1], "%)"),
         y = paste0("PC2 (", var_explained[2], "%)")) +
    theme_minimal() +
    theme(plot.title = element_text(face="bold", hjust=0.5))
  
  ggsave(filename = output_path, plot = p, width = 8, height = 6, dpi = 300)
}