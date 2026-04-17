# ==============================================================================
# PROTOCOL: STRICT PRESERVATION MODE
# 1. DO NOT REMOVE EXISTING FEATURES for "cleanliness" or "minimalism".
# 2. IF EDITING: Only modify the specific logic requested.
# ==============================================================================

# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(pheatmap)
library(tibble)
library(plotly)
library(htmlwidgets)
library(ggrepel)
library(WGCNA)
library(igraph)
library(RColorBrewer)
library(viridis)

# --- CONNECTION RESET (Fixes 'all 128 connections are in use' error) ---
closeAllConnections()
graphics.off()

# ==============================================================================
# 0. Directory Configuration (Split Script and Data Folders)
# ==============================================================================
# The folder where your analysis scripts (.R files) are saved (Automated relative path)
SCRIPT_DIR <- "./scripts"

# The folder where the scripts save their results and where the meta-analysis runs (Automated relative path)
DATA_DIR   <- "./"

# Set working directory to the Data/Results folder
setwd(DATA_DIR)

message("\n========================================================")
message("Starting Integrated Professional Meta-Analysis & WGCNA")
message(paste("Scripts Directory:", SCRIPT_DIR))
message(paste("Data Directory:   ", DATA_DIR))
message("========================================================")

# ==============================================================================
# 1. Flexible File Helper (Enhanced for Results Subdirectories)
# ==============================================================================
find_file_flexible <- function(path_str) {
  # 1. Try exact path provided
  if (file.exists(path_str)) return(path_str)
  
  # 2. Extract GSE ID and filename to try standard relative paths
  fname <- basename(path_str)
  gse_match <- regmatches(fname, regexpr("GSE[0-9]+", fname))
  gse_id <- if(length(gse_match) > 0) gse_match[1] else NULL
  
  # 3. Try standard result path: results/GSE_ID/filename
  if (!is.null(gse_id)) {
    res_path <- file.path("output/results", gse_id, fname)
    if (file.exists(res_path)) return(res_path)
    # Also check legacy path just in case
    res_path_legacy <- file.path("results", gse_id, fname)
    if (file.exists(res_path_legacy)) return(res_path_legacy)
  }
  
  # 4. Try looking in the Data Directory (WD) root
  wd_path <- file.path(DATA_DIR, fname)
  if (file.exists(wd_path)) return(wd_path)
  
  # 5. Try looking in the Script Directory
  script_path <- file.path(SCRIPT_DIR, fname)
  if (file.exists(script_path)) return(script_path)
  
  # 6. Final recursive search in the Data Directory
  search_res <- list.files(DATA_DIR, pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE)
  if (length(search_res) > 0) return(search_res[1])
  
  return(NULL) # Not found
}

# ==============================================================================
# 0. Orchestration: Run Individual GSE Scripts (with Smart Caching)
# ==============================================================================
# Set this to TRUE to run GSE analysis scripts.
run_all_individual_scripts <- TRUE 

if (run_all_individual_scripts) {
  # Mapping of scripts to their primary output marker (UPDATED TO EXACT FILENAMES)
  script_output_map <- list(
    "GSE38959.R"     = "GSE38959_differential_expression_results_NoExclusion.csv",
    "GSE65194.R"     = "GSE65194_TNBC_vs_Healthy_NoExclusion_results.csv",
    "GSE76250.R"     = "GSE76250_differential_expression_results.csv", 
    "GSE45827.R"     = "GSE45827_results_TNBC-Normal_NoExclusion.csv",
    "GSE139038v2.R"  = "GSE139038_receptor_statusTNBC - receptor_statusapparent_normal_NoExclusion_results.csv"
  )
  
  message("\n[ORCHESTRATION] Starting execution of individual GSE pipelines...")
  
  for (script_name in names(script_output_map)) {
    output_marker <- script_output_map[[script_name]]
    
    # Check if results already exist using flexible lookup
    if (!is.null(find_file_flexible(output_marker))) {
      message(paste(" >>> [SKIPPING] :", script_name, "(Results already detected)"))
    } else {
      actual_script_path <- file.path(SCRIPT_DIR, script_name)
      if (file.exists(actual_script_path)) {
        message(paste("\n >>> [EXECUTING] :", actual_script_path, "--------------------"))
        tryCatch({
          # Use local = FALSE to ensure objects are loaded into global environment
          source(actual_script_path, local = FALSE)
          message(paste(" >>> [COMPLETED] :", script_name))
        }, error = function(e) {
          message(paste(" [!] ERROR running", script_name, ":", e$message))
        })
      } else {
        message(paste(" [!] WARNING: Script not found in:", SCRIPT_DIR, "/", script_name))
      }
    }
  }
}

# ==============================================================================
# 2. Advanced Statistical Helpers
# ==============================================================================
calc_random_effects <- function(logfcs, weights) {
  logfcs <- as.numeric(na.omit(logfcs)); weights <- as.numeric(na.omit(weights))
  k <- length(logfcs)
  if(k < 2) return(list(logFC = mean(logfcs), i2 = 0, tau2 = 0))
  vw <- sum(weights); vw2 <- sum(weights^2)
  mu_fixed <- sum(weights * logfcs) / vw
  Q <- sum(weights * (logfcs - mu_fixed)^2); df <- k - 1
  tau2 <- max(0, (Q - df) / (vw - (vw2 / vw)))
  new_weights <- 1 / ((1/weights) + tau2)
  mu_random <- sum(new_weights * logfcs) / sum(new_weights)
  i2 <- max(0, (Q - df) / Q) * 100
  return(list(logFC = mu_random, i2 = i2, tau2 = tau2))
}

# ==============================================================================
# 3. Sample Exclusion Audit Function
# ==============================================================================
generate_exclusion_audit <- function(dataset_map, suffix) {
  message(paste("\n--- Auditing Sample Exclusions and Group Breakdown for:", suffix, "---"))
  audit_results <- list()
  
  for (ds in dataset_map) {
    gse_id <- sub("_.*", "", ds$id)
    # Corrected: Added underscore to match GSE script naming (e.g., _NoExclusion)
    meta_name <- paste0(gse_id, "_metadata_", suffix, ".csv")
    meta_path <- find_file_flexible(meta_name)
    
    if (!is.null(meta_path)) {
      message(paste(" [AUDIT] Processing metadata for study:", ds$id))
      meta <- read_csv(meta_path, show_col_types = FALSE)
      
      id_col <- if("geo_accession" %in% colnames(meta)) "geo_accession" else if("Sample" %in% colnames(meta)) "Sample" else colnames(meta)[1]
      group_col <- if("receptor_status" %in% colnames(meta)) "receptor_status" else if("Sample_group" %in% colnames(meta)) "Sample_group" else if("Condition" %in% colnames(meta)) "Condition" else grep("group|condition|status", colnames(meta), ignore.case = TRUE, value = TRUE)[1]
      
      if (is.na(group_col)) {
        message(paste(" [!] ERROR: Could not identify grouping column for", ds$id))
        next
      }
      
      if ("is_outlier" %in% colnames(meta)) {
        meta$is_outlier <- as.logical(meta$is_outlier)
        group_summary <- meta %>%
          group_by(!!sym(group_col)) %>%
          summarise(Original_N = n(), Excluded_N = sum(is_outlier), Final_N = sum(!is_outlier), .groups = "drop")
        group_stats_str <- paste(apply(group_summary, 1, function(x) paste0(x[1], ": ", x[2], "->", x[4], " (Dropped: ", x[3], ")")), collapse = " | ")
      } else {
        group_summary <- meta %>% group_by(!!sym(group_col)) %>% summarise(Original_N = n(), .groups = "drop")
        group_stats_str <- paste(apply(group_summary, 1, function(x) paste0(x[1], ": ", x[2])), collapse = " | ")
      }
      
      audit_results[[ds$id]] <- data.frame(Study = ds$id, Grouping_Used = group_col, Original_Total = nrow(meta), Group_Breakdown = group_stats_str, Pipeline = suffix)
    } else {
      # Debugging message to show where it's looking
      message(paste(" [!] WARNING: Metadata file not found for", ds$id, "- Expected:", meta_name))
    }
  }
  
  if (length(audit_results) > 0) {
    final_audit <- do.call(rbind, audit_results)
    out_dir <- file.path("output/results", "MetaAnalysis", suffix)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    write_csv(final_audit, file.path(out_dir, "Detailed_Sample_Exclusion_Audit.csv"))
    message(paste(" [INFO] Audit report saved to:", file.path(out_dir, "Detailed_Sample_Exclusion_Audit.csv")))
  } else {
    message(" [!] FAILED: No metadata files found to generate audit report.")
  }
}

# ==============================================================================
# 4. Meta-Analysis Main Logic
# ==============================================================================
run_meta_analysis_logic <- function(dataset_map, suffix_label) {
  output_folder <- file.path("output/results", "MetaAnalysis", suffix_label)
  plots_folder  <- file.path("output/plots", "MetaAnalysis", suffix_label)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  dir.create(plots_folder, recursive = TRUE, showWarnings = FALSE)
  
  merged_data <- NULL
  for (ds in dataset_map) {
    actual_path <- find_file_flexible(ds$file)
    if (is.null(actual_path)) { message(paste(" [MISSING] File:", ds$file)); next }
    
    message(paste(" [INFO] Loading dataset from:", actual_path))
    raw_data <- read_csv(actual_path, show_col_types = FALSE)
    
    if (!"GeneSymbol" %in% colnames(raw_data)) {
      possible_cols <- colnames(raw_data)[grep("gene|symbol", colnames(raw_data), ignore.case = TRUE)]
      if (length(possible_cols) > 0) colnames(raw_data)[colnames(raw_data) == possible_cols[1]] <- "GeneSymbol"
    }
    
    clean_data <- raw_data %>% 
      dplyr::select(GeneSymbol, logFC, P.Value, any_of("t")) %>% 
      filter(!is.na(GeneSymbol) & GeneSymbol != "") %>% 
      mutate(logFC = as.numeric(logFC), P.Value = as.numeric(P.Value))
    
    # Scale logFC to ensure cross-study comparability
    clean_data$logFC <- (clean_data$logFC - mean(clean_data$logFC, na.rm=TRUE)) / sd(clean_data$logFC, na.rm=TRUE)
    
    if ("t" %in% colnames(clean_data)) { 
      clean_data <- clean_data %>% mutate(Weight = (as.numeric(t) / logFC)^2) 
    } else { 
      clean_data <- clean_data %>% mutate(SE = abs(logFC / qnorm(1 - P.Value/2)), Weight = 1/(SE^2)) 
    }
    
    clean_data <- clean_data %>% 
      group_by(GeneSymbol) %>% 
      summarise(logFC = mean(logFC, na.rm = TRUE), P.Value = min(P.Value, na.rm = TRUE), Weight = mean(Weight, na.rm = TRUE), .groups = "drop") %>% 
      rename_with(~ paste0(., "_", ds$id), .cols = c("logFC", "P.Value", "Weight"))
    
    if (is.null(merged_data)) { merged_data <- clean_data } else { merged_data <- full_join(merged_data, clean_data, by = "GeneSymbol") }
  }
  
  if (is.null(merged_data)) return(message(" [!] Meta-analysis skipped: No input data found."))
  
  meta_stats <- merged_data %>% 
    rowwise() %>% 
    mutate(N_Studies = sum(!is.na(c_across(starts_with("logFC")))), 
           RE_Result = list(calc_random_effects(c_across(starts_with("logFC")), c_across(starts_with("Weight"))))) %>% 
    mutate(Final_LogFC = RE_Result$logFC, I2_Heterogeneity = RE_Result$i2, tau2 = RE_Result$tau2) %>% 
    dplyr::select(-RE_Result) %>% 
    ungroup()
  
  meta_results <- meta_stats %>% 
    rowwise() %>% 
    mutate(Meta_P_Value = pchisq(-2 * sum(log(pmax(1e-300, na.omit(c_across(starts_with("P.Value")))))), df = 2 * N_Studies, lower.tail = FALSE)) %>% 
    ungroup() %>% 
    mutate(Meta_Adj_P = p.adjust(Meta_P_Value, method = "BH"))
  
  write_csv(meta_results, file.path(output_folder, "Full_Meta_Analysis_Table.csv"))
  generate_exclusion_audit(dataset_map, suffix_label)
}

# --- Execution Block ---
# ALIGNED TO THE EXACT FILENAMES ACTUALLY SAVED BY THE GSE SCRIPTS
target_files <- list(
  list(id = "GSE139038_AN", file = "GSE139038_receptor_statusTNBC - receptor_statusapparent_normal_NoExclusion_results.csv"),
  list(id = "GSE139038_PN", file = "GSE139038_receptor_statusTNBC - receptor_statuspaired_normal_NoExclusion_results.csv"),
  list(id = "GSE45827",     file = "GSE45827_results_TNBC-Normal_NoExclusion.csv"),
  list(id = "GSE76250",     file = "GSE76250_differential_expression_results.csv"), 
  list(id = "GSE65194",     file = "GSE65194_TNBC_vs_Healthy_NoExclusion_results.csv"),
  list(id = "GSE38959",     file = "GSE38959_differential_expression_results_NoExclusion.csv")
)

run_meta_analysis_logic(target_files, "NoExclusion")

target_files_exclusion <- lapply(target_files, function(x) {
  # Special case for GSE76250 which doesn't have suffix in NoExclusion version
  if(x$id == "GSE76250") {
    ex_file <- gsub(".csv", "_WithExclusion.csv", x$file)
  } else {
    ex_file <- gsub("NoExclusion", "WithExclusion", x$file)
  }
  list(id = x$id, file = ex_file)
})
run_meta_analysis_logic(target_files_exclusion, "WithExclusion")

message("\nAll Integrated Analyses Finalised.")