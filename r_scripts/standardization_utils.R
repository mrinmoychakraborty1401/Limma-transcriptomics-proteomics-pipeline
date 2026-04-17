# Load required libraries
library(dplyr)
library(limma)
library(stringr)

# ==============================================================================
# Function 1: Clean and Standardize Gene Symbols (Global Source of Truth)
# ==============================================================================
clean_gene_symbols <- function(df, symbol_col) {
  message("Starting gene symbol cleaning...")
  
  # Ensure column exists
  if (!symbol_col %in% colnames(df)) {
    stop(paste("Column", symbol_col, "not found in dataframe."))
  }
  
  # 1. Handle "///" and " // " - Keep FIRST entry (Primary Target)
  # Uses regex " /// | // " to match either separator (handles GPL570 and GPL17586)
  df[[symbol_col]] <- sapply(strsplit(as.character(df[[symbol_col]]), " /// | // "), `[`, 1)
  df[[symbol_col]] <- trimws(df[[symbol_col]])
  
  # Filter out empty or NA
  df <- df[!is.na(df[[symbol_col]]) & df[[symbol_col]] != "", ]
  
  # 2. SOURCE OF TRUTH: Convert Aliases to Official Symbols using limma
  # This uses the HGNC / NCBI human gene index to force synonyms to the official standard.
  # This ensures "p53" becomes "TP53" across ALL datasets.
  unique_symbols <- unique(df[[symbol_col]])
  
  # alias2SymbolTable is the robust vector-based function
  official_symbols <- alias2SymbolTable(unique_symbols, species = "Hs")
  
  # Create mapping dataframe
  map_df <- data.frame(
    Original = unique_symbols, 
    Official = official_symbols, 
    stringsAsFactors = FALSE
  )
  
  # Fallback: if mapping returns NA (gene too new or unknown), keep original
  # This prevents losing genes that limma doesn't know about yet.
  map_df$Official[is.na(map_df$Official)] <- map_df$Original[is.na(map_df$Official)]
  
  # 3. Merge clean symbols back
  df$Old_Symbol <- df[[symbol_col]] # Keep record for debugging
  df[[symbol_col]] <- map_df$Official[match(df[[symbol_col]], map_df$Original)]
  
  # 4. Final Cleanup: Remove NAs that might have resulted from mapping failures
  df <- df[!is.na(df[[symbol_col]]) & df[[symbol_col]] != "", ]
  
  message(paste("Cleaned", nrow(df), "rows. Gene symbols standardized to Official HGNC."))
  return(df)
}

# ==============================================================================
# Function 2: Standardize Metadata (TNBC vs Normal)
# ==============================================================================
standardize_metadata <- function(pdata, dataset_id, mapping_csv = "master_mapping.csv") {
  
  if (!file.exists(mapping_csv)) {
    warning("master_mapping.csv not found in working directory. Returning original metadata.")
    pdata$Condition <- "Other"
    return(pdata)
  }
  
  mappings <- read.csv(mapping_csv, stringsAsFactors = FALSE)
  rule <- mappings[mappings$dataset_id == dataset_id, ]
  
  if (nrow(rule) == 0) {
    warning(paste("No mapping rule found for", dataset_id))
    pdata$Condition <- "Other"
    return(pdata)
  }
  
  # Helper to find columns loosely (exact or case-insensitive)
  find_col <- function(df, col_hint) {
    if (col_hint %in% colnames(df)) return(col_hint)
    match_idx <- grep(col_hint, colnames(df), ignore.case = TRUE, fixed = FALSE)
    if (length(match_idx) > 0) return(colnames(df)[match_idx[1]])
    return(NULL)
  }
  
  tnbc_col <- find_col(pdata, rule$tnbc_column)
  norm_col <- find_col(pdata, rule$normal_column)
  
  pdata$Condition <- "Other"
  
  # Apply Normal Logic first
  if (!is.null(norm_col)) {
    hits <- grepl(rule$normal_value, pdata[[norm_col]], ignore.case = TRUE)
    pdata$Condition[hits] <- "Normal"
  }
  
  # Apply TNBC Logic (overwrites Normal if there's a conflict, though unlikely)
  if (!is.null(tnbc_col)) {
    hits <- grepl(rule$tnbc_value, pdata[[tnbc_col]], ignore.case = TRUE)
    pdata$Condition[hits] <- "TNBC"
  }
  
  # Report
  tbl <- table(pdata$Condition)
  message(paste("Dataset", dataset_id, "Standardized:"))
  print(tbl)
  
  return(pdata)
}