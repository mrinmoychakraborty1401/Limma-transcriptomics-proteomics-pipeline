# ==================================
# Data Annotation and Preparation
# ==================================
# PROTOCOL: STRICT PRESERVATION MODE
# 1. DO NOT REMOVE EXISTING FEATURES for "cleanliness" or "minimalism".
# 2. IF EDITING: Only modify the specific logic requested.
# 3. PRESERVE: All custom column parsing, MyGene caching, and metadata handling.
# ==================================
# This script cleans the input data based on user selections.
# UPDATED: Removed "Sync to Disk" logic. This script is now READ-ONLY for input configs.
# UPDATED: Implemented "Consume-on-Match" logic to prevent column collisions.
# UPDATED: Fixed CSV writing bug (using write.csv instead of write.table with quote=FALSE).
# UPDATED: Sanitize character columns to remove newlines before saving.
# UPDATED: REVERTED "Shift Detection" row dropping.
# UPDATED: IMPLEMENTED Smart Accession/GN extraction and column preservation.
# UPDATED: Fixed CSV Reading with SMART PRE-PROCESSING to handle unquoted commas in descriptions.
# UPDATED: Prevented valid GeneSymbol rows from dropping by protecting internal_id_col.

# --- Load Required Libraries ---
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
suppressPackageStartupMessages(
  pacman::p_load(readxl, dplyr, stringr, mygene, tools, data.table, jsonlite, rlang, argparse)
)

# --- Function to log messages to stderr ---
log_message <- function(...) {
  cat(paste0("PROGRESS:", ..., "\n"), file = stderr())
}

# --- Custom mean function to handle all-NA cases gracefully ---
mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(mean(x, na.rm = TRUE))
  }
}

# --- Get Command Line Arguments (UPDATED to argparse) ---
parser <- ArgumentParser()
parser$add_argument("--input", type="character", required=TRUE, help="Path to input file")
parser$add_argument("--output", type="character", required=TRUE, help="Output directory")
parser$add_argument("--config", type="character", required=TRUE, help="Path to config JSON")
parser$add_argument("--cache_dir", type="character", default="", help="Optional cache directory")

args <- parser$parse_args()

input_path <- args$input
output_dir <- args$output
config_path <- args$config
cache_dir_arg <- args$cache_dir

# --- Ensure Output Directory Exists ---
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
annotated_file_path <- file.path(output_dir, "annotated_quant.csv")

# --- FORCE REGENERATION ---
if (file.exists(annotated_file_path)) {
  log_message("Removing existing annotated_quant.csv to ensure clean overwrite.")
  unlink(annotated_file_path)
}

log_message("Starting Annotation...")

# --- Read configuration from the JSON file ---
if (!file.exists(config_path)) stop(paste("Config file not found:", config_path), call. = FALSE)
config <- jsonlite::fromJSON(config_path)
id_col <- config$idColumn
sample_config <- config$sampleConfig
meta_cols <- config$metadataColumns

# --- DEDUPLICATE CONFIGURATION ---
if (is.data.frame(sample_config)) {
  initial_rows <- nrow(sample_config)
  sample_config <- unique(sample_config)
  if (nrow(sample_config) < initial_rows) {
    log_message(paste("Removed", initial_rows - nrow(sample_config), "duplicate entries from sample configuration."))
  }
}

# --- READ DATA (Standardized) ---
data <- tryCatch({
  ext <- tolower(tools::file_ext(input_path))
  if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(input_path, .name_repair = "minimal")
  } else {
    # ---------------------------------------------------------
    # SMART CSV REPAIR SYSTEM (NEW)
    # ---------------------------------------------------------
    # Problem: Descriptions like "subunit beta, mitochondrial" without quotes cause column shifting.
    # Solution: Detect row vs header comma imbalance and substitute ", " with "; ".
    
    log_message("Reading text data...")
    raw_lines <- readLines(input_path, warn = FALSE)
    
    # Filter out empty lines to prevent analysis skewing
    raw_lines <- raw_lines[raw_lines != ""]
    
    if (length(raw_lines) > 0) {
      header_line <- raw_lines[1]
      
      # Use stringr for reliable counting
      header_comma_count <- stringr::str_count(header_line, ",")
      
      # Check a subset of body rows (max 100) to detect shifts
      # We skip the header (index 1)
      check_limit <- min(100, length(raw_lines))
      if (check_limit > 1) {
        body_sample <- raw_lines[2:check_limit]
        body_comma_counts <- stringr::str_count(body_sample, ",")
        
        # Heuristic: If we find rows with MORE commas than the header, 
        # it implies unquoted text fields containing commas.
        if (any(body_comma_counts > header_comma_count)) {
          log_message("Detected potential column shift due to unquoted commas in descriptions.")
          log_message("Applying Smart Fix: Substituting ', ' with '; ' in text fields...")
          
          # Substitute ", " (comma space) with "; " (semicolon space)
          # This targets text like "subunit beta, mitochondrial" -> "subunit beta; mitochondrial"
          # It preserves the structure but removes the separator ambiguity.
          raw_lines <- gsub(", ", "; ", raw_lines)
          
          # Re-read using the corrected text
          read.csv(text = raw_lines, check.names = FALSE, stringsAsFactors = FALSE, quote = "\"", comment.char = "")
        } else {
          # No obvious shift detected, proceed with raw lines to save I/O
          read.csv(text = raw_lines, check.names = FALSE, stringsAsFactors = FALSE, quote = "\"", comment.char = "")
        }
      } else {
        # File too short for heuristics, just read it
        read.csv(text = raw_lines, check.names = FALSE, stringsAsFactors = FALSE, quote = "\"", comment.char = "")
      }
    } else {
      stop("Input file is empty.")
    }
  }
}, error = function(e) {
  stop("Error reading data file: ", e$message, call. = FALSE)
})

# Ensure it's a standard dataframe
data <- as.data.frame(data)

# CRITICAL FIX: Sanitize column names immediately.
# This ensures that we are matching against R-safe names (e.g. dots instead of spaces/hyphens)
colnames(data) <- make.names(colnames(data), unique = TRUE)
available_cols <- colnames(data)

# --- LOAD SESSION FILE (Fall-back / Source of Truth) ---
session_file_path <- paste0(input_path, ".session.json")
session_column_map <- NULL

if (file.exists(session_file_path)) {
  log_message(paste("Found session file:", session_file_path))
  tryCatch({
    session_data <- jsonlite::fromJSON(session_file_path)
    if (!is.null(session_data$columnConfig)) {
      session_column_map <- session_data$columnConfig
      log_message("Loaded column configuration from session file.")
    }
  }, error = function(e) {
    log_message(paste("Warning: Failed to parse session file:", e$message))
  })
}

# ==============================================================================
# REWRITTEN COLUMN MAPPING LOGIC (ROBUST & CONSUME-ON-MATCH)
# ==============================================================================

# Helper: Normalization function (strip non-alphanumeric, lowercase)
clean_name <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x[is.na(x)] <- ""
  # Force ASCII to handle unicode
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  tolower(gsub("[^A-Za-z0-9]", "", x))
}

# The Master Matcher
# Now accepts a 'pool' of columns to search in, to support consumption
find_column_in_pool <- function(target_name, pool_cols, strict = FALSE) {
  if (length(target_name) == 0 || is.na(target_name)) return(NA)
  
  # 1. Exact Match
  if (target_name %in% pool_cols) return(target_name)
  
  # 2. Normalized Match
  norm_target <- clean_name(target_name)
  norm_available <- clean_name(pool_cols)
  match_idx <- which(norm_available == norm_target)
  
  # Strict mode: Only accept if ONE match is found (avoids ambiguity for ID col)
  if (strict && length(match_idx) > 1) {
    log_message(paste("Warning: Multiple matches found for strict search of", target_name, "- Taking first."))
  }
  
  if (length(match_idx) > 0) return(pool_cols[match_idx[1]])
  
  return(NA)
}

# --- Resolve ID Column (STRICT FIRST) ---
# FIX: Prioritize exact match for ID column to prevent accidental overwrites
log_message(paste("Resolving ID column based on selection:", id_col))

# 1. Try exact match in available columns
if (id_col %in% available_cols) {
  resolved_id <- id_col
  log_message(paste("  Exact match found for ID column:", resolved_id))
} else {
  # 2. Try sanitized match (R make.names often changes ' ' to '.')
  sanitized_id <- make.names(id_col)
  if (sanitized_id %in% available_cols) {
    resolved_id <- sanitized_id
    log_message(paste("  Sanitized match found for ID column:", resolved_id))
  } else {
    # 3. Fallback to fuzzy match ONLY if exact fails
    resolved_id <- find_column_in_pool(id_col, available_cols)
    if (is.na(resolved_id)) {
      stop(paste("Error: ID Column '", id_col, "' not found in input file."), call. = FALSE)
    }
    log_message(paste("  Fuzzy match found for ID column:", resolved_id))
  }
}

# IMPORTANT: Remove ID column from pool so samples don't accidentally grab it
remaining_pool <- available_cols[available_cols != resolved_id]

# --- Resolve Sample Columns ---
resolved_original_names <- character(nrow(sample_config))
missing_samples <- c()

log_message("Resolving sample columns...")

for (i in 1:nrow(sample_config)) {
  orig <- sample_config$originalName[i]
  final <- sample_config$finalName[i]
  
  # Strategy 1: Match by Original Name (Header)
  match <- find_column_in_pool(orig, remaining_pool)
  
  # Strategy 2: Match by Final Name (User Assigned)
  if (is.na(match)) {
    match <- find_column_in_pool(final, remaining_pool)
  }
  
  # Strategy 3: Consult Session File
  if (is.na(match) && !is.null(session_column_map)) {
    if (orig %in% names(session_column_map) && orig %in% remaining_pool) {
      match <- orig
    } else {
      # Reverse lookup: Find key where value$finalName == final
      found_key <- NULL
      for (key in names(session_column_map)) {
        if (identical(session_column_map[[key]]$finalName, final)) {
          found_key <- key
          break
        }
      }
      if (!is.null(found_key) && found_key %in% remaining_pool) {
        match <- found_key
        log_message(paste0("    Recovered mapping from Session File: '", final, "' -> '", match, "'"))
      }
    }
  }
  
  if (!is.na(match)) {
    resolved_original_names[i] <- match
    # CRITICAL: Remove matched column from pool so it can't be reused by another sample
    remaining_pool <- remaining_pool[remaining_pool != match]
  } else {
    missing_samples <- c(missing_samples, orig)
  }
}

if (length(missing_samples) > 0) {
  log_message("--- DIAGNOSTICS: COLUMN MISMATCH ---")
  log_message(paste("File has", length(available_cols), "columns."))
  log_message(paste("First 10 File Columns (Sanitized):", paste(head(available_cols, 10), collapse=", ")))
  log_message(paste("Missing Config Keys (All):", paste(missing_samples, collapse=", ")))
  stop(paste0("Error: Could not find ", length(missing_samples), " sample columns in the input file. First missing: '", missing_samples[1], "'"), call. = FALSE)
}

# Apply the resolved names back to config in memory
sample_config$originalName <- resolved_original_names

# --- CONFIG PRESERVATION ---
# We DO NOT write back to config_path to avoid corrupting the source file.
# The resolved logic is only used in-memory for this run.
log_message("Configuration resolved in-memory. Original config file preserved.")

# ID Column is already resolved above
id_col <- resolved_id

# --- Resolve Metadata Columns ---
if (length(meta_cols) > 0) {
  resolved_meta <- c()
  for (m in meta_cols) {
    match <- find_column_in_pool(m, available_cols)
    if (!is.na(match)) resolved_meta <- c(resolved_meta, match)
  }
  meta_cols <- resolved_meta
}

# Re-construct the full list of columns to keep using the REAL names
all_user_cols <- c(unlist(id_col), unlist(sample_config$originalName), unlist(meta_cols))
all_user_cols <- unique(all_user_cols)

# --- AUTO-DETECT & PRESERVE DESCRIPTION COLUMN ---
potential_desc_candidates <- c("Description", "Protein.names", "Protein.Names", "Protein.Name", "Fasta.headers", "Fasta.header", "Name", "Title")
# Find intersection with sanitized column names
found_extra_desc <- intersect(available_cols, potential_desc_candidates)[1]

if (!is.null(found_extra_desc) && !is.na(found_extra_desc) && !(found_extra_desc %in% all_user_cols)) {
  log_message(paste("Auto-preserving description column for parsing:", found_extra_desc))
  all_user_cols <- c(all_user_cols, found_extra_desc)
}

# --- Select and prepare data subset ---
data_subset <- data %>% dplyr::select(dplyr::all_of(all_user_cols))

# --- RENAME SAMPLES ---
# Map the RESOLVED original columns back to the FINAL user names (486-Control3)
old_sample_names <- sample_config$originalName
new_sample_names <- sample_config$finalName
current_colnames <- names(data_subset)
indices_to_rename <- match(old_sample_names, current_colnames)

if (any(is.na(indices_to_rename))) {
  stop("Error: Internal column mapping mismatch during renaming.", call. = FALSE)
}

current_colnames[indices_to_rename] <- new_sample_names
names(data_subset) <- current_colnames

final_sample_cols <- sample_config$finalName
internal_id_col <- "InternalProteinID_XYZ"
names(data_subset)[names(data_subset) == id_col] <- internal_id_col

# === GENE SYMBOL MAPPING AND FILTERING ===

potential_desc_cols <- c("Description", "Protein.names", "Protein.Names", "Protein.Name", "Fasta.headers", "Fasta.header", "Name", "Title")
found_desc_col <- intersect(names(data_subset), potential_desc_cols)[1]

if (!is.na(found_desc_col)) {
  log_message(paste("Found description column:", found_desc_col, "- Parsing Gene Symbols (GN=) and Accessions."))
  source_col_for_parsing <- as.character(data_subset[[found_desc_col]])
} else {
  log_message("Did not find a standard Description column. Parsing from the selected ID column.")
  source_col_for_parsing <- as.character(data_subset[[internal_id_col]])
}

log_message("Attempting to parse protein accessions and gene names...")

# 0. Clean the strings (remove BOMs, hidden chars, leading whitespace)
source_col_for_parsing <- gsub("^\\s+|\\s+$|[^[:print:]]", "", source_col_for_parsing)

# --- NEW: ROBUST ID & GENE PARSING ---

# 1. Parse Accession ID (UniProt, RefSeq, Ensembl)
uniprot_regex <- "[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}"
refseq_regex <- "[NM]_[0-9]+(\\.[0-9]+)?"
ensembl_regex <- "ENS[GTP][0-9]+"
combined_id_regex <- paste0("(", uniprot_regex, "|", refseq_regex, "|", ensembl_regex, ")")

# Strategy A: Embedded in pipes (sp|P12345|...)
acc_pipe <- str_match(source_col_for_parsing, "\\|([A-Z0-9]+)\\|")[, 2]
# Strategy B: At start of string
acc_start <- str_extract(source_col_for_parsing, paste0("^", combined_id_regex))
# Strategy C: Anywhere in string
acc_any <- str_extract(source_col_for_parsing, combined_id_regex)

data_subset$Extracted_Accession <- dplyr::coalesce(acc_pipe, acc_start, acc_any)
# Clean versions (remove isoform suffixes for mapping)
data_subset$BaseAcc <- gsub("-\\d+$", "", data_subset$Extracted_Accession)

log_message(paste("Extracted", sum(!is.na(data_subset$Extracted_Accession)), "Accession IDs."))

# 2. Parse Gene Name (GN=...)
gene_regex <- "(?:GN|Gene|Gene_Name|Name|Symbol)\\s*=\\s*([^\\s,;\\]\\)]+)"
gene_matches <- str_match(source_col_for_parsing, regex(gene_regex, ignore_case = TRUE))
data_subset$Extracted_GN <- gene_matches[, 2]

log_message(paste("Extracted", sum(!is.na(data_subset$Extracted_GN)), "Gene Names via 'GN=' tag."))

# 3. Smart Symbol Assignment
# If GeneSymbol column already exists and has data, TRUST IT FIRST.
if (!"GeneSymbol" %in% colnames(data_subset)) {
  # FIX: If the user selected 'GeneSymbol' or 'Symbol' as their ID, it is currently protected in internal_id_col. Restore it!
  if (tolower(id_col) %in% c("genesymbol", "symbol", "gene.symbol", "gene_symbol")) {
    data_subset$GeneSymbol <- data_subset[[internal_id_col]]
  } else {
    data_subset$GeneSymbol <- NA
  }
}

# Logic:
# 1. If GeneSymbol is valid, keep it.
# 2. If GeneSymbol missing, use Extracted_GN.
# 3. If both missing, use BaseAcc for lookup.

missing_mask <- is.na(data_subset$GeneSymbol) | data_subset$GeneSymbol == "" | data_subset$GeneSymbol == "NA"

# Fill from GN= where missing
if (any(missing_mask)) {
  gn_fill_mask <- missing_mask & !is.na(data_subset$Extracted_GN)
  data_subset$GeneSymbol[gn_fill_mask] <- data_subset$Extracted_GN[gn_fill_mask]
  log_message(paste("Filled", sum(gn_fill_mask), "missing symbols using extracted GN= values."))
}

# Update mask
missing_mask <- is.na(data_subset$GeneSymbol) | data_subset$GeneSymbol == ""

# Fill from Accession Mapping where still missing
if (any(missing_mask)) {
  log_message("Mapping remaining missing symbols using Extracted Accession IDs...")
  
  data_subset$ID_to_map <- NA
  data_subset$ID_to_map[missing_mask] <- data_subset$BaseAcc[missing_mask]
  
  ids_to_query <- unique(na.omit(data_subset$ID_to_map))
  
  if (length(ids_to_query) > 0) {
    if (length(cache_dir_arg) > 0 && nchar(cache_dir_arg) > 0) {
      cache_dir <- cache_dir_arg
    } else { 
      cache_dir <- file.path(output_dir, ".mygene_cache")
    }
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    cache_file <- file.path(cache_dir, "mygene_simple_id_cache.csv") 
    
    cached_mappings <- if(file.exists(cache_file)) read.csv(cache_file, colClasses=c("ID_to_map"="character", "MappedSymbol"="character")) else data.frame(ID_to_map=character(), MappedSymbol=character())
    
    # Check cache first
    uncached_ids <- setdiff(ids_to_query, cached_mappings$ID_to_map)
    
    if(length(uncached_ids) > 0) {
      log_message(paste("Querying MyGene for", length(uncached_ids), "IDs..."))
      mg_res <- tryCatch({
        queryMany(uncached_ids, scopes = c("uniprot", "accession", "refseq", "ensembl.protein"), fields = "symbol", species = "human")
      }, error = function(e) NULL)
      
      if (!is.null(mg_res)) {
        new_maps <- as.data.frame(mg_res) %>% 
          dplyr::select(query, symbol) %>% 
          dplyr::rename(ID_to_map = query, MappedSymbol = symbol) %>%
          dplyr::filter(!is.na(MappedSymbol)) %>%
          dplyr::distinct(ID_to_map, .keep_all=TRUE)
        
        cached_mappings <- bind_rows(cached_mappings, new_maps) %>% dplyr::distinct(ID_to_map, .keep_all=TRUE)
        write.csv(cached_mappings, cache_file, row.names=FALSE)
      }
    }
    
    # Apply mappings
    lookup <- cached_mappings[cached_mappings$ID_to_map %in% ids_to_query, ]
    match_idx <- match(data_subset$ID_to_map, lookup$ID_to_map)
    update_mask <- missing_mask & !is.na(match_idx)
    data_subset$GeneSymbol[update_mask] <- lookup$MappedSymbol[match_idx[update_mask]]
    log_message(paste("Mapped", sum(update_mask), "IDs to Gene Symbols."))
  }
}

# --- Final Filtering ---
original_rows <- nrow(data_subset)
data_subset <- data_subset %>% dplyr::filter(!is.na(GeneSymbol) & GeneSymbol != "")
filtered_rows <- nrow(data_subset)

log_message(paste("Final valid proteins:", filtered_rows, "(from", original_rows, "initial rows)."))

if (filtered_rows == 0) {
  stop("Fatal Error: No proteins could be mapped to a valid Gene Symbol.", call. = FALSE)
}

log_message("Converting sample columns to numeric...")
data_subset <- data_subset %>%
  dplyr::mutate(across(all_of(final_sample_cols), 
                       ~ suppressWarnings(as.numeric(gsub(",", "", as.character(.))))))

log_message("Averaging abundances by Gene Symbol...")
# Ensure we keep Extracted_Accession in the final output
meta_info_cols <- c("GeneSymbol", internal_id_col, "Extracted_Accession", unlist(meta_cols))
meta_info <- data_subset %>%
  dplyr::select(dplyr::all_of(intersect(meta_info_cols, names(data_subset)))) %>%
  dplyr::distinct(GeneSymbol, .keep_all = TRUE)

data_avg <- data_subset %>%
  dplyr::group_by(GeneSymbol) %>%
  dplyr::summarise(across(all_of(final_sample_cols), mean_or_na), .groups = "drop")

final_data <- dplyr::left_join(meta_info, data_avg, by = "GeneSymbol")

# Rename internal ID back to original
if (id_col != "GeneSymbol") {
  names(final_data)[names(final_data) == internal_id_col] <- id_col
} else {
  # If the ID column was already GeneSymbol, drop the temporary duplicate
  final_data[[internal_id_col]] <- NULL
}

# --- REORDER COLUMNS: ID, Accession, Symbol ---
cols_order <- unique(c(id_col, "Extracted_Accession", "GeneSymbol", unlist(meta_cols), final_sample_cols))
cols_order <- intersect(cols_order, names(final_data))
final_data <- final_data %>% dplyr::select(all_of(cols_order))

# --- Save main data file ---
character_cols <- sapply(final_data, is.character)
final_data[character_cols] <- lapply(final_data[character_cols], function(x) gsub("[\r\n]", " ", x))

write.csv(final_data, annotated_file_path, row.names = FALSE)

log_message("✅ Annotation and data preparation complete.")