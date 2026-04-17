# === Load Required Libraries ===
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tools, jsonlite, readxl, data.table)

# === Get Command Line Arguments ===
args <- commandArgs(trailingOnly = TRUE)
# Handle flag parsing manually to be robust
input_path_idx <- grep("--input", args)
if (length(input_path_idx) > 0) {
  input_path <- args[input_path_idx + 1]
} else {
  # Fallback to first argument if flag not found (for direct calls)
  input_path <- args[1]
}

if (length(input_path) == 0 || is.na(input_path) || !file.exists(input_path)) {
  # Clean potential quotes just in case
  if(!is.na(input_path)) input_path <- gsub('^"|"$', "", input_path)
  
  if (length(input_path) == 0 || is.na(input_path) || !file.exists(input_path)) {
    stop("Input file not found or path not provided.", call. = FALSE)
  }
}

# --- Use the correct reader based on file extension ---
headers <- tryCatch({
  ext <- tolower(tools::file_ext(input_path))
  if (ext %in% c("xlsx", "xls")) {
    # Use readxl for Excel files, read only the first row to get headers
    # .name_repair = "minimal" allows us to see raw headers initially
    names(readxl::read_excel(input_path, n_max = 0, .name_repair = "minimal"))
  } else {
    # Use read.csv for text-based files
    # check.names = FALSE gets raw headers initially
    names(read.csv(input_path, nrows = 1, check.names = FALSE))
  }
}, error = function(e) {
  stop("Error reading file headers: ", e$message, call. = FALSE)
})

# FIX: Sanitize column names to be R-compatible. This ensures that what the user
# sees and selects in the UI exactly matches what the analysis script will see after
# loading the data, preventing errors with special characters like '#', ':', or spaces.
# This UNIFIES the method: UI configuration keys will always match R internal names.
sanitized_headers <- make.names(headers, unique = TRUE)

# Output sanitized headers as a JSON array
cat(jsonlite::toJSON(sanitized_headers))