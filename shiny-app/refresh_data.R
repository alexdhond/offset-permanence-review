# ============================================================================
# refresh_data.R - One-Click Data Refresh Script
# ============================================================================
# Purpose: Automate the data update pipeline for the Shiny app
#
# This script:
# 1. Copies the source Excel database from the main project folder
# 2. Copies final CSV files from the main project's data/final folder
# 3. Converts all CSV/Excel files to RDS format
# 4. Regenerates db_summary_stats.rds
# 5. Updates the data timestamp
#
# Usage: Run this script whenever you update the raw data:
#   source("refresh_data.R")
#
# Prerequisites:
#   - offset-permanence-review folder should be at the same level as this app
#   - Or update SOURCE_PROJECT_PATH below to point to your data source
# ============================================================================

# ---- Configuration ----
# Path to the source project containing raw data
# Adjust this path if your folder structure is different
SOURCE_PROJECT_PATH <- ".."

# Source data locations
SOURCE_EXCEL <- file.path(SOURCE_PROJECT_PATH, "data", "raw", "offset_perm_rev_database.xlsx")
SOURCE_DERIVED_DIR <- file.path(SOURCE_PROJECT_PATH, "data", "final")

# Target data location (this app's data folder)
TARGET_DATA_DIR <- "data"

# ---- Helper Functions ----

#' Print a timestamped message
log_msg <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(...)))
}

#' Safely copy a file with error handling
safe_copy <- function(from, to, description = "") {
  if (!file.exists(from)) {
    log_msg("WARNING: Source file not found: ", from)
    return(FALSE)
  }
  tryCatch({
    file.copy(from, to, overwrite = TRUE)
    log_msg("Copied ", description, ": ", basename(from))
    return(TRUE)
  }, error = function(e) {
    log_msg("ERROR copying ", description, ": ", e$message)
    return(FALSE)
  })
}

# ---- Main Refresh Process ----

log_msg("Starting data refresh...")
log_msg("Source project: ", normalizePath(SOURCE_PROJECT_PATH, mustWork = FALSE))

# Check if source project exists
if (!dir.exists(SOURCE_PROJECT_PATH)) {
  stop(paste0(
    "Source project folder not found: ", SOURCE_PROJECT_PATH, "\n",
    "Please update SOURCE_PROJECT_PATH in refresh_data.R to point to your data source."
  ))
}

# Step 1: Copy source Excel database
log_msg("Step 1: Copying source Excel database...")
if (file.exists(SOURCE_EXCEL)) {
  safe_copy(
    SOURCE_EXCEL,
    file.path(TARGET_DATA_DIR, "offset_perm_rev_database.xlsx"),
    "source database"
  )
} else {
  log_msg("WARNING: Source Excel not found at: ", SOURCE_EXCEL)
}

# Step 2: Copy derived CSV files
log_msg("Step 2: Copying derived CSV files...")
if (dir.exists(SOURCE_DERIVED_DIR)) {
  derived_files <- list.files(SOURCE_DERIVED_DIR, pattern = "\\.(csv|xlsx)$", full.names = TRUE)
  if (length(derived_files) > 0) {
    for (f in derived_files) {
      safe_copy(f, file.path(TARGET_DATA_DIR, basename(f)), "derived file")
    }
  } else {
    log_msg("No CSV/Excel files found in derived folder")
  }
} else {
  log_msg("WARNING: Derived data folder not found: ", SOURCE_DERIVED_DIR)
}

# Step 3: Convert CSV/Excel to RDS
log_msg("Step 3: Converting files to RDS format...")
if (file.exists("data/convert_to_rds.R")) {
  tryCatch({
    source("data/convert_to_rds.R")
    log_msg("File conversion complete")
  }, error = function(e) {
    log_msg("ERROR during file conversion: ", e$message)
  })
} else {
  log_msg("WARNING: convert_to_rds.R not found, skipping conversion")
}

# Step 4: Regenerate summary statistics
log_msg("Step 4: Regenerating summary statistics...")
if (file.exists("compute_db_summary_stats.R")) {
  tryCatch({
    source("compute_db_summary_stats.R")
    log_msg("Summary statistics updated")
  }, error = function(e) {
    log_msg("ERROR computing summary stats: ", e$message)
  })
} else {
  log_msg("WARNING: compute_db_summary_stats.R not found, skipping stats generation")
}

# Step 5: Update timestamp file
log_msg("Step 5: Updating data timestamp...")
timestamp_file <- file.path(TARGET_DATA_DIR, "last_updated.txt")
writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), timestamp_file)
log_msg("Timestamp updated: ", timestamp_file)

# ---- Summary ----
log_msg("=" |> rep(60) |> paste0(collapse = ""))
log_msg("Data refresh complete!")
log_msg("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_msg("")
log_msg("Files in data folder:")
data_files <- list.files(TARGET_DATA_DIR, pattern = "\\.(rds|csv|xlsx)$")
for (f in data_files) {
  info <- file.info(file.path(TARGET_DATA_DIR, f))
  log_msg("  ", f, " (", format(info$mtime, "%Y-%m-%d %H:%M"), ")")
}
log_msg("=" |> rep(60) |> paste0(collapse = ""))
