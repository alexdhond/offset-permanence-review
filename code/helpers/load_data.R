# =============================================================================
# Data Loading Script for Offset Permanence Review Analysis
# =============================================================================
# This script loads all required packages and datasets used across multiple
# analysis documents. Source this at the beginning of QMD files to avoid
# code duplication.
# =============================================================================

# Load Required Packages --------------------------------------------------

library(tidyverse)       # Data manipulation and visualization
library(here)            # Easy file path management
library(janitor)         # Clean column names
library(readxl)          # Reading Excel files
library(countrycode)     # Geospatial country codes
library(sf)              # Geospatial data manipulation
library(rnaturalearth)   # Natural Earth map data
library(rnaturalearthdata) # Natural Earth datasets
library(RColorBrewer)    # Color palettes
library(knitr)           # Document knitting
library(gt)              # Grammar of tables
library(purrr)           # Functional programming tools
library(viridis)         # Color scales
library(scales)          # Formatting scales
library(glue)            # String interpolation
library(colorspace)      # Color space manipulation


# Set Global Options ------------------------------------------------------

# Configure knitr chunk options for consistent output
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)


# Load Primary Dataset ----------------------------------------------------

# Load the cleaned, long-format final dataset
final_df <- read_csv(
  here("data", "final", "offset_perm_rev_long_cleaned.csv"),
  guess_max = 10000,
  col_types = cols(
    species_common_name     = col_character(),
    species_scientific_name = col_character(),
    species_taxonomic_group = col_character(),
    .default = col_guess()
  )
)

# Ensure every study_title has a unique study_id
# (Creates deterministic IDs based on factor levels)
final_df <- final_df %>%
  mutate(study_id = paste0("id_", as.integer(factor(study_title))))


# Load Reference Data -----------------------------------------------------

# Load permanence risk typology lookup table
risk_typology <- read_csv(
  here("data", "lookups", "permanence_risk_typology_lookup.csv")
) %>%
  janitor::clean_names() %>%
  select(
    domain = broad,
    category = specific,
    type = sub_risk
  )


# Calculate Summary Statistics --------------------------------------------

# Count total unique studies in dataset (used in multiple analyses)
total_unique_studies <- final_df %>%
  distinct(study_id) %>%
  nrow()


# Load Cached World Map Data (if available) -------------------------------

# Check if cached world map exists, otherwise create it
world_map_cache_path <- here("data", "cache", "world_map_cached.rds")

if (file.exists(world_map_cache_path)) {
  # Load cached version for faster rendering
  world_map <- readRDS(world_map_cache_path)
  message("Loaded cached world map from: ", world_map_cache_path)
} else {
  # Fetch from rnaturalearth and cache for future use
  world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Save to cache
  saveRDS(world_map, world_map_cache_path)
  message("Created and cached world map to: ", world_map_cache_path)
}


# Confirmation Message ----------------------------------------------------

message("Data loading complete!")
message("  - final_df: ", nrow(final_df), " rows")
message("  - Unique studies: ", total_unique_studies)
message("  - Risk typology: ", nrow(risk_typology), " risk types")
