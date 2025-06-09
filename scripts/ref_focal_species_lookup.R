# ================================================================
# Script:     ref_focal_species_lookup.R
# Date:       2025-06-09
# Author:     Alex Dhond
# Purpose:    Extract and standardize focal species entries from
#             the main database. Create a lookup table and identify
#             unmapped species for manual review.
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)
library(tidyverse)
library(janitor)
library(here)

# ---------------------------
# 2. Load and prepare data
# ---------------------------

data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

# ---------------------------
# 3. Extract and explode focal species
# ---------------------------

# Add row ID for tracking
data <- data %>%
  mutate(row_id = row_number())

# Explode semicolon-delimited species
species_exploded <- data %>%
  select(row_id, focal_species) %>%
  separate_rows(focal_species, sep = ";\\s*") %>%
  mutate(focal_species = str_trim(focal_species)) %>%
  filter(focal_species != "", !is.na(focal_species)) %>%
  distinct()

# ---------------------------
# 4. Build initial species lookup
# ---------------------------

# This is your working lookup table to standardize species names
species_lookup <- species_exploded %>%
  distinct(focal_species) %>%
  mutate(
    standard_common_name = NA_character_,
    standard_scientific_name = NA_character_,
    taxonomic_group = NA_character_,
    mapping_status = "unmapped"
  )

# ---------------------------
# 5. Optional: Example manual entries (to get started)
# ---------------------------

# You can manually fill some known mappings for testing
manual_mappings <- tribble(
  ~focal_species,        ~standard_common_name, ~standard_scientific_name, ~taxonomic_group,
  "lion",                "African Lion",        "Panthera leo",            "Mammal",
  "Panthera leo",        "African Lion",        "Panthera leo",            "Mammal",
  "elephant",            "African Elephant",    "Loxodonta africana",      "Mammal",
  "blue whale",          "Blue Whale",          "Balaenoptera musculus",   "Mammal"
)

# Join and update mapping info
species_lookup <- species_lookup %>%
  left_join(manual_mappings, by = "focal_species") %>%
  mutate(
    standard_common_name = coalesce(standard_common_name.y, standard_common_name.x),
    standard_scientific_name = coalesce(standard_scientific_name.y, standard_scientific_name.x),
    taxonomic_group = coalesce(taxonomic_group.y, taxonomic_group.x),
    mapping_status = if_else(!is.na(standard_scientific_name), "mapped", "unmapped")
  ) %>%
  select(focal_species, standard_common_name, standard_scientific_name, taxonomic_group, mapping_status)

# ---------------------------
# 6. Save outputs
# ---------------------------

output_dir <- here("data", "reference")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(species_lookup, file.path(output_dir, "focal_species_lookup.csv"))

message("✅ focal_species_lookup.csv written. Ready for manual review and use in cleaning script.")
