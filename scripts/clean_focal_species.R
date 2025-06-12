# ================================================================
# Script:     clean_focal_species.R
# Date:       2025-06-11
# Author:     Alex Dhond
# Purpose:    Clean and standardize focal species entries using
#             validated lookup table.
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(here)

# ---------------------------
# 2. Load raw data and lookup
# ---------------------------

raw_data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(row_id = row_number())

species_lookup <- read_csv(here("data", "reference", "focal_species_lookup.csv"))

# ---------------------------
# 3. Explode and clean focal species
# ---------------------------

species_cleaned <- raw_data %>%
  select(study_id, study_title, row_id, focal_species) %>%
  separate_rows(focal_species, sep = ";\\s*") %>%
  mutate(focal_species = str_trim(focal_species)) %>%
  filter(!is.na(focal_species) & focal_species != "") %>%
  
  # Normalize known synonyms or variants 
  mutate(focal_species = case_when(
    focal_species == "wood frog (Rana sylvatica)" ~ "wood frog (Lithobates sylvaticus)",
    focal_species == "red-tailed black-cockatoo (Calyptorhynchus banksii graptogyne)" ~ 
      "red-tailed black-cockatoo (Calyptorhynchus banksii)",
    TRUE ~ focal_species
  )) %>%
  
  # Join to standardized species lookup
  left_join(species_lookup, by = "focal_species") %>%
  mutate(
    valid_flag = if_else(mapping_status == "mapped", TRUE, FALSE)
  )

# ---------------------------
# 4. Diagnostics
# ---------------------------

species_valid   <- species_cleaned %>% filter(valid_flag)
species_invalid <- species_cleaned %>% filter(!valid_flag)

# count unmatched species
# species_invalid %>% count(focal_species, sort = TRUE)

# remove mapping and flag before export
species_cleaned <- species_cleaned %>%
  select(!mapping_status:valid_flag)

# ---------------------------
# 5. Export cleaned outputs
# ---------------------------

output_dir <- here("data", "intermediate")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(species_cleaned, file.path(output_dir, "focal_species_cleaned_long.csv"))


message("✅ Focal species data cleaned and exported.")
