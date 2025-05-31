# ================================================================
# Script:     explode_database.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    explode database
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # Read Excel files
library(tidyverse) # Data manipulation (includes dplyr, tidyr)
library(here)      # Reproducible file paths
library(janitor)   # clean column names

# ---------------------------
# 2. Load raw data
# ---------------------------

data_raw <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

# ---------------------------
# 2. Explode multi-entry columns
# ---------------------------

data_exploded <- data_raw %>%
  separate_rows(country, sep = ";\\s*") %>%
  separate_rows(subnational_region, sep = ";\\s*") %>%
  separate_rows(subnational_region_type, sep = ";\\s*") %>%
  separate_rows(focal_species, sep = ";\\s*") %>%
  separate_rows(ecosystem_type_specific, sep = ";\\s*") %>%
  separate_rows(ecosystem_type_broad, sep = ";\\s*")

# ---------------------------
# 3. Load lookup tables
# ---------------------------

# Define paths

# ecosystem lookup path
eco_lookup_path <- here("data", "reference", "ecosystem_specific_to_broad_lookup.csv")

# country lookup path
country_lookup_path <- here("data", "reference", "global_country_region_lookup.csv")

# Read into R
lookup_eco <- read_csv(eco_lookup_path, show_col_types = FALSE)
lookup_country <- read_csv(country_lookup_path, show_col_types = FALSE)

# ---------------------------
# 4. ECOSYSTEM: Explode, join, and delete old column
# ---------------------------

data_cleaned <- data_raw %>%
  separate_rows(ecosystem_type_specific, sep = ";\\s*") %>%
  #filter(!is.na(ecosystem_type_specific) & ecosystem_type_specific != "") %>%
  select(-ecosystem_type_broad) %>%  # Drop old column if it exists
  left_join(lookup_eco, by = "ecosystem_type_specific") %>%  # Join to get Broad_Ecosystem
  rename(ecosystem_type_broad = broad_ecosystem) %>%  # Rename to match original column name
  relocate(ecosystem_type_broad, .after = ecosystem_type_specific)  # Move it beside the specific type

# check unmapped ecosystems
unmapped_ecosystems <- data_raw %>%
  separate_rows(ecosystem_type_specific, sep = ";\\s*") %>%
  distinct(ecosystem_type_specific) %>%
  filter(!ecosystem_type_specific %in% lookup_eco$ecosystem_type_specific)

