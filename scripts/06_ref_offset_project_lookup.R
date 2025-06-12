# ================================================================
# Script:     ref_offset_project_lookup.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Create a lookup table for standardizing project names
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # Read Excel files
library(tidyverse) # Data wrangling
library(here)      # Relative file paths
library(janitor)   # Clean column names

# ---------------------------
# 2. Load and prepare data
# ---------------------------

data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(row_id = row_number())

# ---------------------------
# 3. Explode project names
# ---------------------------

# Split and unnest project names
project_exploded <- data %>%
  select(study_id, row_id, offset_project_name) %>%
  mutate(project_name = str_split(offset_project_name, ";\\s*")) %>%
  unnest_longer(project_name) %>%
  mutate(project_name = str_trim(project_name)) %>%
  filter(!is.na(project_name), project_name != "")

# ---------------------------
# 4. Create initial lookup table
# ---------------------------

project_lookup <- project_exploded %>%
  distinct(project_name) %>%
  mutate(
    standard_project_name = NA_character_,
    mapping_status = "unmapped"
  )

# ---------------------------
# 5. Save lookup for manual mapping
# ---------------------------

# output_dir <- here("data", "reference")
# if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
# 
# write_csv(project_lookup, file.path(output_dir, "offset_project_name_lookup.csv"))
# 
# message("✅ Project name lookup table written to reference folder.")
