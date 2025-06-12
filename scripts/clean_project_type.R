# ================================================================
# Script:     clean_project_type.R
# Date:       2025-06-11
# Author:     Alex Dhond
# Purpose:    Clean and standardize project_type_specific values,
#             match to intervention types, and flag unmapped terms.
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(here)

# ---------------------------
# 2. Load raw data
# ---------------------------

raw_data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(row_id = row_number())

# ---------------------------
# 3. Load lookup table
# ---------------------------

lookup_project_type <- read_csv(here("data", "reference", "project_intervention_type_lookup.csv")) %>%
  mutate(project_type_specific = str_to_lower(str_trim(project_type_specific)))

# ---------------------------
# 4. Explode and match project types
# ---------------------------

project_cleaned <- raw_data %>%
  select(study_id, study_title, row_id, project_type_specific) %>%
  separate_rows(project_type_specific, sep = ";\\s*") %>%
  mutate(project_type_specific = str_to_lower(str_trim(project_type_specific))) %>%
  filter(!is.na(project_type_specific) & project_type_specific != "") %>%
  
  # Join with lookup
  left_join(lookup_project_type, by = "project_type_specific") %>%
  mutate(valid_flag = !is.na(intervention_type))

# ---------------------------
# 5. Diagnostics: Unmapped terms
# ---------------------------

unmatched_project_types <- project_cleaned %>%
  filter(!valid_flag) %>%
  distinct(project_type_specific) %>%
  arrange(project_type_specific)

# ---------------------------
# 6. Export cleaned data
# ---------------------------

# delete flag
project_cleaned <- project_cleaned %>%
  select(!valid_flag)

output_dir <- here("data", "intermediate")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
write_csv(project_cleaned, file.path(output_dir, "project_type_cleaned_long.csv"))
message("✅ Project type cleaning complete. Files written to 'data/intermediate'")
