# ================================================================
# Script:     clean_ecosystem_types.R
# Date:       2025-06-11
# Author:     Alex Dhond
# Purpose:    Clean and standardize ecosystem_type_specific and 
#             match to broad_ecosystem using lookup table.
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

lookup_ecosystem <- read_csv(here("data", "reference", "ecosystem_specific_to_broad_lookup.csv"))

# ---------------------------
# 4. Explode and clean ecosystem columns
# ---------------------------

ecosystem_cleaned <- raw_data %>%
  select(study_id, study_title, row_id, ecosystem_type_specific) %>%
  separate_rows(ecosystem_type_specific, sep = ";\\s*") %>%
  mutate(ecosystem_type_specific = str_trim(ecosystem_type_specific)) %>%
  filter(!is.na(ecosystem_type_specific) & ecosystem_type_specific != "") %>%
  
  # Join to lookup to get broad ecosystem
  left_join(lookup_ecosystem, by = "ecosystem_type_specific") %>%
  mutate(
    valid_flag = if_else(!is.na(broad_ecosystem), TRUE, FALSE)
  )

# ---------------------------
# 5. Diagnostics: View unmatched values
# ---------------------------

unmatched_ecosystem <- ecosystem_cleaned %>%
  filter(!valid_flag) %>%
  distinct(ecosystem_type_specific) %>%
  arrange(ecosystem_type_specific)

# remove flags 
ecosystem_cleaned <- ecosystem_cleaned %>%
  select(!valid_flag)

# ---------------------------
# 6. Export cleaned data
# ---------------------------

output_dir <- here("data", "intermediate")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(ecosystem_cleaned, file.path(output_dir, "ecosystem_cleaned_long.csv"))

message("✅ Ecosystem cleaning complete. Cleaned and unmatched files written to 'data/intermediate'")
