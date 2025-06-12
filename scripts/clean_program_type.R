# ================================================================
# Script:     clean_program_type.R
# Date:       2025-06-11
# Author:     Alex Dhond
# Purpose:    Clean and standardize offset program names and
#             extract classification metadata (type, mechanism, etc.)
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
# 3. Load reference lookup
# ---------------------------

program_lookup <- read_csv(here("data", "reference", "offset_program_lookup.csv")) %>%
  mutate(original_name = str_trim(original_name))

# ---------------------------
# 4. Explode and clean program entries
# ---------------------------

program_cleaned <- raw_data %>%
  select(study_id, study_title, row_id, offset_program_name) %>%
  separate_rows(offset_program_name, sep = ";\\s*") %>%
  mutate(offset_program_name = str_trim(offset_program_name)) %>%
  filter(!is.na(offset_program_name) & offset_program_name != "") %>%
  
  # Join to lookup
  left_join(program_lookup, by = c("offset_program_name" = "original_name")) %>%
  mutate(valid_flag = !is.na(standardized_name))

# ---------------------------
# 5. Diagnostics: Unmatched Programs
# ---------------------------

unmatched_programs <- program_cleaned %>%
  filter(!valid_flag) %>%
  distinct(offset_program_name) %>%
  arrange(offset_program_name)

# ---------------------------
# 6. Export Cleaned Data
# ---------------------------

program_cleaned_export <- program_cleaned %>%
  select(study_id, study_title, row_id,
         offset_program_name,
         standardized_name,
         program_type, mechanism_type, status,
         scope_level, scope_location, related_program, note)

output_dir <- here("data", "intermediate")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(program_cleaned_export, file.path(output_dir, "program_type_cleaned_long.csv"))
message("✅ Program type cleaning complete. Output written to 'data/intermediate'.")
