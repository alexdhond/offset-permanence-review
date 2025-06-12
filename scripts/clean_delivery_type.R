# ================================================================
# Script:     clean_delivery_type.R
# Date:       2025-06-11
# Author:     Alex Dhond
# Purpose:    Clean and standardize offset delivery types
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

lookup_delivery_type <- read_csv(here("data", "reference", "offset_delivery_type_lookup.csv")) %>%
  select(raw_delivery_type, standardized_delivery_type) %>%
  mutate(raw_delivery_type = str_trim(raw_delivery_type))

# ---------------------------
# 4. Explode and clean
# ---------------------------

delivery_cleaned <- raw_data %>%
  select(study_id, study_title, row_id, offset_delivery_type) %>%
  separate_rows(offset_delivery_type, sep = ";\\s*") %>%
  mutate(raw_delivery_type = str_trim(offset_delivery_type)) %>%
  filter(!is.na(raw_delivery_type) & raw_delivery_type != "") %>%
  left_join(lookup_delivery_type, by = "raw_delivery_type") %>%
  mutate(valid_flag = !is.na(standardized_delivery_type))

# ---------------------------
# 5. Diagnostics: Unmapped values
# ---------------------------

unmapped_delivery_types <- delivery_cleaned %>%
  filter(!valid_flag) %>%
  distinct(raw_delivery_type) %>%
  arrange(raw_delivery_type)

# Optionally write to CSV for review
# write_csv(unmapped_delivery_types, here("data", "intermediate", "unmapped_delivery_types.csv"))

# ---------------------------
# 6. Save cleaned output
# ---------------------------

delivery_cleaned_final <- delivery_cleaned %>%
  select(study_id, study_title, row_id, raw_delivery_type, standardized_delivery_type)

output_dir <- here("data", "intermediate")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(delivery_cleaned_final, file.path(output_dir, "offset_delivery_type_cleaned_long.csv"))
message("✅ Offset delivery type cleaning complete. File saved to 'data/intermediate'")
