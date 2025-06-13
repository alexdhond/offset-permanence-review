# ================================================================
# Script:     clean_policy_type.R
# Date:       2025-06-11
# Author:     Alex Dhond
# Purpose:    Clean and standardize policy names and extract
#             classification info (type, status, jurisdiction, etc.)
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
# 3. Load policy reference table
# ---------------------------

policy_lookup <- read_csv(here("data", "reference", "offset_policy_lookup.csv")) %>%
  mutate(original_name = str_trim(original_name))

# ---------------------------
# 4. Explode and match policy entries
# ---------------------------

policy_cleaned <- raw_data %>%
  select(study_id, study_title, row_id,
         policy_legal_instrument_name,
         year_of_policy_adoption,
         policy_jurisdiction) %>%
  mutate(across(everything(), ~ str_split(.x, ";\\s*"))) %>%
  unnest_longer(policy_legal_instrument_name) %>%
  unnest_longer(year_of_policy_adoption) %>%
  unnest_longer(policy_jurisdiction) %>%
  mutate(across(everything(), str_trim)) %>%
  filter(!is.na(policy_legal_instrument_name), policy_legal_instrument_name != "") %>%
  
  # Join with lookup
  left_join(policy_lookup, by = c("policy_legal_instrument_name" = "original_name")) %>%
  mutate(valid_flag = !is.na(standardized_name))

# ---------------------------
# 5. Review unmapped policies
# ---------------------------

unmatched_policies <- policy_cleaned %>%
  filter(!valid_flag) %>%
  distinct(policy_legal_instrument_name) %>%
  arrange(policy_legal_instrument_name)

# ---------------------------
# 6. Export cleaned dataset
# ---------------------------

policy_cleaned_export <- policy_cleaned %>%
  select(
    study_id, study_title, row_id,
    policy_legal_instrument_name,
    standardized_name,
    policy_type, policy_type_standardized, policy_type_notes,
    jurisdiction_level, jurisdiction_level_standardized, jurisdiction_level_notes,
    jurisdiction_location, status, status_standardized, year_adopted,
    description
  )

output_dir <- here("data", "intermediate")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(policy_cleaned_export, file.path(output_dir, "policy_type_cleaned_long.csv"))
message("✅ Policy type cleaning complete. Output saved to 'data/intermediate'.")
