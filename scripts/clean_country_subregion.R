# ===============================================
# Script:     clean_country_subregion.R
# Date:       2025-06-11
# Author:     Alex Dhond
# Purpose:    Clean and standardize country + subregion entries 
#             using validated lookup and alias tables.
# ===============================================

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
# 3. Load lookup and alias tables
# ---------------------------

validated_lookup <- read_csv(here("data", "reference", "validated_country_region_lookup.csv"))
country_aliases  <- read_csv(here("data", "reference", "country_aliases.csv"))
region_aliases   <- read_csv(here("data", "reference", "region_aliases.csv"))

# ---------------------------
# 4. Explode and standardize input values
# ---------------------------

country_cleaned <- raw_data %>%
  select(study_id, study_title, row_id, country, subnational_region, subnational_region_type) %>%
  mutate(across(everything(), ~ str_split(.x, ";\\s*"))) %>%
  unnest_longer(country) %>%
  unnest_longer(subnational_region) %>%
  unnest_longer(subnational_region_type) %>%
  mutate(across(everything(), str_trim)) %>%
  filter(!is.na(country) | !is.na(subnational_region) | !is.na(subnational_region_type)) %>%
  
  # Apply standardization from alias tables
  left_join(country_aliases, by = c("country" = "raw_country")) %>%
  mutate(country = coalesce(standard_country, country)) %>%
  select(-standard_country) %>%
  left_join(region_aliases, by = c("subnational_region" = "raw_subnational_region")) %>%
  mutate(subnational_region = coalesce(standard_subnational_region, subnational_region)) %>%
  select(study_id, study_title, row_id, country, subnational_region, subnational_region_type)

# ---------------------------
# 5. Tag validity using lookup table
# ---------------------------

country_tagged <- country_cleaned %>%
  left_join(
    validated_lookup %>%
      mutate(valid_flag = TRUE),
    by = c("country", "subnational_region", "subnational_region_type")
  ) %>%
  mutate(valid_flag = if_else(is.na(valid_flag), FALSE, TRUE))

# ---------------------------
# 6. Diagnostics
# ---------------------------

country_valid <- country_tagged %>% filter(valid_flag)
country_invalid <- country_tagged %>% filter(!valid_flag)

country_valid <- country_valid %>% 
  select(!valid_flag)

# ---------------------------
# 7. Export cleaned data
# ---------------------------

output_dir <- here("data", "intermediate")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save cleaned long format
write_csv(country_valid, file.path(output_dir, "country_clean_valid.csv"))