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
# 2. Load raw data and aliases
# ---------------------------

data_raw <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

country_aliases <- read_csv(here("data", "reference", "country_aliases.csv"), show_col_types = FALSE)
region_aliases  <- read_csv(here("data", "reference", "region_aliases.csv"), show_col_types = FALSE)

# ---------------------------
# 3. Load lookup tables
# ---------------------------

lookup_eco <- read_csv(here("data", "reference", "ecosystem_specific_to_broad_lookup.csv"), show_col_types = FALSE)
lookup_country <- read_csv(here("data", "reference", "validated_country_region_lookup.csv"), show_col_types = FALSE)
lookup_proj_int <- read_csv(here("data", "reference", "project_intervention_type_lookup.csv"), show_col_types = FALSE)

# ---------------------------
# 4. ECOSYSTEM: Explode and join lookup
# ---------------------------

# data_ecosystem_cleaned <- data_raw %>%
#   separate_rows(ecosystem_type_specific, sep = ";\\s*") %>%
#   select(-ecosystem_type_broad) %>%
#   left_join(lookup_eco, by = "ecosystem_type_specific") %>%
#   rename(ecosystem_type_broad = broad_ecosystem) %>%
#   relocate(ecosystem_type_broad, .after = ecosystem_type_specific)
# 
# unmapped_ecosystems <- data_raw %>%
#   separate_rows(ecosystem_type_specific, sep = ";\\s*") %>%
#   distinct(ecosystem_type_specific) %>%
#   filter(!ecosystem_type_specific %in% lookup_eco$ecosystem_type_specific)

# ---------------------------
# 5. COUNTRY/SUBNATIONAL REGION: Clean, validate, join
# ---------------------------

# ---------------------------
# 5. COUNTRY/SUBNATIONAL REGION: Clean, alias, validate
# ---------------------------

# Step 1: Explode and clean
data_country_cleaned <- data_raw %>%
  separate_rows(country, sep = ";\\s*") %>%
  separate_rows(subnational_region, sep = ";\\s*") %>%
  separate_rows(subnational_region_type, sep = ";\\s*") %>%
  mutate(across(c(country, subnational_region, subnational_region_type), str_trim)) %>%
  filter(!(is.na(country) & is.na(subnational_region) & is.na(subnational_region_type))) %>%
  select(study_title, country, subnational_region, subnational_region_type)

# Step 2: Apply aliases
data_country_cleaned <- data_country_cleaned %>%
  left_join(country_aliases, by = c("country" = "raw_country")) %>%
  mutate(country = coalesce(standard_country, country)) %>%
  select(-standard_country) %>%
  left_join(region_aliases, by = c("subnational_region" = "raw_subnational_region")) %>%
  mutate(subnational_region = coalesce(standard_subnational_region, subnational_region)) %>%
  select(study_title, country, subnational_region, subnational_region_type)

# 3. Impute missing subnational_region_type for country-only rows (NEW STEP)
data_country_cleaned <- data_country_cleaned %>%
  left_join(
    lookup_country %>%
      filter(is.na(subnational_region)) %>%
      select(country, subnational_region_type),
    by = "country",
    suffix = c("", "_lookup")
  ) %>%
  mutate(
    subnational_region_type = coalesce(subnational_region_type, subnational_region_type_lookup)
  ) %>%
  select(-subnational_region_type_lookup)

# 4: Exact matching with lookup
lookup_country_clean <- lookup_country %>%
  mutate(across(c(country, subnational_region, subnational_region_type), str_trim)) %>%
  distinct()

validated_country <- data_country_cleaned %>%
  mutate(row_id = row_number()) %>%
  inner_join(lookup_country_clean,
             by = c("country", "subnational_region", "subnational_region_type")) %>%
  mutate(matched_lookup = TRUE)

# Step 4: Identify unmatched rows
unmatched_country <- data_country_cleaned %>%
  anti_join(validated_country,
            by = c("country", "subnational_region", "subnational_region_type"))

# Step 5: Optional diagnostics
unmatched_review <- unmatched_country %>%
  left_join(
    lookup_country_clean,
    by = c("country", "subnational_region"),
    suffix = c("_input", "_lookup")
  )

# Keep only matched rows — effectively removing unmatched ones
data_country_final <- validated_country %>%
  select(-matched_lookup, -row_id)  # optional: remove helper cols if present

# ---------------------------
# 6. PROJECT TYPE: Placeholder for next steps
# ---------------------------

# [Add your logic for exploding and joining project intervention types here]
