# ================================================================
# Script:     10_clean_country_fields.R
# Date:       2025-06-09
# Author:     Alex Dhond
# Purpose:    Apply standardized country and subnational region lookup to raw data
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------
library(tidyverse)
library(readxl)
library(here)
library(janitor)

# ---------------------------
# 2. Load raw data and lookups
# ---------------------------
raw_data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(row_id = row_number()) # for rejoining later

country_aliases <- read_csv(here("data", "reference", "country_aliases.csv"))
region_aliases  <- read_csv(here("data", "reference", "region_aliases.csv"))
validated_lookup <- read_csv(here("data", "reference", "validated_country_region_lookup.csv"))

# ---------------------------
# 3. Explode semicolon-delimited fields
# ---------------------------
exploded <- raw_data %>%
  select(study_title, row_id, country, subnational_region, subnational_region_type) %>%
  mutate(across(everything(), ~ str_split(., ";\\s*"))) %>%
  unnest_longer(country) %>%
  unnest_longer(subnational_region) %>%
  unnest_longer(subnational_region_type) %>%
  mutate(across(everything(), str_trim)) %>%
  filter(!(is.na(country) & is.na(subnational_region) & is.na(subnational_region_type)))

# ---------------------------
# 4. Apply alias mappings
# ---------------------------
exploded <- exploded %>%
  left_join(country_aliases, by = c("country" = "raw_country")) %>%
  mutate(country = coalesce(standard_country, country)) %>%
  select(-standard_country) %>%
  left_join(region_aliases, by = c("subnational_region" = "raw_subnational_region")) %>%
  mutate(subnational_region = coalesce(standard_subnational_region, subnational_region)) %>%
  select(study_title, row_id, country, subnational_region, subnational_region_type)

# ---------------------------
# 5. Join to validated lookup and keep only valid rows
# ---------------------------

cleaned <- exploded %>%
  inner_join(validated_lookup, by = c("country", "subnational_region", "subnational_region_type"))

# ---------------------------
# 5b. Validation diagnostics
# ---------------------------

# Find dropped row_ids by comparing before/after cleaning
dropped_rows <- anti_join(
  exploded %>% mutate(row_id = as.integer(row_id)),
  cleaned %>% mutate(row_id = as.integer(row_id)),
  by = c("row_id", "country", "subnational_region", "subnational_region_type")
)

# Join to original study titles to see which records were affected
dropped_summary <- dropped_summary <- dropped_rows %>%
  select(row_id, study_title, country, subnational_region, subnational_region_type) %>%
  arrange(study_title)

# Print summary to console
message("⚠️ The following country-region combinations could not be matched to the validated lookup:")
print(dropped_summary, n = 50)  # adjust as needed

# Optionally save to file
# write_csv(dropped_summary, here("data", "reference", "dropped_country_region_combinations.csv"))

# ---------------------------
# 5c. Validation: Confirm expected counts
# ---------------------------

original_summary <- exploded %>%
  summarise(
    studies_original = n_distinct(row_id),
    countries_original = n_distinct(country),
    regions_original = n_distinct(subnational_region)
  )

cleaned_summary <- cleaned %>%
  summarise(
    studies_cleaned = n_distinct(row_id),
    countries_cleaned = n_distinct(country),
    regions_cleaned = n_distinct(subnational_region)
  )

validation_check <- bind_cols(original_summary, cleaned_summary)

print(validation_check)

# Optional message
if (validation_check$studies_cleaned < validation_check$studies_original) {
  message("⚠️ Some studies were dropped during cleaning.")
} else {
  message("✅ All studies retained.")
}

# Get list of all study row_ids from exploded and cleaned
dropped_studies <- exploded %>%
  distinct(row_id, study_title) %>%
  anti_join(cleaned %>% distinct(row_id), by = "row_id")

dropped_details <- dropped_rows %>%
  filter(row_id %in% dropped_studies$row_id) %>%
  select(row_id, study_title, country, subnational_region, subnational_region_type) %>%
  distinct() %>%
  arrange(study_title)

# ---------------------------
# 6. Collapse back to one row per study
# ---------------------------
cleaned_collapsed <- cleaned %>%
  group_by(row_id) %>%
  reframe(
    study_title = first(study_title),
    country_clean = paste(unique(country), collapse = "; "),
    subnational_region_clean = paste(unique(subnational_region), collapse = "; "),
    subnational_region_type_clean = paste(unique(subnational_region_type), collapse = "; ")
  )

length(unique(cleaned_collapsed$study_title))
length(unique(cleaned$study_title))


# ---------------------------
# 7. Merge back into full dataset
# ---------------------------
raw_data_cleaned <- raw_data %>%
  left_join(cleaned_collapsed %>% mutate(row_id = as.integer(row_id)), by = "row_id")

# ---------------------------
# 8. Save output
# ---------------------------
#write_csv(raw_data_cleaned, here("data", "intermediate", "cleaned_country_fields.csv"))

#message("✅ Country cleaning complete. Output written to /data/intermediate/cleaned_country_fields.csv")
