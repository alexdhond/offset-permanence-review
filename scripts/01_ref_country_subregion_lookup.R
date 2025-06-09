# ===============================================
# Script:     01_ref_country_subregion_lookup.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Standardize and validate country and subnational 
#             region entries using authoritative geographic data
# ===============================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(rnaturalearth) # Regions of the world
library(sf) # Spatial data (just need for NE)
library(tidyverse) # Data manipulation
library(readr) # Reading files
library(janitor) # Clean column names
library(readxl) # Read Excel files
library(here) # Easy path names

# ---------------------------
# 2. Load raw data
# ---------------------------

# Load data and clean column names
data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

# ---------------------------
# 3. Build and save alias tables for countries
# ---------------------------

# Country aliases
country_aliases <- tribble(
  ~raw_country,       ~standard_country,
  "UK",               "United Kingdom",
  "USA",              "United States of America",
  "US",               "United States of America",
  "Russia",           "Russian Federation",
  "Viet Nam",         "Vietnam",
  "Iran",             "Iran, Islamic Republic of",
  "South Korea",      "Korea, Republic of",
  "Tanzania",         "United Republic of Tanzania"
)

# Region aliases
region_aliases <- tribble(
  ~raw_subnational_region,    ~standard_subnational_region,
  "Huaphan",                  "Houaphan",
  "British Colombia",         "British Columbia",
  "NWT",                      "Northwest Territories",
  "South Baja California",    "Baja California Sur",
  "North Baja California",    "Baja California",
  "East Kalimantan",          "Kalimantan Timur",
  "Xishuangbanna",            "Yunnan",
  "Nghe An",                  "Nghệ An",
  "Saxony-Anhalt",            "Sachsen-Anhalt",
  "Lower Saxony",             "Niedersachsen",
  "North Rhine-Westphalia",   "Nordrhein-Westfalen",
  "Oddar Meanchey",           "Otdar Mean Chey",
  "Quebec",                   "Québec"
)

# Custom entries (supranational, continents, regions)
custom_entries <- tribble(
  ~country,              ~subnational_region, ~subnational_region_type,
  "United Kingdom",      "England",           "Constituent Country",
  "European Union",      NA_character_,       "Supranational",
  "Europe",              NA_character_,       "Continent",
  "Latin America",       NA_character_,       "Region",
  "Sub-Saharan Africa",  NA_character_,       "Region",
  "Asia",                NA_character_,       "Continent",
  "Africa",              NA_character_,       "Continent",
  "West Africa",         NA_character_,       "Region"
)

# Save country and region aliases
write_csv(country_aliases, here("data", "reference", "country_aliases.csv"))
write_csv(region_aliases, here("data", "reference", "region_aliases.csv"))

# ---------------------------
# 4. Build authoritative lookup
# ---------------------------

# Build lookup table
lookup_global <- ne_states(returnclass = "sf") %>%
  st_set_geometry(NULL) %>%
  select(
    country = admin,
    subnational_region = name,
    subnational_region_type = type_en
  ) %>%
  mutate(across(everything(), str_trim)) %>%
  bind_rows(custom_entries) %>%
  distinct()

# ---------------------------
# 5. Un-nest raw data
# ---------------------------

# Unnest raw data
data_exploded <- data %>%
  
  # Select relevant columns (country, subnational region, region type)
  select(country, subnational_region, subnational_region_type) %>%
  
  # Split semicolon-delimited strings into lists for each column
  mutate( 
    country = str_split(country, ";\\s*"),
    subnational_region = str_split(subnational_region, ";\\s*"),
    subnational_region_type = str_split(subnational_region_type, ";\\s*")
  ) %>%
  
  # Un-nest each list column so each combo appears in own row
  unnest_longer(country) %>%
  unnest_longer(subnational_region) %>%
  unnest_longer(subnational_region_type) %>%
  
  # Remove whitespace
  mutate(across(everything(), str_trim)) %>%
  
  # Remove all NAs
  filter(!(is.na(country) & is.na(subnational_region) & is.na(subnational_region_type))) %>%
  
  # Standardize country and region names using alias tables
  left_join(country_aliases, by = c("country" = "raw_country")) %>%
  mutate(country = coalesce(standard_country, country)) %>%
  select(-standard_country) %>%
  left_join(region_aliases, by = c("subnational_region" = "raw_subnational_region")) %>%
  mutate(subnational_region = coalesce(standard_subnational_region, subnational_region)) %>%
  select(country, subnational_region, subnational_region_type) %>%
  
  # Ensure each row is unique
  distinct()

# ---------------------------
# 6. Fill in missing region types from lookup
# ---------------------------

data_exploded <- data_exploded %>%
  
  # Join the authoritative region type from lookup_global
  # Only brings in 'type_fill' (renamed subnational_region_type) for matching country/region combos
  left_join(
    lookup_global %>%
      select(country, subnational_region, type_fill = subnational_region_type),
    by = c("country", "subnational_region")
  ) %>%
  
  # If subnational_region_type was missing in the raw data, fill it using the authoritative value
  mutate(
    subnational_region_type = coalesce(subnational_region_type, type_fill)
  ) %>%
  # Drop the helper column once it's no longer needed
  select(-type_fill)

# ---------------------------
# 7. Validate exploded values
# ---------------------------

# Full validated combinations: country + region + type
matched_full <- data_exploded %>%
  semi_join(lookup_global, by = c("country", "subnational_region", "subnational_region_type"))

# Country-only match (where region info is missing)
country_only_match <- data_exploded %>%
  filter(is.na(subnational_region) & is.na(subnational_region_type)) %>%
  semi_join(lookup_global %>% distinct(country), by = "country")

# Combine validated
validated <- bind_rows(matched_full, country_only_match)

# Identify unmatched combos
unmatched <- anti_join(data_exploded, validated, by = c("country", "subnational_region", "subnational_region_type"))

# ---------------------------
# 8. Diagnose unmatched cases
# ---------------------------

# Reference sets for comparison
lookup_countries <- lookup_global %>% distinct(country)
lookup_regions <- lookup_global %>% distinct(subnational_region)
lookup_triplets <- lookup_global %>% distinct(country, subnational_region, subnational_region_type)

# Classify why each unmatched row failed to match
unmatched_diagnosed <- unmatched %>%
  mutate(
    issue_type = case_when(
      
      # Country is not in lookup at all
      !country %in% lookup_countries$country ~ "bad_country",
      # Region is not in lookup, flag only if missing
      !subnational_region %in% lookup_regions$subnational_region & !is.na(subnational_region) ~ "bad_region",
      # Combo exists in parts but not as a full match
      !paste(country, subnational_region, subnational_region_type) %in%
        paste(lookup_triplets$country, lookup_triplets$subnational_region, lookup_triplets$subnational_region_type)
      ~ "bad_type",
      # Only country is provided, no region/type info
      is.na(subnational_region) & is.na(subnational_region_type) ~ "only_country",
      # anything else
      TRUE ~ "completely_unknown"
    )
  )

# ---------------------------
# 9. Review unmatched entries
# ---------------------------

unmatched_review <- unmatched %>%
  left_join(
    lookup_global,
    by = c("country", "subnational_region"),
    suffix = c("_input", "_ref")
  )

# ---------------------------
# 10. Export for manual review
# ---------------------------

# Export unmatched
write_csv(unmatched, here("data", "reference", "unmatched_country_region.csv"))

# Export validated pairs
write_csv(validated, here("data", "reference", "validated_country_region_lookup.csv"))

