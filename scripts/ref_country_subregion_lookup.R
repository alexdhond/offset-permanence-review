# ===============================================
# Script:     ref_country_subregion_lookup.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Explode and validate country/subnational region pairs
# ===============================================

# ---------------------------
# 1. Load packages
# ---------------------------
library(rnaturalearth)
library(sf)
library(tidyverse)
library(readr)
library(janitor)
library(readxl)
library(here)

# ---------------------------
# 2. Load raw data
# ---------------------------

# load data
data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

# ---------------------------
# 3. Define alias tables
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

# save aliases
write_csv(country_aliases, here("data", "reference", "country_aliases.csv"))
write_csv(region_aliases, here("data", "reference", "region_aliases.csv"))

# Custom entries (supranational, continents, regions)
custom_entries <- tribble(
  ~country,              ~subnational_region, ~subnational_region_type,
  "United Kingdom",      "England",           "Constituent Country",
  "European Union",      NA_character_,       "Supranational",
  "Europe",              NA_character_,       "Continent",
  "Latin America",       NA_character_,       "Region",
  "Scandinavia",         NA_character_,       "Cultural Region",
  "Sub-Saharan Africa",  NA_character_,       "Region",
  "Asia",                NA_character_,       "Continent",
  "West Africa",         NA_character_,       "Region"
)

# ---------------------------
# 4. Build authoritative lookup table of regions from Natural Earth
# ---------------------------

# Build lookup table and select 
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
# 5. Explode and clean data
# ---------------------------
data_exploded <- data %>%
  select(country, subnational_region, subnational_region_type) %>%
  mutate(
    country = str_split(country, ";\\s*"),
    subnational_region = str_split(subnational_region, ";\\s*"),
    subnational_region_type = str_split(subnational_region_type, ";\\s*")
  ) %>%
  unnest_longer(country) %>%
  unnest_longer(subnational_region) %>%
  unnest_longer(subnational_region_type) %>%
  mutate(across(everything(), str_trim)) %>%
  filter(!(is.na(country) & is.na(subnational_region) & is.na(subnational_region_type))) %>%
  left_join(country_aliases, by = c("country" = "raw_country")) %>%
  mutate(country = coalesce(standard_country, country)) %>%
  select(-standard_country) %>%
  left_join(region_aliases, by = c("subnational_region" = "raw_subnational_region")) %>%
  mutate(subnational_region = coalesce(standard_subnational_region, subnational_region)) %>%
  select(country, subnational_region, subnational_region_type) %>%
  distinct()

# ---------------------------
# 6. Fill in missing region types from lookup
# ---------------------------
data_exploded <- data_exploded %>%
  left_join(
    lookup_global %>%
      select(country, subnational_region, type_fill = subnational_region_type),
    by = c("country", "subnational_region")
  ) %>%
  mutate(
    subnational_region_type = coalesce(subnational_region_type, type_fill)
  ) %>%
  select(-type_fill)

# ---------------------------
# 7. Validate against lookup
# ---------------------------

# Full match (country + region + type)
matched_full <- data_exploded %>%
  semi_join(lookup_global, by = c("country", "subnational_region", "subnational_region_type"))

# Country-only match (where region info is missing)
country_only_match <- data_exploded %>%
  filter(is.na(subnational_region) & is.na(subnational_region_type)) %>%
  semi_join(lookup_global %>% distinct(country), by = "country")

# Final validated table
validated <- bind_rows(matched_full, country_only_match)

# Identify unmatched rows
unmatched <- anti_join(data_exploded, validated, by = c("country", "subnational_region", "subnational_region_type"))

# ---------------------------
# 8. Diagnose unmatched
# ---------------------------
lookup_countries <- lookup_global %>% distinct(country)
lookup_regions <- lookup_global %>% distinct(subnational_region)
lookup_triplets <- lookup_global %>% distinct(country, subnational_region, subnational_region_type)

unmatched_diagnosed <- unmatched %>%
  mutate(
    issue_type = case_when(
      !country %in% lookup_countries$country ~ "bad_country",
      !subnational_region %in% lookup_regions$subnational_region & !is.na(subnational_region) ~ "bad_region",
      !paste(country, subnational_region, subnational_region_type) %in%
        paste(lookup_triplets$country, lookup_triplets$subnational_region, lookup_triplets$subnational_region_type)
      ~ "bad_type",
      is.na(subnational_region) & is.na(subnational_region_type) ~ "only_country",
      TRUE ~ "completely_unknown"
    )
  )

# ---------------------------
# 9. Review unmatched by joining lookup
# ---------------------------
unmatched_review <- unmatched %>%
  left_join(
    lookup_global,
    by = c("country", "subnational_region"),
    suffix = c("_input", "_ref")
  )

# ---------------------------
# 10. Optional: Export for manual review
# ---------------------------
write_csv(unmatched, here("data", "reference", "unmatched_country_region.csv"))

write_csv(validated, here("data", "reference", "validated_country_region_lookup.csv"))
