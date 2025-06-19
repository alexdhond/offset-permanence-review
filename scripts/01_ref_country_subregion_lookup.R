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
library(countrycode) # Regions of world

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

continent_aliases <- tribble(
  ~raw_continent,              ~standard_continent,
  "North America",             "North America",
  "South America",             "South America",
  "Latin America",             "South America",
  "Latin America & Caribbean", "South America",
  "Central America",           "South America",
  "Americas",                  NA_character_,  # too broad, mark NA to flag later
  "Europe",                    "Europe",
  "European Union",            "Europe",
  "Asia",                      "Asia",
  "South Asia",                "Asia",
  "East Asia",                 "Asia",
  "East Asia & Pacific",       "Asia",
  "Middle East",               "Asia",
  "Middle East & North Africa","Asia",
  "Africa",                    "Africa",
  "Sub-Saharan Africa",        "Africa",
  "West Africa",               "Africa",
  "Oceania",                   "Oceania",
  "Australia and New Zealand", "Oceania"
)

# Custom entries (supranational, continents, regions)
custom_entries <- tribble(
  ~country,              ~subnational_region, ~subnational_region_type,
  "United Kingdom",      "England",           "Constituent Country",
  "European Union",      NA_character_,       "Supranational"
)

# Save country and region aliases
write_csv(continent_aliases, here("data", "reference", "continent_aliases.csv"))
write_csv(country_aliases, here("data", "reference", "country_aliases.csv"))
write_csv(region_aliases, here("data", "reference", "region_aliases.csv"))

# ---------------------------
# 4. Build unified authoritative lookup
# ---------------------------

lookup_global <- ne_states(returnclass = "sf") %>%
  st_set_geometry(NULL) %>%
  select(
    country = admin,
    subnational_region = name,
    subnational_region_type = type_en
  ) %>%
  mutate(across(everything(), str_trim)) %>%
  bind_rows(custom_entries) %>%
  mutate(
    wb_region = countrycode(
      country,
      origin = "country.name",
      destination = "region",
      custom_match = c(
        "European Union" = "Europe & Central Asia",
        "Latin America" = "Latin America & Caribbean",
        "Africa" = "Sub-Saharan Africa",
        "Asia" = "East Asia & Pacific",
        "Europe" = "Europe & Central Asia",
        "West Africa" = "Sub-Saharan Africa",
        "Oceania" = "Australia and New Zealand"
      )
    ),
    continent = case_when(
      # Manually correct known North America countries in Latin America & Caribbean
      country %in% c("Mexico", "Costa Rica", "Guatemala", "Panama", "Honduras", "El Salvador", "Nicaragua") ~ "North America",
      country %in% c("Australia", "New Zealand", "Papua New Guinea", "Fiji", "Samoa") ~ "Oceania",
      wb_region == "Latin America & Caribbean" ~ "South America",
      wb_region == "North America" ~ "North America",
      wb_region == "Europe & Central Asia" ~ "Europe",
      wb_region %in% c("East Asia & Pacific", "South Asia", "Middle East & North Africa") ~ "Asia",
      wb_region == "Sub-Saharan Africa" ~ "Africa",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct()

# ✅ Create authoritative quadruplet list
lookup_quadruplets <- lookup_global %>%
  select(continent, country, subnational_region, subnational_region_type) %>%
  distinct()

# ✅ Valid continent-country combinations
valid_country_continent <- lookup_global %>%
  distinct(continent, country) %>%
  filter(!is.na(continent) & !is.na(country))

# ---------------------------
# 5. Clean and explode raw input data
# ---------------------------

data_exploded <- data %>%
  select(continent, country, subnational_region, subnational_region_type) %>%
  mutate(across(everything(), ~str_split(.x, ";\\s*"))) %>%
  unnest_longer(continent) %>%
  unnest_longer(country) %>%
  unnest_longer(subnational_region) %>%
  unnest_longer(subnational_region_type) %>%
  mutate(across(everything(), str_trim)) %>%
  filter(!(is.na(continent) & is.na(country) & is.na(subnational_region) & is.na(subnational_region_type))) %>%
  left_join(country_aliases, by = c("country" = "raw_country")) %>%
  mutate(country = coalesce(standard_country, country)) %>%
  select(-standard_country) %>%
  left_join(region_aliases, by = c("subnational_region" = "raw_subnational_region")) %>%
  mutate(subnational_region = coalesce(standard_subnational_region, subnational_region)) %>%
  select(continent, country, subnational_region, subnational_region_type) %>%
  left_join(continent_aliases, by = c("continent" = "raw_continent")) %>%
  mutate(continent = coalesce(standard_continent, continent)) %>%
  select(-standard_continent) %>%
  distinct()

# ---------------------------
# 6. Fill missing continent/type from authoritative source
# ---------------------------

data_exploded <- data_exploded %>%
  left_join(
    lookup_global %>%
      select(country, subnational_region, type_fill = subnational_region_type, continent_lookup = continent),
    by = c("country", "subnational_region")
  ) %>%
  mutate(
    subnational_region_type = coalesce(subnational_region_type, type_fill),
    continent = coalesce(continent, continent_lookup)
  ) %>%
  select(continent, country, subnational_region, subnational_region_type) %>%
  distinct()

# ---------------------------
# 7. Validate exploded values
# ---------------------------

# 1. Full quadruplet match
validated_full <- data_exploded %>%
  filter(!is.na(subnational_region) | !is.na(subnational_region_type)) %>%
  inner_join(
    lookup_quadruplets,
    by = c("continent", "country", "subnational_region", "subnational_region_type")
  )

# 2. Country-only match (valid continent-country combos)
validated_country_only <- data_exploded %>%
  filter(is.na(subnational_region) & is.na(subnational_region_type)) %>%
  inner_join(valid_country_continent, by = c("continent", "country"))

# ✅ 3. Continent-only match (all other fields NA)
validated_continent_only <- data_exploded %>%
  filter(!is.na(continent) & is.na(country) & is.na(subnational_region) & is.na(subnational_region_type)) %>%
  distinct()

# 4. Combine all validated
validated <- bind_rows(validated_full, validated_country_only, validated_continent_only) %>%
  distinct()

# 5. Identify unmatched records
unmatched <- anti_join(
  data_exploded,
  validated,
  by = c("continent", "country", "subnational_region", "subnational_region_type")
)

# ---------------------------
# 8. Diagnose unmatched quadruplets (robust version)
# ---------------------------

# Reference lookup sets
lookup_countries <- lookup_quadruplets %>% distinct(country)
lookup_regions <- lookup_quadruplets %>% distinct(subnational_region)
lookup_types <- lookup_quadruplets %>% distinct(subnational_region_type)
lookup_quad_str <- lookup_quadruplets %>%
  mutate(quad = paste(continent, country, subnational_region, subnational_region_type))

unmatched_diagnosed <- unmatched %>%
  mutate(
    quad = paste(continent, country, subnational_region, subnational_region_type),
    issue_type = case_when(
      # Case: continent-only entry (no country, no region, no type)
      !is.na(continent) & is.na(country) & is.na(subnational_region) & is.na(subnational_region_type) ~ "continent_only",
      
      # Case: supranational entry (e.g., EU)
      !is.na(country) & is.na(subnational_region) & !is.na(subnational_region_type) &
        paste(country, subnational_region_type) %in%
        paste(lookup_quadruplets$country, lookup_quadruplets$subnational_region_type) ~ "valid_supranational",
      
      # Case: country-only entry
      !is.na(country) & is.na(subnational_region) & is.na(subnational_region_type) &
        country %in% lookup_countries$country ~ "country_only",
      
      # Case: invalid country
      !is.na(country) & !country %in% lookup_countries$country ~ "bad_country",
      
      # Case: invalid region
      !is.na(subnational_region) & !subnational_region %in% lookup_regions$subnational_region ~ "bad_region",
      
      # Case: invalid region type
      !is.na(subnational_region_type) & !subnational_region_type %in% lookup_types$subnational_region_type ~ "bad_type",
      
      # Case: valid fields, but not matching a known quadruplet
      !quad %in% lookup_quad_str$quad ~ "bad_quadruplet",
      
      # Catch-all
      TRUE ~ "unknown"
    )
  ) %>%
  select(continent, country, subnational_region, subnational_region_type, issue_type)

# ---------------------------
# 9. Review unmatched entries
# ---------------------------

unmatched_review <- unmatched_diagnosed %>%
  left_join(
    lookup_quadruplets,
    by = c("continent", "country", "subnational_region", "subnational_region_type"),
    suffix = c("_input", "_ref")
  )

# ---------------------------
# 10. Export for manual review
# ---------------------------

write_csv(unmatched, here("data", "reference", "unmatched_country_region.csv"))
write_csv(validated, here("data", "reference", "validated_country_region_lookup.csv"))
