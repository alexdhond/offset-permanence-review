# ===============================================
# Script:     ref_country_subregion_lookup.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Load and save a global lookup table for countries and subnational regions
# ===============================================

# ---------------------------
# 1. Load packages
# ---------------------------
library(rnaturalearth) # access natural earth data
library(sf) # spatial data frame
library(dplyr) # data manipulation
library(readr) # read/write csv
library(here) # construct relative file paths for easier saving

# ---------------------------
# 2. Load Global Admin Level 1 Data
# ---------------------------

# ne_states returns first-level administrative units (e.g., states, provinces)
admin1 <- ne_states(returnclass = "sf")

# ---------------------------
# 3. Clean and Prepare Lookup Table
# ---------------------------

lookup_global <- admin1 %>%
  st_set_geometry(NULL) %>%       # Drop spatial geometry, keep attributes only
  select(
    country = admin,              # Country name
    subnational_region = name,    # Subnational region name
    subnational_region_type = type_en  # Type of subdivision (state, province, etc.)
  ) %>%
  distinct() %>%                  # Remove duplicate rows if any
  arrange(country, subnational_region)  # Sort for readability

# ---------------------------
# 4 Standardize Country Names (Alias Mapping)
# ---------------------------

# Create lookup table for common aliases
country_aliases <- tribble(
  ~raw_country,       ~standard_country,
  "UK",               "United Kingdom",
  "U.K.",             "United Kingdom",
  "USA",              "United States of America",
  "US",               "United States of America",
  "Russia",           "Russian Federation",
  "Viet Nam",         "Vietnam",
  "Iran",             "Iran, Islamic Republic of",
  "South Korea",      "Korea, Republic of"
  # Add more aliases as needed
)

unique(data_exploded$country)


# Apply to lookup table
lookup_global <- lookup_global %>%
  left_join(country_aliases, by = c("country" = "raw_country")) %>%
  mutate(country = coalesce(standard_country, country)) %>%
  select(-standard_country)

# ---------------------------
# 4. Save Lookup Table
# ---------------------------

# Save to directory, make one if does not exist
output_dir <- here("data", "reference")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# save file to path and write to csv
output_path <- file.path(output_dir, "global_country_region_lookup.csv")
write_csv(lookup_global, output_path)

