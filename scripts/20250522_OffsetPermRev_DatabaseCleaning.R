# ===============================================
# Script:     ref_country_region.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Create reference table mapping countries to subnational regions and region types from excel data
# ===============================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # read Excel files
library(janitor)   # clean column names
library(tidyverse) # data wrangling and manipulation
library(here)      # file paths

# ---------------------------
# 2. Define file path and clean data
# ---------------------------

# Load Excel file from correct folder path
excel_file <- here("data", "offset_perm_rev_database.xlsx")

# Load data into R
data <- read_excel(excel_file) %>%
  clean_names()

# ---------------------------
# 3. Split and align country columns
# ---------------------------

# Filter and explode only country, subnational_region, and subnational_region_type
df_locations <- data %>%
  mutate(across(
    c(country, subnational_region, subnational_region_type),
    ~ str_split(.x, ";\\s*")
  )) %>%
  mutate(row_id = row_number()) %>%
  unnest_longer(country) %>%
  unnest_longer(subnational_region) %>%
  unnest_longer(subnational_region_type) %>%
  select(-row_id)

# ---------------------------
# 4. Create country-region reference table
# ---------------------------

ref_country_region <- df_locations %>%
  distinct(country, subnational_region, subnational_region_type) %>%
  arrange(country, subnational_region)


install.packages("rnaturalearth")

library(rnaturalearth)
library(sf)
library(dplyr)
library(tidyr)

admin1 <- ne_states(returnclass = "sf")
# Get admin level 1 data (states, provinces, regions)
admin1 <- ne_states(returnclass = "sf")

# Select relevant columns
lookup_global <- admin1 %>%
  st_set_geometry(NULL) %>%  # drop spatial info
  select(admin = admin,      # country name
         name = name,        # subnational region name
         type = type_en) %>%    # type (state, province, territory, etc)
  distinct() %>%
  rename(
    country = admin,
    subnational_region = name,
    subnational_region_type = type
  ) %>%
  arrange(country, subnational_region)

# View sample
head(lookup_global)

library(tibble)

global_lookup <- tibble( country = unique(ref_country_region$country),
  subnational_region = unique(ref_country_region$subnational_region),
  subnational_region_type = unique(ref_country_region$subnational_region_type))


# View it
print(global_lookup)





# ---------------------------
# 6. Save reference table
# ---------------------------

output_path <- here("data", "processed", "country_region_reference.csv")
write_csv(ref_country_region, output_path)

# ---------------------------
# 7. Done
# ---------------------------

message("✅ Country-region reference table saved to: ", output_path)

