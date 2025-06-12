# ================================================================
# Script:     ref_offset_program_lookup.R
# Date:       2025-06-03
# Author:     Alex Dhond
# Purpose:    Build and export lookup for standardizing offset program names
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)
library(tidyverse)
library(here)
library(janitor)

# ---------------------------
# 2. Load and prepare data
# ---------------------------

data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(row_id = row_number())

# Explode semicolon-separated program names
program_exploded <- data %>%
  select(row_id, study_title, offset_program_name) %>%
  mutate(offset_program_name = str_split(offset_program_name, ";\\s*")) %>%
  unnest_longer(offset_program_name) %>%
  mutate(offset_program_name = str_trim(offset_program_name)) %>%
  filter(!is.na(offset_program_name), offset_program_name != "")

# Check unique program names
unique_programs <- unique(program_exploded$offset_program_name)

# ---------------------------
# 3. Define standardization lookup table
# ---------------------------

offset_program_lookup <- tribble(
  ~original_name, ~standardized_name, ~program_type, ~mechanism_type, ~status, ~related_program, ~scope_level, ~scope_location, ~note,
  
  # --- US Programs ---
  "US Clean Water Act Section 404 Wetland and Stream Permitting", "US CWA 404 Permitting", "compliance", "permitting", "active", NA, "national", "USA", NA,
  "US Wetland and Stream Mitigation Banking", "US Mitigation Banking", "compliance", "banking", "active", NA, "national", "USA", NA,
  "US Species Conservation Banking Program", "US Conservation Banking", "compliance", "banking", "active", NA, "national", "USA", NA,
  
  # --- UN / Global Mechanisms ---
  "Activities Implemented Jointly (AIJ) Pilot Phase", "AIJ Pilot Phase", "voluntary", "project-based mechanism", "historical", NA, "international", "UNFCCC", NA,
  "UNFCCC Clean Development Mechanism (CDM)", "UNFCCC CDM", "compliance", "project-based mechanism", "active", NA, "international", "UNFCCC", NA,
  "UNFCCC REDD+ Mechanism", "REDD+", "voluntary", "project-based mechanism", "active", NA, "international", "Global", NA,
  "REDD/REDD+", "REDD+", "voluntary", "project-based mechanism", "active", NA, "international", "Global", NA,
  
  # --- Canada ---
  "Canada Fisheries Act Section 35(2) Fish Habitat Authorization Program", "Canada Fisheries Act Offsetting", "compliance", "permitting", "active", NA, "national", "Canada", NA,
  "Canadian Fisheries Act Offsetting", "Canada Fisheries Act Offsetting", "compliance", "permitting", "active", NA, "national", "Canada", NA,
  
  # --- Australia (State & National) ---
  "NSW BioBanking Scheme", "NSW BioBanking Scheme", "compliance", "banking", "replaced", "NSW Biodiversity Offsets", "subnational", "Australia", NA,
  "NSW Native Vegetation Offset Scheme", "NSW Native Vegetation Offset Scheme", "compliance", "banking", "replaced", "NSW Biodiversity Offsets", "subnational", "Australia", NA,
  "NSW Environmental Impact Offset Scheme", "NSW Environmental Impact Offset Scheme", "compliance", "permitting", "replaced", "NSW Biodiversity Offsets", "subnational", "Australia", NA,
  "NSW Biodiversity Offset Scheme", "NSW Biodiversity Offsets", "compliance", "banking", "active", NA, "subnational", "Australia", NA,
  "NSW Biodiversity Offsets", "NSW Biodiversity Offsets", "compliance", "banking", "active", NA, "subnational", "Australia", NA,
  "Victoria's Native Vegetation Management Framework", "Victoria Native Vegetation Framework", "compliance", "permitting", "active", NA, "subnational", "Australia", NA,
  "Victoria Native Vegetation Framework", "Victoria Native Vegetation Framework", "compliance", "permitting", "active", NA, "subnational", "Australia", NA,
  "Australia Carbon Farming Initiative", "Australia CFI", "compliance", "project-based mechanism", "replaced", "Australia ERF", "national", "Australia", NA,
  "Carbon Farming Initiative", "Australia CFI", "compliance", "project-based mechanism", "replaced", "Australia ERF", "national", "Australia", NA,
  "Carbon Farming Initiative (CFI)", "Australia CFI", "compliance", "project-based mechanism", "replaced", "Australia ERF", "national", "Australia", NA,
  "Australian Emissions Reduction Fund", "Australia ERF", "compliance", "project-based mechanism", "active", NA, "national", "Australia", NA,
  "Australia Emissions Reduction Fund", "Australia ERF", "compliance", "project-based mechanism", "active", NA, "national", "Australia", NA,
  "Emissions Reduction Fund (ERF)", "Australia ERF", "compliance", "project-based mechanism", "active", NA, "national", "Australia", NA,
  "Australian Carbon Credit Unit (ACCU) Scheme", "Australia ACCU Scheme", "compliance", "registry/crediting system", "active", "Australia ERF", "national", "Australia", NA,
  "Australia Carbon Pricing Mechanism", "Australia Carbon Pricing Mechanism", "compliance", "trading scheme", "defunct", NA, "national", "Australia", NA,
  
  # --- California & US Climate Programs ---
  "California Cap-and-Trade Program", "CA Cap-and-Trade", "compliance", "trading scheme", "active", NA, "subnational", "USA", NA,
  "California Air Resources Board (ARB) Compliance Offset Program", "California ARB Offset Program", "compliance", "project-based mechanism", "active", "CA Cap-and-Trade", "subnational", "USA", NA,
  "California ARB Compliance Offset Program", "California ARB Offset Program", "compliance", "project-based mechanism", "active", "CA Cap-and-Trade", "subnational", "USA", NA,
  "California Forest Carbon Offset Program", "California Forest Carbon Offsets", "compliance", "project-based mechanism", "active", "California ARB Offset Program", "subnational", "USA", NA,
  "California Forest Carbon Offset Program (CARB), California Cap-and-Trade", "California Forest Carbon Offsets", "compliance", "project-based mechanism", "active", "California ARB Offset Program", "subnational", "USA", NA,
  "Regional Greenhouse Gas Initiative (RGGI)", "RGGI", "compliance", "trading scheme", "active", NA, "multi-jurisdictional", "USA", NA,
  
  # --- European Mechanisms ---
  "EU Natura 2000/Article 6 Offsetting", "EU Natura 2000 Art. 6 Offsetting", "compliance", "project-based mechanism", "active", NA, "multi-jurisdictional", "EU", NA,
  "German Compensation System (Eingriffsregelung)", "Germany Eingriffsregelung", "compliance", "project-based mechanism", "active", NA, "national", "Germany", NA,
  "National Habitat Compensation Programme", "National Habitat Compensation Programme", "compliance", "project-based mechanism", "active", NA, "national", "France", NA,
  "Great Crested Newt Licensing", "Great Crested Newt Licensing", "compliance", "licensing", "active", NA, "national", "UK", NA,
  
  # --- UK ---
  "England Biodiversity Net Gain", "England BNG", "compliance", "project-based mechanism", "active", NA, "national", "UK", NA,
  "Biodiversity Net Gain", "England BNG", "compliance", "project-based mechanism", "active", NA, "national", "UK", NA,
  "UK Bat Mitigation Licensing Scheme", "UK Bat Mitigation Licensing", "compliance", "project-based mechanism", "active", NA, "national", "UK", NA,
  
  # --- Voluntary Registries (Global) ---
  "Verra Verified Carbon Standard (VCS)", "Verra VCS", "voluntary", "registry/crediting system", "active", NA, "international", "Global", NA,
  "Verified Carbon Standard", "Verra VCS", "voluntary", "registry/crediting system", "active", NA, "international", "Global", NA,
  "Verified Carbon Standard (VCS)", "Verra VCS", "voluntary", "registry/crediting system", "active", NA, "international", "Global", NA,
  "Voluntary Carbon Standard (VCS)", "Verra VCS", "voluntary", "registry/crediting system", "active", NA, "international", "Global", NA,
  "Climate Action Reserve", "Climate Action Reserve (CAR)", "voluntary", "registry/crediting system", "active", NA, "national", "USA", NA,
  "Climate Action Reserve (CAR)", "Climate Action Reserve (CAR)", "voluntary", "registry/crediting system", "active", NA, "national", "USA", NA,
  "American Carbon Registry", "American Carbon Registry (ACR)", "voluntary", "registry/crediting system", "active", NA, "national", "USA", NA,
  "American Carbon Registry (ACR)", "American Carbon Registry (ACR)", "voluntary", "registry/crediting system", "active", NA, "national", "USA", NA,
  
  # --- Other National Programs ---
  "New Zealand Emissions Trading Scheme", "NZ ETS", "compliance", "trading scheme", "active", NA, "national", "New Zealand", NA,
  "China Certified Emission Reductions (CCER)", "China CCER", "compliance", "project-based mechanism", "active", NA, "national", "China", NA,
  "Massachusetts Wetlands Protection Act regulatory program", "Massachusetts Wetlands Program", "compliance", "permitting", "active", NA, "subnational", "USA", NA
)



# ---------------------------
# 4. Join and standardize program names
# ---------------------------

program_standardized <- program_exploded %>%
  left_join(offset_program_lookup, by = c("offset_program_name" = "original_name"))

# ---------------------------
# 4b. Diagnostics on unmapped programs
# ---------------------------

# View unmapped programs and study titles where they occur
unmapped_programs <- program_standardized %>%
  filter(is.na(standardized_name)) %>%
  select(study_title, offset_program_name) %>%
  distinct() %>%
  arrange(offset_program_name, study_title)

# Count how many times each unmapped program occurs
unmapped_program_counts <- program_standardized %>%
  filter(is.na(standardized_name)) %>%
  count(offset_program_name, sort = TRUE)

# Compare raw vs. trimmed names to detect potential whitespace issues
program_exploded_raw <- data %>%
  select(row_id, study_title, offset_program_name) %>%
  mutate(offset_program_name = str_split(offset_program_name, ";")) %>%
  unnest_longer(offset_program_name)

raw_vs_trimmed <- program_exploded_raw %>%
  mutate(raw_name = offset_program_name,
         cleaned_name = str_trim(offset_program_name)) %>%
  filter(raw_name != cleaned_name)

# ---------------------------
# 5. Output results or warnings
# ---------------------------

if (nrow(unmapped_programs) > 0) {
  warning("⚠️ Some program names were not standardized. See `unmapped_programs` and `unmapped_program_counts`.")
  print(unmapped_program_counts)
} else {
  message("✅ All program names successfully standardized.")
}

# (Optional) Save lookup for reuse
write_csv(offset_program_lookup, here("data", "reference", "offset_program_lookup.csv"))
