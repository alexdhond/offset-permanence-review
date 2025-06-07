# ===============================================
# Script:     ref_project_type_lookup.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Create a lookup table for mapping project types
# ===============================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # Read Excel files
library(tidyverse) # data manipulation
library(here) # construct relative file paths for easier saving
library(janitor) # clean column names 

# ---------------------------
# 2. Load and preprocess data
# ---------------------------

# Load data data in and clean column names
data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() # Clean column names 

# Check for missing or incomplete ecosystem specific and broad pairings
incomplete_project_rows <- data %>%
  filter(
    (is.na(project_type_specific) & !is.na(project_type_broad)) |
      (!is.na(project_type_specific) & is.na(project_type_specific))
  )

# View if any problematic rows exist
if (nrow(incomplete_project_rows) > 0) {
  warning("Some rows have inconsistent project information (intervention types missing).")
  print(incomplete_project_rows)
} else {
  message("All entries have corresponding type ")
}

# ---------------------------
# 3. Ecological intervention categories
# ---------------------------

# create data frame and description of interventions
eco_interventions <- tibble(
  ecological_intervention = c(
    "Creation",
    "Restoration",
    "Protection",
    "Enhancement",
    "Ecosystem Management",
    "Species Management",
    "Engineering Approaches",
    "Miscellaneous Compensation"
  ),
  description = c(
    "Establish new habitat (wetland creation, pond creation, afforestation, etc)",
    "Restore ecosystem (reforestation, stream restoration, wetland restoration)",
    "Prevent degradation (avoided deforestation, habitat protection, preservation)",
    "Improve ecosystem function (habitat enhancement, planting, stream enhancement)",
    "Ongoing ecosystem management actions (fire management, sustainable forest management, grazing management)",
    "Direct species actions (translocation, nest boxes, predator control)",
    "Use of engineering approaches to alter ecosystem (breakwater, bank stabilization, channel design)",
    "Other offset interventions or compensation actions that don’t fit well elsewhere (payments, random)"
))

# ---------------------------
# 4. Define specific intervention terms by category
# ---------------------------

# Creation terms
creation_terms <- c(
  "wetland creation",
  "pond creation",
  "meadow creation",
  "vernal pool creation",
  "species introduction",
  "micro-habitat creation",
  "linear plantation creation",
  "habitat creation",
  "wetland replication",
  "afforestation",
  "habitat establishment",
  "temporary pool creation",
  "plantation establishment"
)

# Restoration terms
restoration_terms <- c(
  "wetland restoration",
  "stream restoration",
  "habitat restoration",
  "river restoration",
  "forest restoration",
  "grassland restoration",
  "lake restoration",
  "reforestation",
  "restoration",
  "vegetation regeneration",
  "seagrass restoration",
  "human-induced regeneration",
  "habitat rehabilitation",
  "brook revitalization",
  "forest regeneration",
  "peatland rewetting",
  "assisted natural regeneration",
  "forest restoration treatments"
)

# Protection terms
protection_terms <- c(
  "avoided deforestation",
  "averted loss",
  "forest protection",
  "habitat protection",
  "wetland preservation",
  "stream preservation",
  "wetland protection",
  "mangrove protection",
  "forest conservation",
  "avoided mangrove deforestation",
  "avoided deforestation/forest conservation",
  "avoided conversion",
  "preservation",
  "avoided loss"
)

# Enhancement terms
enhancement_terms <- c(
  "wetland enhancement",
  "stream enhancement",
  "habitat enhancement",
  "vegetation enhancement",
  "enrichment planting",
  "environmental plantings",
  "tree planting"
)

# Ecosystem Management terms
ecosystem_mgmt_terms <- c(
  "fuel reduction",
  "fire management",
  "prescribed burning",
  "mechanical thinning",
  "sustainable forest management",
  "improved forest management",
  "forest management",
  "grazing management",
  "community-based forest management",
  "prescribed fire",
  "habitat management",
  "habitat maintenance",
  "controlled fire management",
  "savanna burning",
  "thinning",
  "agroforestation",
  "agroforestry"
  

)

# Species Management terms
species_mgmt_terms <- c(
  "translocation",
  "species translocation",
  "nest box installation",
  "disease control",
  "predator control"
)

# Engineering Approaches terms
engineering_terms <- c(
  "breakwater",
  "sediment tubes",
  "vessel removal",
  "natural channel design",
  "channel design",
  "bank stabilization",
  "sediment modification",
  "sediment removal",
  "transplant",
  "vessel damage repair",
  "managed realignment",
  "sediment fill"
)

# Miscellaneous Compensation terms
misc_comp_terms <- c(
  "compensation payments",
  "planting",
  "threat management",
  "weed control",
  "pest control",
  "land conversion"
)

# ---------------------------
# 5. Build lookup table from defined categories
# ---------------------------

# Match intervention categories to list of terms
intervention_categories <- list(
  Creation = creation_terms,
  Restoration = restoration_terms,
  Protection = protection_terms,
  Enhancement = enhancement_terms,
  `Ecosystem Management` = ecosystem_mgmt_terms,
  `Species Management` = species_mgmt_terms,
  `Engineering Approaches` = engineering_terms,
  `Miscellaneous Compensation` = misc_comp_terms
)

# Map to interventions and make dataframe
lookup_intervention_type <- purrr::map2_dfr(
  names(intervention_categories),
  intervention_categories,
  ~ tibble(
    project_type_specific = .y,
    intervention_type = .x
  )
)

# ---------------------------
# 6. Check for unmapped terms
# ---------------------------

# Join raw data to the lookup
project_mapped <- data %>%
  separate_rows(project_type_specific, sep = ";\\s*") %>%
  left_join(lookup_intervention_type, by = "project_type_specific") %>%
  mutate(
    mapping_status = case_when(
      is.na(intervention_type) ~ "unmapped",
      TRUE ~ "mapped"
    )
  )

# List unmapped rows
unmapped_project_types <- project_mapped %>%
  filter(is.na(intervention_type)) %>%
  distinct(project_type_specific) %>%
  arrange(project_type_specific)

# Find any duplicates 
lookup_intervention_type %>%
  count(project_type_specific, sort = TRUE) %>%
  filter(n > 1)

# ---------------------------
# 7. Save project type lookup table
# ---------------------------

# Save to directory, make one if does not exist
output_dir <- here("data", "reference")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# save file to path and write to csv
output_path <- file.path(output_dir, "project_intervention_type_lookup.csv")
write_csv(lookup_intervention_type, output_path)

