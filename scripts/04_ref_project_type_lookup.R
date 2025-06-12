# ================================================================
# Script:     ref_project_type_lookup.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Create a lookup table for mapping project types
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # Read Excel files
library(tidyverse) # Data wrangling
library(here)      # Relative file paths
library(janitor)   # Clean column names

# ---------------------------
# 2. Load and prepare main data
# ---------------------------

data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(row_id = row_number())

# Check for incomplete pairings of project_type_broad and _specific
incomplete_project_rows <- data %>%
  filter(
    (is.na(project_type_specific) & !is.na(project_type_broad)) |
      (is.na(project_type_broad) & !is.na(project_type_specific))
  )

if (nrow(incomplete_project_rows) > 0) {
  warning("⚠️ Some rows have missing project type fields:")
  print(incomplete_project_rows)
} else {
  message("✅ All project type rows are complete.")
}

# ---------------------------
# 3. Define intervention categories
# ---------------------------

eco_interventions <- tibble(
  ecological_intervention = c(
    "Creation", "Restoration", "Protection", "Enhancement",
    "Ecosystem Management", "Species Management",
    "Engineering Approaches", "Miscellaneous Compensation"
  ),
  description = c(
    "Establish new habitat",
    "Restore degraded habitat",
    "Prevent degradation",
    "Improve function or quality",
    "Sustain ongoing ecosystem processes",
    "Act directly on species",
    "Use engineering to modify environment",
    "Other offset/compensation actions"
  )
)

# ---------------------------
# 4. Define intervention terms
# ---------------------------

creation_terms <- c(
  "wetland creation", "pond creation", "meadow creation", "vernal pool creation",
  "species introduction", "micro-habitat creation", "linear plantation creation",
  "habitat creation", "wetland replication", "afforestation", "habitat establishment",
  "temporary pool creation", "plantation establishment", "wetland construction",
  "off-channel reservoir construction"
)

restoration_terms <- c(
  "wetland restoration", "stream restoration", "habitat restoration", "river restoration",
  "forest restoration", "grassland restoration", "lake restoration", "reforestation",
  "restoration", "vegetation regeneration", "seagrass restoration", "human-induced regeneration",
  "habitat rehabilitation", "brook revitalization", "forest regeneration", "peatland rewetting",
  "assisted natural regeneration", "forest restoration treatments"
)

protection_terms <- c(
  "avoided deforestation", "averted loss", "forest protection", "habitat protection",
  "wetland preservation", "stream preservation", "wetland protection", "mangrove protection",
  "forest conservation", "avoided mangrove deforestation", "avoided deforestation/forest conservation",
  "avoided conversion", "preservation", "avoided loss"
)

enhancement_terms <- c(
  "wetland enhancement", "stream enhancement", "habitat enhancement", "vegetation enhancement",
  "enrichment planting", "environmental plantings", "tree planting"
)

ecosystem_mgmt_terms <- c(
  "fuel reduction", "fire management", "prescribed burning", "mechanical thinning",
  "sustainable forest management", "improved forest management", "forest management",
  "grazing management", "community-based forest management", "prescribed fire",
  "habitat management", "habitat maintenance", "controlled fire management",
  "savanna burning", "thinning", "agroforestation", "agroforestry"
)

species_mgmt_terms <- c(
  "translocation", "species translocation", "nest box installation",
  "disease control", "predator control", "species conservation", "species management"
)

engineering_terms <- c(
  "breakwater", "sediment tubes", "vessel removal", "natural channel design", "channel design",
  "bank stabilization", "sediment modification", "sediment removal", "transplant",
  "vessel damage repair", "managed realignment", "sediment fill", "channel reconfiguration",
  "tidal regulation", "unmanaged realignment"
)

misc_comp_terms <- c(
  "compensation payments", "planting", "threat management", "weed control",
  "pest control", "land conversion"
)

# ---------------------------
# 5. Create lookup table
# ---------------------------

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

lookup_intervention_type <- purrr::map2_dfr(
  names(intervention_categories),
  intervention_categories,
  ~ tibble(
    project_type_specific = str_to_lower(str_trim(.y)),
    intervention_type = .x
  )
)

# ---------------------------
# 6. Map project types and flag unmapped terms
# ---------------------------

project_mapped <- data %>%
  separate_rows(project_type_specific, sep = ";\\s*") %>%
  mutate(project_type_specific = str_to_lower(str_trim(project_type_specific))) %>%
  left_join(lookup_intervention_type %>%
              mutate(project_type_specific = str_to_lower(str_trim(project_type_specific))),
            by = "project_type_specific") %>%
  mutate(
    mapping_status = if_else(is.na(intervention_type), "unmapped", "mapped")
  )

# ---------------------------
# 6b. Identify and review unmapped or NA values
# ---------------------------

# Unmapped but non-NA
unmapped_terms <- project_mapped %>%
  filter(mapping_status == "unmapped", !is.na(project_type_specific)) %>%
  distinct(project_type_specific) %>%
  arrange(project_type_specific)

# Rows causing unmapped status
unmapped_rows <- project_mapped %>%
  filter(project_type_specific %in% unmapped_terms$project_type_specific)

# Rows with missing specific project types
na_specific_rows <- project_mapped %>%
  filter(is.na(project_type_specific)) %>%
  select(study_id, study_title, everything())

# Review
print(unmapped_rows)
print(na_specific_rows)


# ---------------------------
# 7. Save cleaned lookup table
# ---------------------------

project_type_lookup_clean <- lookup_intervention_type %>%
  distinct(project_type_specific, intervention_type) %>%
  arrange(intervention_type, project_type_specific)

output_dir <- here("data", "reference")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(project_type_lookup_clean, file.path(output_dir, "project_intervention_type_lookup.csv"))
message("✅ Cleaned lookup table written: project_intervention_type_lookup.csv")
