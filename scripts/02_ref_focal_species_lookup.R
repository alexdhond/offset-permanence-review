# ================================================================
# Script:     ref_focal_species_lookup.R
# Date:       2025-06-09
# Author:     Alex Dhond
# Purpose:    Extract and standardize focal species entries from
#             the main database. Create a lookup table and identify
#             unmapped species for manual review.
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)      # Read Excel files
library(tidyverse)   # Data wrangling
library(janitor)     # Clean column names
library(here)        # Project-rooted file paths

# ---------------------------
# 2. Load and prepare data
# ---------------------------

data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(row_id = row_number())

# ---------------------------
# 3. Extract and explode focal species
# ---------------------------

species_exploded <- data %>%
  select(row_id, focal_species) %>%
  separate_rows(focal_species, sep = ";\\s*") %>%
  mutate(focal_species = str_trim(focal_species)) %>%
  filter(focal_species != "", !is.na(focal_species)) %>%
  distinct()

# ---------------------------
# 4. Normalize known synonyms or variants
# ---------------------------

species_exploded <- species_exploded %>%
  mutate(focal_species = case_when(
    focal_species == "wood frog (Rana sylvatica)" ~ "wood frog (Lithobates sylvaticus)",
    focal_species == "red-tailed black-cockatoo (Calyptorhynchus banksii graptogyne)" ~ 
      "red-tailed black-cockatoo (Calyptorhynchus banksii)",
    TRUE ~ focal_species
  ))

# ---------------------------
# 5. Build initial species lookup
# ---------------------------

species_lookup <- species_exploded %>%
  distinct(focal_species) %>%
  mutate(
    focal_species_standard_common_name = NA_character_,
    focal_species_standard_scientific_name = NA_character_,
    focal_species_taxonomic_group = NA_character_,
    mapping_status = "unmapped"
  )

# ---------------------------
# 6. Define manual mappings (editable block)
# ---------------------------

manual_mappings <- tribble(
  ~focal_species, ~focal_species_standard_common_name, ~focal_species_standard_scientific_name, ~focal_species_taxonomic_group,
  "wood frog (Rana sylvatica)",                      "Wood Frog",                     "Rana sylvatica",              "Amphibian",
  "mole salamander (Ambystoma spp.)",                "Mole Salamander (genus)",       "Ambystoma spp.",              "Amphibian",
  "fairy shrimp (Order Chirocephalidae)",            "Fairy Shrimp",                  "Chirocephalidae (order)",     "Crustacean",
  "great crested newt (Triturus cristatus)",         "Great Crested Newt",            "Triturus cristatus",          "Amphibian",
  "spotted salamander (Ambystoma maculatum)",        "Spotted Salamander",            "Ambystoma maculatum",         "Amphibian",
  "green frog (Rana clamitans)",                     "Green Frog",                    "Rana clamitans",              "Amphibian",
  "red-tailed black-cockatoo (Calyptorhynchus banksii)", "Red-tailed Black Cockatoo", "Calyptorhynchus banksii",     "Bird",
  "buloke (Allocasuarina luehmannii)",               "Buloke",                        "Allocasuarina luehmannii",    "Plant",
  "gamba grass (andropogon gayanus)",                "Gamba Grass",                   "Andropogon gayanus",          "Plant",
  "wood frog (Lithobates sylvaticus)",               "Wood Frog",                     "Lithobates sylvaticus",       "Amphibian",
  "squirrel glider (Petaurus norfolcensis)",         "Squirrel Glider",               "Petaurus norfolcensis",       "Mammal",
  "brown treecreeper (Climacteris picumnus)",        "Brown Treecreeper",             "Climacteris picumnus",        "Bird",
  "superb parrot (Polytelis swainsonii)",            "Superb Parrot",                 "Polytelis swainsonii",        "Bird",
  "koala (Phascolarctos cinereus)",                  "Koala",                         "Phascolarctos cinereus",      "Mammal",
  "green and golden bell frog (Litoria aurea)",      "Green and Golden Bell Frog",    "Litoria aurea",               "Amphibian",
  "Powerful Owl (Ninox strenua)",                    "Powerful Owl",                  "Ninox strenua",               "Bird",
  "northern brown bandicoot (Isoodon macrourus)",    "Northern Brown Bandicoot",      "Isoodon macrourus",           "Mammal",
  "tiger quoll (Dasyurus maculatus)",                "Tiger Quoll",                   "Dasyurus maculatus",          "Mammal",
  "yellow-bellied glider (Petaurus australis)",      "Yellow-bellied Glider",         "Petaurus australis",          "Mammal"
)

# ---------------------------
# 7. Join and finalize lookup table
# ---------------------------

joined <- species_lookup %>%
  left_join(manual_mappings, by = "focal_species")

names(joined)

species_lookup <- joined %>%
  mutate(
    standard_common_name = coalesce(focal_species_standard_common_name.y, focal_species_standard_common_name.x),
    standard_scientific_name = coalesce(focal_species_standard_scientific_name.y, focal_species_standard_scientific_name.x),
    taxonomic_group = coalesce(focal_species_taxonomic_group.y, focal_species_taxonomic_group.x),
    mapping_status = if_else(!is.na(standard_scientific_name), "mapped", "unmapped")
  ) %>%
  select(focal_species, standard_common_name, standard_scientific_name, taxonomic_group, mapping_status)

# ---------------------------
# 8. Export finalized lookup table
# ---------------------------

output_dir <- here("data", "reference")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(species_lookup, file.path(output_dir, "focal_species_lookup.csv"))

