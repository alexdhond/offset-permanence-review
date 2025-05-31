# ================================================================
# Script:     ref_ecosystem_type_lookup.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Load latest dataset, extract and save lookup table
#             for ecosystem_type_specific and ecosystem_type_broad
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # Read Excel files
library(tidyverse) # Data manipulation (includes dplyr, tidyr)
library(here)      # Reproducible file paths
library(janitor)   # clean column names

# ---------------------------
# 2. Load and preprocess data
# ---------------------------

# Load data data in and clean columns
data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() # Clean column names 

# Select only ecosystem columns
ecosystem <- data %>%
  select(ecosystem_type_specific, ecosystem_type_broad)

# Explode and filter columns to get all possible combinations (will have incorrect combos)
df_ecosystem <- ecosystem %>%
  separate_rows(ecosystem_type_specific, sep = ";\\s*") %>%
  separate_rows(ecosystem_type_broad, sep = ";\\s*") %>%
  filter(if_all(everything(), ~ !is.na(.) & . != "")) %>%
  distinct() %>%
  arrange(ecosystem_type_broad, ecosystem_type_specific)

# ---------------------------
# 3. Extract unique broad and specific ecosystem values
# ---------------------------

# Specific ecosystem
eco_spec_df <- df_ecosystem %>%
  distinct(ecosystem_type_specific) %>%
  arrange(ecosystem_type_specific)

# Broad ecosystem
eco_broad_df <- df_ecosystem %>%
  distinct(ecosystem_type_broad) %>%
  arrange(ecosystem_type_broad)

# ---------------------------
# 4. Broad ecosystem categories
# ---------------------------

# The data in my database is of various quality, so I am going to assign a broad habitat classification

# create data frame and description
broad_ecosystems <- tibble(
  broad_ecosystem = c(
    "Forest",
    "Wetland",
    "Grassland",
    "Shrubland",
    "Coastal/Marine",
    "Freshwater",
    "Agricultural"
    ),
  description = c(
    "All forest types (tropical, temperate, boreal, montane, etc)",
    "All palustrine, floodplain, marsh, bog, fen, peatland, and vernal pool types",
    "Grasslands, savannas, pastures, meadows, etc",
    "Heathlands, bushlands, chaparral",
    "Coastal and blue carbon ecosystems (seagrass, coral reefs, estuaries, mangroves, saltmarshes, tidal flats, etc)",
    "Rivers, streams, lakes (non-wetland), springs",
    "Croplands, plantations, agroforests, shifting cultivation")
)

# Map original labels from my database to standardized categories
broad_ecosystems_mapped <- eco_broad_df %>%
  mutate(
    broad_ecosystem = case_when(
      ecosystem_type_broad %in% c("agricultural", "agriculture") ~ "Agricultural",
      ecosystem_type_broad == "coastal/marine" ~ "Coastal/Marine",
      ecosystem_type_broad == "forest" ~ "Forest",
      ecosystem_type_broad == "grassland" ~ "Grassland",
      ecosystem_type_broad == "shrubland" ~ "Shrubland",
      ecosystem_type_broad == "wetland" ~ "Wetland",
      ecosystem_type_broad == "river/stream" ~ "Freshwater",
      ecosystem_type_broad == "terrestrial" ~ NA_character_,  # Too vague – decide how you want to treat it
      TRUE ~ NA_character_  # fallback for anything unexpected
    )
  ) %>%
  left_join(broad_ecosystems, by = "broad_ecosystem")

# Join and flag unmapped types
broad_ecosystems_mapped <- broad_ecosystems_mapped %>%
  mutate(
    mapping_status = case_when(
      is.na(broad_ecosystem) ~ "unmapped",
      TRUE ~ "mapped"
    )
  )

# ---------------------------
# 5. Specific ecosystem categories
# ---------------------------

# I need to map the specific categories I extracted to the broad categories
# First create a series of lists of the terms

# Forest terms
forest_terms <- c(
  "dense forest",
  "lowland humid tropical forest",
  "natural forest",
  "secondary forest",
  "timberland",
  "forest",
  "woodland",
  "Amazon forest",
  "Amazonian forest",
  "Atlantic Forest",
  "buloke woodland",
  "eucalypt forest",
  "evergreen forest",
  "humid tropical forest",
  "liana forest",
  "logged forest",
  "mixed conifer-hardwood forest",
  "rainforest",
  "pine forest",
  "tropical rain forest",
  "tropical rainforest",
  "tropical forest",
  "tropical evergreen forest",
  "tropical high forest",
  "spruce-fir forest",
  "semi-evergreen forest",
  "primary forest",
  "northeast conifer forest",
  "northern hardwood forest",
  "conifer forest",
  "deciduous forest",
  "degraded forest",
  "dipterocarp forest",
  "dry deciduous forest",
  "dryland forest",
  "eastern hemlock forest",
  "lowland evergreen forest",
  "lowland forest",
  "lowland rain forest",
  "lowland tropical forest",
  "miombo woodland",
  "mixed liana forest",
  "mossy forest",
  "muyong forest",
  "burned forest",
  "plantation forest",
  "paperbark woodland",
  "seasonally dry forest",
  "semideciduous forest",
  "short inundated forest",
  "tall inundated forest",
  "temperate eucalypt box gum grassy woodland",
  "woody vegetation",
  "upland forest",
  "tall evergreen forest",
  "scattered trees",
  "riverine forest",
  "riparian forest habitat"
)

# Wetland terms
wetland_terms <- c(
  "woodland pond",
  "wooded wetland",
  "wet meadow",
  "temporary freshwater pool",
  "sedge meadow/wet prairie",
  "pothole",
  "scrub/shrub wetland",
  "seasonal pond",
  "oper-water/submergent wetland",
  "open herbaceous wetland",
  "forested seasonal pool",
  "hardwood wetland",
  "marsh",
  "created seasonal pool",
  "depressional emergent marsh",
  "depressional wetland",
  "emergent marsh",
  "artifical wetland",
  "mudflat",
  "pond",
  "peatland",
  "vernal pool",
  "created wetland",
  "forested wetland",
  "freshwater marsh",
  "freshwater wetland",
  "freshwater grazing marsh",
  "emergent wetland",
  "wetland",
  "floodplain mosaic",
  "floodplain wetland",
  "mesotrophic wetland",
  "floodplain forest",
  "ephemeral pond",
  "vernal pool habitat",
  "water-dependent habitat",
  "seasonal wetland",
  "shrub wetland",
  "perennial freshwater wetland",
  "non-riparian wetland",
  "palustrine aquatic bed",
  "palustrine shore",
  "riparian wetland",
  "unidentified wetland"
)

# Grassland terms
grassland_terms <- c(
  "grassland",
  "mesotrophic meadow",
  "oligotrophic dry grassland",
  "open grassland",
  "tropical savanna",
  "rupestrian grassland",
  "prairie",
  "meadow",
  "mesic prairie buffer",
  "savanna",
  "tall herb field",
  "mesotrophic pasture",
  "rangeland"
)

# Shrubland terms
shrubland_terms <- c(
  "shrubland",
  "hedge",
  "dwarf shrub heath",
  "chaparral scrub",
  "coastal sage"
)

# Coastal/Marine terms
coastal_marine_terms <- c(
  "salt marsh",
  "salt-marsh",
  "saltmarsh",
  "saltwater marsh",
  "mangrove forest",
  "seagrass meadow",
  "tidal marsh",
  "estuarine",
  "estuarine habitat",
  "coastal wetland",
  "transitional brackish habitat",
  "mussel bed",
  "intertidal habitat",
  "marine habitat",
  "estuarine island",
  "intertidal wetland"
)

# Freshwater terms
freshwater_terms <- c(
  "river",
  "riparian corridor",
  "river band",
  "lower riverine bottom",
  "riverine",
  "lacustrine bottom",                         
  "lacustrine habitat",
  "lacustrine shore",
  "lake",
  "brook",
  "stream",
  "riparian habitat",
  "riparian",
  "riparian woodland",
  "riparian forest",
  "waterbody",
  "perennial stream",
  "in-channel habitat",
  "off-channel habitat"
  
)

# Agricultural terms
agricultural_terms <- c(
  "agricultural land",
  "arable land",
  "grain cropping land",
  "non-cropped land",
  "oil palm plantation",
  "plantation crops",
  "rubber agroforest",
  "shifting cultivation",
  "pasture",
  "greenfield",
  "terrace",
  "linear plantation"
  
)

# ---------------------------
# 6. Create lookup table for specific to broad categories
# ---------------------------

# Create a named vector for broad ecosystem categories and terms
ecosystem_broad_categories <- list(
  Forest = forest_terms,
  Wetland = wetland_terms,
  Grassland = grassland_terms,
  Shrubland = shrubland_terms,
  `Coastal/Marine` = coastal_marine_terms,
  Agricultural = agricultural_terms,
  Freshwater = freshwater_terms
)

# Build lookup table
lookup_specific_to_broad <- map2_dfr(
  names(ecosystem_broad_categories),
  ecosystem_broad_categories,
  ~ tibble(
    ecosystem_type_specific = .y,
    broad_ecosystem = .x
  )
)

# ---------------------------
# 7. Check for unmapped specific terms
# ---------------------------

# select all terms
observed_specific_terms <- df_ecosystem %>%
  select(ecosystem_type_specific) %>%
  separate_rows(ecosystem_type_specific, sep = ";\\s*") %>%
  distinct() %>%
  filter(!is.na(ecosystem_type_specific), ecosystem_type_specific != "")

# find unmapped terms
unmapped_terms <- observed_specific_terms %>%
  anti_join(lookup_specific_to_broad, by = "ecosystem_type_specific")

# View duplicates in lookup
lookup_specific_to_broad %>%
  count(ecosystem_type_specific, sort = TRUE) %>%
  filter(n > 1)

# ---------------------------
# 8. Apply lookup to dataset
# ---------------------------

data_ecosystem_mapped <- data %>%
  select(study_title, ecosystem_type_specific) %>%
  separate_rows(ecosystem_type_specific, sep = ";\\s*") %>%
  left_join(lookup_specific_to_broad, by = "ecosystem_type_specific")


#####




# ---------------------------
# 9. OPTIONAL: Create IUCN typology to match ecosystems
# ---------------------------

# Using the IUCN Global Ecosystem Typology 2.0 to try and classify the biomes, to provide more resolution on what offsets exist

# Create the typology from https://portals.iucn.org/library/sites/library/files/documents/2020-037-En.pdf
iucn_typology <- tribble(
  ~Realm, ~Biome_Code, ~Biome, ~EFG_Code, ~EFG_Name,
  "TERRESTRIAL", "T1", "Tropical-subtropical forests", "T1.1", "Tropical-subtropical lowland rainforests",
  "TERRESTRIAL", "T1", "Tropical-subtropical forests", "T1.2", "Tropical-subtropical dry forests and thickets",
  "TERRESTRIAL", "T1", "Tropical-subtropical forests", "T1.3", "Tropical-subtropical montane rainforests",
  "TERRESTRIAL", "T1", "Tropical-subtropical forests", "T1.4", "Tropical heath forests",
  "TERRESTRIAL", "T2", "Temperate-boreal forests & woodlands", "T2.1", "Boreal and temperate high montane forests and woodlands",
  "TERRESTRIAL", "T2", "Temperate-boreal forests & woodlands", "T2.2", "Deciduous temperate forests",
  "TERRESTRIAL", "T2", "Temperate-boreal forests & woodlands", "T2.3", "Oceanic cool temperate rainforests",
  "TERRESTRIAL", "T2", "Temperate-boreal forests & woodlands", "T2.4", "Warm temperate laurophyll forests",
  "TERRESTRIAL", "T2", "Temperate-boreal forests & woodlands", "T2.5", "Temperate pyric humid forests",
  "TERRESTRIAL", "T2", "Temperate-boreal forests & woodlands", "T2.6", "Temperate pyric sclerophyll forests and woodlands",
  "TERRESTRIAL", "T3", "Shrublands & shrubby woodlands", "T3.1", "Seasonally dry tropical shrublands",
  "TERRESTRIAL", "T3", "Shrublands & shrubby woodlands", "T3.2", "Seasonally dry temperate heaths and shrublands",
  "TERRESTRIAL", "T3", "Shrublands & shrubby woodlands", "T3.3", "Cool temperate heathlands",
  "TERRESTRIAL", "T3", "Shrublands & shrubby woodlands", "T3.4", "Rocky pavements, screes and lava flows",
  "TERRESTRIAL", "T4", "Savannas and grasslands", "T4.1", "Trophic savannas",
  "TERRESTRIAL", "T4", "Savannas and grasslands", "T4.2", "Pyric tussock savannas",
  "TERRESTRIAL", "T4", "Savannas and grasslands", "T4.3", "Hummock savannas",
  "TERRESTRIAL", "T4", "Savannas and grasslands", "T4.4", "Temperate woodlands",
  "TERRESTRIAL", "T4", "Savannas and grasslands", "T4.5", "Temperate subhumid grasslands",
  "TERRESTRIAL", "T5", "Deserts and semi-deserts", "T5.1", "Semi-desert steppes",
  "TERRESTRIAL", "T5", "Deserts and semi-deserts", "T5.2", "Thorny deserts and semi-deserts",
  "TERRESTRIAL", "T5", "Deserts and semi-deserts", "T5.3", "Sclerophyll hot deserts and semi-deserts",
  "TERRESTRIAL", "T5", "Deserts and semi-deserts", "T5.4", "Cool deserts and semi-deserts",
  "TERRESTRIAL", "T5", "Deserts and semi-deserts", "T5.5", "Hyper-arid deserts",
  "TERRESTRIAL", "T6", "Polar-alpine", "T6.1", "Ice sheets, glaciers and perennial snowfields",
  "TERRESTRIAL", "T6", "Polar-alpine", "T6.2", "Polar-alpine rocky outcrops",
  "TERRESTRIAL", "T6", "Polar-alpine", "T6.3", "Polar tundra and deserts",
  "TERRESTRIAL", "T6", "Polar-alpine", "T6.4", "Temperate alpine grasslands and shrublands",
  "TERRESTRIAL", "T6", "Polar-alpine", "T6.5", "Tropical alpine grasslands and shrublands",
  "TERRESTRIAL", "T7", "Intensive land-use systems", "T7.1", "Annual croplands",
  "TERRESTRIAL", "T7", "Intensive land-use systems", "T7.2", "Sown pastures and fields",
  "TERRESTRIAL", "T7", "Intensive land-use systems", "T7.3", "Plantations",
  "TERRESTRIAL", "T7", "Intensive land-use systems", "T7.4", "Urban and industrial ecosystems",
  "TERRESTRIAL", "T7", "Intensive land-use systems", "T7.5", "Derived semi-natural pastures and oldfields",
  "SUBTERRANEAN", "S1", "Subterranean lithic systems", "S1.1", "Aerobic caves",
  "SUBTERRANEAN", "S1", "Subterranean lithic systems", "S1.2", "Endolithic systems",
  "SUBTERRANEAN", "S1", "Subterranean lithic systems", "S2.1", "Anthropogenic subterranean voids",
  "SUBTERRANEAN-FRESHWATER", "SF1", "Subterranean freshwaters", "SF1.1", "Underground streams and pools",
  "SUBTERRANEAN-FRESHWATER", "SF1", "Subterranean freshwaters", "SF1.2", "Groundwater ecosystems",
  "SUBTERRANEAN-FRESHWATER", "SF1", "Subterranean freshwaters", "SF2.1", "Water pipes and subterranean canals",
  "SUBTERRANEAN-FRESHWATER", "SF1", "Subterranean freshwaters", "SF2.2", "Flooded mines and other voids",
  "SUBTERRANEAN-MARINE", "SM1", "Subterranean tidal systems", "SM3.1", "Anchialine caves",
  "SUBTERRANEAN-MARINE", "SM1", "Subterranean tidal systems", "SM3.2", "Anchialine pools",
  "SUBTERRANEAN-MARINE", "SM1", "Subterranean tidal systems", "SM3.1", "Sea caves",
  "FRESHWATER-TERRESTRIAL", "TF1", "Palustrine wetlands", "TF1.1", "Tropical flooded forests and peat forests",
  "FRESHWATER-TERRESTRIAL", "TF1", "Palustrine wetlands", "TF1.2", "Subtropical/temperate forested wetlands",
  "FRESHWATER-TERRESTRIAL", "TF1", "Palustrine wetlands", "TF1.3", "Permanent marshes",
  "FRESHWATER-TERRESTRIAL", "TF1", "Palustrine wetlands", "TF1.4", "Seasonal floodplain marshes",
  "FRESHWATER-TERRESTRIAL", "TF1", "Palustrine wetlands", "TF1.5", "Episodic arid floodplains",
  "FRESHWATER-TERRESTRIAL", "TF1", "Palustrine wetlands", "TF1.6", "Boreal, temperate and montane peat bogs",
  "FRESHWATER-TERRESTRIAL", "TF1", "Palustrine wetlands", "TF1.7", "Boreal and temperate fens",
  "FRESHWATER", "F1", "Rivers and streams", "F1.1", "Permanent upland streams",
  "FRESHWATER", "F1", "Rivers and streams", "F1.2", "Permanent lowland rivers",
  "FRESHWATER", "F1", "Rivers and streams", "F1.3", "Freeze-thaw rivers and streams",
  "FRESHWATER", "F1", "Rivers and streams", "F1.4", "Seasonal upland streams",
  "FRESHWATER", "F1", "Rivers and streams", "F1.5", "Seasonal lowland rivers",
  "FRESHWATER", "F1", "Rivers and streams", "F1.6", "Episodic arid rivers",
  "FRESHWATER", "F1", "Rivers and streams", "F1.7", "Large lowland rivers",
  "FRESHWATER", "F2", "Lakes", "F2.1", "Large permanent freshwater lakes",
  "FRESHWATER", "F2", "Lakes", "F2.2", "Small permanent freshwater lakes",
  "FRESHWATER", "F2", "Lakes", "F2.3", "Seasonal freshwater lakes",
  "FRESHWATER", "F2", "Lakes", "F2.4", "Freeze-thaw freshwater lakes",
  "FRESHWATER", "F2", "Lakes", "F2.5", "Ephemeral freshwater lakes",
  "FRESHWATER", "F2", "Lakes", "F2.6", "Permanent salt and soda lakes",
  "FRESHWATER", "F2", "Lakes", "F2.7", "Ephemeral salt lakes",
  "FRESHWATER", "F2", "Lakes", "F2.8", "Artesian springs and oases",
  "FRESHWATER", "F2", "Lakes", "F2.9", "Geothermal pools and wetlands",
  "FRESHWATER", "F2", "Lakes", "F2.10", "Subglacial lakes",
  "FRESHWATER", "F3", "Artificial fresh waters", "F3.1", "Large reservoirs",
  "FRESHWATER", "F3", "Artificial fresh waters", "F3.2", "Constructed lacustrine wetlands",
  "FRESHWATER", "F3", "Artificial fresh waters", "F3.3", "Rice paddies",
  "FRESHWATER", "F3", "Artificial fresh waters", "F3.4", "Freshwater aquafarms",
  "FRESHWATER", "F3", "Artificial fresh waters", "F3.5", "Canals, ditches and drains",
  "FRESHWATER-MARINE", "FM1", "Semi-confined transitional waters", "FM1.1", "Deepwater coastal inlets",
  "FRESHWATER-MARINE", "FM1", "Semi-confined transitional waters", "FM1.2", "Permanently open riverine estuaries and bays",
  "FRESHWATER-MARINE", "FM1", "Semi-confined transitional waters", "FM1.3", "Intermittently closed and open lakes and lagoons",
  "MARINE", "M", "Marine shelfs", "M1.1", "Seagrass meadows",
  "MARINE", "M", "Marine shelfs", "M1.2", "Kelp forests",
  "MARINE", "M", "Marine shelfs", "M1.3", "Photic coral reefs",
  "MARINE", "M", "Marine shelfs", "M1.4", "Shellfish beds and reefs",
  "MARINE", "M", "Marine shelfs", "M1.5", "Photo-limited marine animal forests",
  "MARINE", "M", "Marine shelfs", "M1.6", "Subtidal rocky reefs",
  "MARINE", "M", "Marine shelfs", "M1.7", "Subtidal sand beds",
  "MARINE", "M", "Marine shelfs", "M1.8", "Subtidal mud plains",
  "MARINE", "M", "Marine shelfs", "M1.9", "Upwelling zones",
  "MARINE", "M2", "Pelagic ocean waters", "M2.1", "Epipelagic ocean waters",
  "MARINE", "M2", "Pelagic ocean waters", "M2.2", "Mesopelagic ocean waters",
  "MARINE", "M2", "Pelagic ocean waters", "M2.3", "Bathypelagic ocean waters",
  "MARINE", "M2", "Pelagic ocean waters", "M2.4", "Abyssopelagic ocean waters",
  "MARINE", "M2", "Pelagic ocean waters", "M2.5", "Sea ice",
  "MARINE", "M3", "Deep sea floors", "M3.1", "Continental and island slopes",
  "MARINE", "M3", "Deep sea floors", "M3.2", "Marine canyons",
  "MARINE", "M3", "Deep sea floors", "M3.3", "Abyssal plains",
  "MARINE", "M3", "Deep sea floors", "M3.4", "Seamounts, ridges and plateaus",
  "MARINE", "M3", "Deep sea floors", "M3.5", "Deepwater biogenic beds",
  "MARINE", "M3", "Deep sea floors", "M3.6", "Hadal trenches and troughs",
  "MARINE", "M3", "Deep sea floors", "M3.7", "Chemosynthetically-based ecosystems",
  "MARINE", "M4", "Anthropogenic marine systems", "M4.1", "Submerged artificial structures",
  "MARINE", "M4", "Anthropogenic marine systems", "M4.2", "Marine aquafarms",
  "MARINE-TERRESTRIAL", "MT1", "Shoreline systems", "MT1.1", "Rocky shores",
  "MARINE-TERRESTRIAL", "MT1", "Shoreline systems", "MT1.2", "Muddy shores",
  "MARINE-TERRESTRIAL", "MT1", "Shoreline systems", "MT1.3", "Sandy shores",
  "MARINE-TERRESTRIAL", "MT1", "Shoreline systems", "MT1.4", "Boulder and cobble shores",
  "MARINE-TERRESTRIAL", "MT1", "Shoreline systems", "MT2.1", "Coastal shrublands and grasslands",
  "MARINE-TERRESTRIAL", "MT1", "Shoreline systems", "MT3.1", "Artificial shores",
  "MARINE-FRESHWATER-TERRESTRIAL", "MFT1", "Brackish tidal systems", "MFT1.1", "Coastal river deltas",
  "MARINE-FRESHWATER-TERRESTRIAL", "MFT1", "Brackish tidal systems", "MFT1.2", "Intertidal forests and shrublands",
  "MARINE-FRESHWATER-TERRESTRIAL", "MFT1", "Brackish tidal systems", "MFT1.3", "Coastal saltmarshes and reedbeds",
)

# ---------------------------
# 5. Manually match broad terms to IUCN list
# ---------------------------

# Create lookup table: specific ecosystem IUCN Biome

lookup_specific_to_iucn_biome <- tibble(
  ecosystem_type_specific = c(
    
    # tropical forest terms
    
    "lowland humid tropical forest",
    "tall evergreen forest",
    "upland forest",
    "tropical rainforest",
    "miombo woodland",
    "pine forest",
    "boreal forest",
    
    
    
    
    # Wetland-related
    "vernal pool",
    "peatland",
    "seasonal pond",
    "forested wetland",
    
    # Coastal/marine
    "salt marsh",
    "mangrove forest",
    "seagrass meadow",
    "tidal marsh",
    
    # Grassland/Savanna
    "prairie",
    "tropical savanna",
    "grassland",
    
    # Freshwater
    "riverine",
    "perennial stream",
    "lacustrine shore"
  ),
  Biome = c(
    # Forest
    "Tropical-subtropical forests",
    "Tropical-subtropical forests",
    "Temperate-boreal forests & woodlands",
    "Tropical-subtropical forests",
    "Savannas and grasslands",
    "Temperate-boreal forests & woodlands",
    "Temperate-boreal forests & woodlands",
    
    # Wetland
    "Palustrine wetlands",
    "Palustrine wetlands",
    "Palustrine wetlands",
    "Palustrine wetlands",
    
    # Coastal/marine
    "Marine shelfs",
    "Brackish tidal systems",
    "Marine shelfs",
    "Brackish tidal systems",
    
    # Grassland/Savanna
    "Savannas and grasslands",
    "Savannas and grasslands",
    "Savannas and grasslands",
    
    # Freshwater
    "Rivers and streams",
    "Rivers and streams",
    "Lakes"
  )
)

