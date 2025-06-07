# ===============================================
# Script:     ref_program_policy_lookup.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Clean and standardize policy/program names and build lookup table
# ===============================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # Read Excel files
library(tidyverse) # data manipulation
library(here)      # construct relative file paths
library(janitor)   # clean column names

# ---------------------------
# 2. Load and preprocess data
# ---------------------------

# Load data and clean column names
data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

# ---------------------------
# 3. Split semicolon-delimited fields
# ---------------------------

# Columns to split
cols_to_split <- c(
  "offset_project_name",
  "offset_program_name",
  "policy_legal_instrument_name",
  "year_of_policy_adoption",
  "policy_jurisdiction",
  "program_policy_note",
  "market_type"
)

# Un-nest (explode) all columns
data_long <- data %>%
  mutate(across(all_of(cols_to_split), ~ str_split(.x, ";\\s*"))) %>%
  unnest_longer(offset_project_name) %>%
  unnest_longer(offset_program_name) %>%
  unnest_longer(policy_legal_instrument_name) %>%
  unnest_longer(year_of_policy_adoption) %>%
  unnest_longer(policy_jurisdiction) %>%
  unnest_longer(program_policy_note) %>%
  unnest_longer(market_type)

# check unique values
unique(data_long$offset_project_name)
unique(data_long$offset_program_name)
unique(data_long$policy_legal_instrument_name)

# -----------------------------------------
# 4. Standardize program names
# -----------------------------------------

data_long <- data_long %>%
  mutate(
    offset_program_name_std = case_when(
      offset_program_name == "US Clean Water Act Section 404 Wetland and Stream Permitting" ~ "US Clean Water Act Section 404",
      offset_program_name == "US Wetland and Stream Mitigation Banking" ~ "US Mitigation Banking",
      offset_program_name == "Activities Implemented Jointly (AIJ) Pilot Phase" ~ "AIJ Pilot Phase",
      offset_program_name == "UNFCCC Clean Development Mechanism (CDM)" ~ "UNFCCC CDM",
      offset_program_name == "Canada Fisheries Act Section 35(2) Fish Habitat Authorization Program" ~ "Canada Fisheries Act Offsetting",
      offset_program_name == "NSW BioBanking Scheme" ~ "NSW BioBanking Scheme",
      offset_program_name == "UNFCCC REDD+ Mechanism" ~ "UNFCCC REDD+",
      offset_program_name %in% c("Voluntary Carbon Standard (VCS)", "Verified Carbon Standard", "Verra Verified Carbon Standard (VCS)", "Verified Carbon Standard (VCS)") ~ "Verra VCS",
      offset_program_name == "Victoria's Native Vegetation Management Framework" ~ "Victoria Native Vegetation Framework",
      offset_program_name == "German Compensation System (Eingriffsregelung)" ~ "Germany Eingriffsregelung",
      offset_program_name == "REDD/REDD+" ~ "REDD+",
      offset_program_name %in% c("Carbon Farming Initiative", "Australia Carbon Farming Initiative", "Carbon Farming Initiative (CFI)") ~ "Australia CFI",
      offset_program_name %in% c("California Air Resources Board (ARB) Compliance Offset Program", "California ARB Compliance Offset Program") ~ "California ARB Offset Program",
      offset_program_name == "EU Natura 2000 Network" ~ "EU Natura 2000",
      offset_program_name == "California Cap-and-Trade Program" ~ "California Cap-and-Trade",
      offset_program_name == "New Zealand Emissions Trading Scheme" ~ "NZ ETS",
      offset_program_name == "Regional Greenhouse Gas Initiative (RGGI)" ~ "RGGI",
      offset_program_name == "Australia Carbon Pricing Mechanism" ~ "Australia Carbon Pricing Mechanism",
      offset_program_name %in% c("Australian Emissions Reduction Fund", "Australia Emissions Reduction Fund", "Emissions Reduction Fund (ERF)") ~ "Australia ERF",
      offset_program_name == "US Species Conservation Banking Program" ~ "US Conservation Banking",
      offset_program_name == "Gold Standard" ~ "Gold Standard",
      offset_program_name %in% c("Climate Action Reserve", "Climate Action Reserve (CAR)") ~ "Climate Action Reserve (CAR)",
      offset_program_name %in% c("American Carbon Registry", "American Carbon Registry (ACR)") ~ "American Carbon Registry (ACR)",
      offset_program_name == "Canadian Fisheries Act Offsetting Policy" ~ "Canada Fisheries Act Offsetting",
      offset_program_name == "EU Water Framework Directive" ~ "EU Water Framework Directive",
      offset_program_name == "EU Habitats Directive" ~ "EU Habitats Directive",
      offset_program_name == "England Biodiversity Net Gain" ~ "England BNG",
      offset_program_name %in% c("NSW Biodiversity Offsets Policy for Major Projects", "NSW Biodiversity Offset Scheme") ~ "NSW Biodiversity Offsets",
      offset_program_name == "California Forest Carbon Offset Program" ~ "California Forest Carbon Offsets",
      offset_program_name == "Victoria Native Vegetation Framework" ~ "Victoria Native Vegetation Framework",
      offset_program_name == "Biodiversity Net Gain" ~ "BNG (Generic)",
      offset_program_name == "US Habitat Conservation Plan (HCP) Program" ~ "US HCP Program",
      offset_program_name == "National Habitat Compensation Programme" ~ "National Habitat Compensation Programme",
      offset_program_name == "Australian Carbon Credit Unit (ACCU) Scheme" ~ "Australia ACCU Scheme",
      offset_program_name == "China Certified Emission Reductions (CCER)" ~ "China CCER",
      offset_program_name == "Forest Carbon Partnership Facility (FCPF)" ~ "FCPF",
      offset_program_name == "Forest+ Program (Brazil)" ~ "Brazil Forest+ Program",
      offset_program_name == "MexiCO2 (Mexico)" ~ "MexiCO2",
      offset_program_name == "Colombia Voluntary Carbon Market Platform" ~ "Colombia Voluntary Market",
      offset_program_name == "National Payment Policy for Environmental Services (Brazil)" ~ "Brazil PES Policy",
      offset_program_name == "Chile ERPA" ~ "Chile ERPA",
      offset_program_name == "Costa Rica Forestry Environmental Services Program (FESP)" ~ "Costa Rica FESP",
      offset_program_name == "multiple" ~ "Multiple (Placeholder)",
      TRUE ~ NA_character_
    )
  )

# -----------------------------------------
# 5. Check for unmapped programs
# -----------------------------------------

unmapped_programs <- data_long %>%
  filter(is.na(offset_program_name_std) & !is.na(offset_program_name)) %>%
  distinct(offset_program_name) %>%
  arrange(offset_program_name)

if (nrow(unmapped_programs) > 0) {
  warning("Some offset_program_name values were not standardized:")
  print(unmapped_programs)
} else {
  message("All offset_program_name values successfully standardized.")
}

# -----------------------------------------
# 6. Create and save lookup table
# -----------------------------------------

# Create lookup table of original to standardized program names
program_lookup <- data_long %>%
  select(offset_program_name, offset_program_name_std) %>%
  filter(!is.na(offset_program_name_std)) %>%
  distinct() %>%
  arrange(offset_program_name_std)

# View the result
print(program_lookup, n = 50)

# Save as CSV for manual review or use elsewhere
# write_csv(program_lookup, here("data", "reference", "offset_program_lookup.csv"))















library(tibble)
library(dplyr)
library(here)
library(readr)

# Create the lookup table manually
offset_program_lookup <- tribble(
  ~original_name,                                                       ~standardized_name,               ~program_type,            ~scope,
  "US Clean Water Act Section 404 Wetland and Stream Permitting",      "US Clean Water Act Section 404", "Regulatory permitting",  "National (US)",
  "US Wetland and Stream Mitigation Banking",                          "US Mitigation Banking",          "Compliance program",     "National (US)",
  "Activities Implemented Jointly (AIJ) Pilot Phase",                  "AIJ Pilot Phase",                "UN Mechanism (historical)", "Global",
  "UNFCCC Clean Development Mechanism (CDM)",                          "UNFCCC CDM",                     "UN Mechanism",           "Global",
  "Canada Fisheries Act Section 35(2) Fish Habitat Authorization Program", "Canada Fisheries Act Offsetting", "Regulatory permitting", "National (Canada)",
  "NSW BioBanking Scheme",                                             "NSW BioBanking Scheme",          "Compliance program",     "Subnational (Australia)",
  "UNFCCC REDD+ Mechanism",                                            "UNFCCC REDD+",                   "UN Mechanism",           "Global",
  "Voluntary Carbon Standard (VCS)",                                   "Verra VCS",                      "Voluntary registry",     "Global",
  "Victoria's Native Vegetation Management Framework",                "Victoria Native Vegetation Framework", "Compliance program", "Subnational (Australia)",
  "German Compensation System (Eingriffsregelung)",                    "Germany Eingriffsregelung",      "Regulatory framework",   "National (Germany)",
  "REDD/REDD+",                                                        "REDD+",                          "Voluntary/UN mechanism", "Global",
  "Carbon Farming Initiative",                                         "Australia CFI",                  "Compliance program",     "National (Australia)",
  "California Air Resources Board (ARB) Compliance Offset Program",    "California ARB Offset Program", "Compliance offset program", "Subnational (US)",
  "EU Natura 2000 Network",                                            "EU Natura 2000",                 "Regulatory network",     "EU",
  "California Cap-and-Trade Program",                                  "California Cap-and-Trade",       "Compliance market",      "Subnational (US)",
  "New Zealand Emissions Trading Scheme",                              "NZ ETS",                         "Compliance market",      "National (NZ)",
  "Australia Carbon Farming Initiative",                               "Australia CFI",                  "Compliance program",     "National (Australia)",
  "Regional Greenhouse Gas Initiative (RGGI)",                         "RGGI",                           "Compliance market",      "Multistate (US)",
  "Australia Carbon Pricing Mechanism",                                "Australia Carbon Pricing Mechanism", "Compliance market", "National (Australia)",
  "Australian Emissions Reduction Fund",                               "Australia ERF",                  "Compliance offset program", "National (Australia)",
  "Carbon Farming Initiative (CFI)",                                   "Australia CFI",                  "Compliance program",     "National (Australia)",
  "US Species Conservation Banking Program",                           "US Conservation Banking",        "Compliance program",     "National (US)",
  "California ARB Compliance Offset Program",                          "California ARB Offset Program",  "Compliance offset program", "Subnational (US)",
  "Verified Carbon Standard",                                          "Verra VCS",                      "Voluntary registry",     "Global",
  "Climate Action Reserve",                                            "Climate Action Reserve (CAR)",   "Voluntary registry",     "US-based",
  "American Carbon Registry",                                          "American Carbon Registry (ACR)", "Voluntary registry",     "US-based",
  "Verra Verified Carbon Standard (VCS)",                              "Verra VCS",                      "Voluntary registry",     "Global",
  "Canadian Fisheries Act Offsetting Policy",                          "Canada Fisheries Act Offsetting","Regulatory permitting",  "National (Canada)",
  "EU Water Framework Directive",                                      "EU Water Framework Directive",   "Directive",              "EU",
  "EU Habitats Directive",                                             "EU Habitats Directive",          "Directive",              "EU",
  "England Biodiversity Net Gain",                                     "England BNG",                    "Compliance policy",      "National (UK)",
  "NSW Biodiversity Offsets Policy for Major Projects",                "NSW Biodiversity Offsets",       "Compliance policy",      "Subnational (Australia)",
  "Victoria Native Vegetation Framework",                              "Victoria Native Vegetation Framework", "Compliance program", "Subnational (Australia)",
  "California Forest Carbon Offset Program",                           "California Forest Carbon Offsets", "Compliance offset program", "Subnational (US)",
  "Biodiversity Net Gain",                                             "BNG (Generic)",                  "Conceptual approach",    "Generic",
  "US Habitat Conservation Plan (HCP) Program",                        "US HCP Program",                 "Compliance mechanism",   "National (US)",
  "National Habitat Compensation Programme",                           "National Habitat Compensation Programme", "Regulatory program", "National (Unspecified)",
  "multiple",                                                          "Multiple (Placeholder)",         "Unspecified",            "Unspecified",
  "NSW Biodiversity Offset Scheme",                                    "NSW Biodiversity Offsets",       "Compliance policy",      "Subnational (Australia)",
  "Australian Carbon Credit Unit (ACCU) Scheme",                       "Australia ACCU Scheme",          "Credit scheme",          "National (Australia)",
  "Emissions Reduction Fund (ERF)",                                    "Australia ERF",                  "Compliance offset program", "National (Australia)",
  "China Certified Emission Reductions (CCER)",                        "China CCER",                     "Compliance market",      "National (China)",
  "Australia Emissions Reduction Fund",                                "Australia ERF",                  "Compliance offset program", "National (Australia)",
  "Forest Carbon Partnership Facility (FCPF)",                         "FCPF",                           "Readiness mechanism",    "Global",
  "Gold Standard",                                                     "Gold Standard",                  "Voluntary registry",     "Global",
  "American Carbon Registry (ACR)",                                    "American Carbon Registry (ACR)", "Voluntary registry",     "US-based",
  "Climate Action Reserve (CAR)",                                      "Climate Action Reserve (CAR)",   "Voluntary registry",     "US-based",
  "Forest+ Program (Brazil)",                                          "Brazil Forest+ Program",         "Public incentive program", "National (Brazil)",
  "MexiCO2 (Mexico)",                                                  "MexiCO2",                        "Voluntary platform",     "National (Mexico)",
  "Colombia Voluntary Carbon Market Platform",                         "Colombia Voluntary Market",      "Voluntary platform",     "National (Colombia)",
  "National Payment Policy for Environmental Services (Brazil)",       "Brazil PES Policy",              "Public payment policy",  "National (Brazil)",
  "Chile ERPA",                                                        "Chile ERPA",                     "Bilateral agreement",     "National (Chile)",
  "Costa Rica Forestry Environmental Services Program (FESP)",         "Costa Rica FESP",                "Public incentive program", "National (Costa Rica)"
)

# View the table
print(offset_program_lookup)

# Optional: save as a CSV
# write_csv(offset_program_lookup, here("data", "reference", "offset_program_lookup.csv"))


# Add detail type based on known standardized names and market_type
offset_program_lookup <- offset_program_lookup %>%
  mutate(
    program_detail_type = case_when(
      # Voluntary registries
      standardized_name %in% c(
        "Verra VCS", "Gold Standard", "Climate Action Reserve (CAR)",
        "American Carbon Registry (ACR)", "MexiCO2", "Colombia Voluntary Market"
      ) ~ "voluntary registry",

      # Cap-and-trade or ETS offsets
      standardized_name %in% c(
        "California ARB Offset Program", "California Cap-and-Trade",
        "NZ ETS", "RGGI", "China CCER"
      ) ~ "cap-and-trade offset",

      # Regulatory permitting or compliance tied to law
      standardized_name %in% c(
        "US Clean Water Act Section 404", "Canada Fisheries Act Offsetting",
        "Germany Eingriffsregelung", "EU Water Framework Directive",
        "EU Habitats Directive", "England BNG", "Victoria Native Vegetation Framework"
      ) ~ "regulatory directive",

      # National credit schemes or carbon funds
      standardized_name %in% c(
        "Australia ERF", "Australia ACCU Scheme", "Australia CFI"
      ) ~ "national offset fund",

      # Habitat and biodiversity banking
      standardized_name %in% c(
        "US Mitigation Banking", "US Conservation Banking", "NSW BioBanking Scheme", "NSW Biodiversity Offsets"
      ) ~ "biodiversity banking",

      # REDD+ and related hybrid/global programs
      standardized_name %in% c("UNFCCC REDD+", "REDD+", "FCPF") ~ "forest/REDD+ mechanism",

      # Public payment or incentive programs
      standardized_name %in% c(
        "Costa Rica FESP", "Brazil PES Policy", "Brazil Forest+ Program"
      ) ~ "ecosystem service payments",

      # Pilot or legacy
      standardized_name == "AIJ Pilot Phase" ~ "pilot mechanism (legacy)",

      # Placeholder
      standardized_name == "Multiple (Placeholder)" ~ "unspecified/multiple",

      # Catch-all fallback
      TRUE ~ "unspecified"
    )
  )





















# ---------------------------
# 5. Check for incomplete entries
# ---------------------------

# Policies with no associated programs
orphan_policies <- policy_program_lookup %>%
  filter(is.na(program_name) | program_name == "")

# Print warning if such rows exist
if (nrow(orphan_policies) > 0) {
  warning("Some policy entries do not have associated programs.")
  print(orphan_policies)
} else {
  message("All policy entries have associated programs (or intentional blanks).")
}




# policy names table 
policy_names <- unique(data_long$policy_legal_instrument_name)

policy_table <- tibble(policy_raw = policy_names) %>%
  arrange(policy_raw)

clean_policies <- tibble(policy_names = policy_names) %>%
  mutate(
    # Extract year in parentheses (4-digit numbers)
    policy_year = str_extract(policy_names, "\\b(19|20)\\d{2}\\b"),
    
    # Remove everything in parentheses
    policy_name_clean = str_trim(str_remove(policy_names, "\\s*\\(.*?\\)")))


print(policy_names)















# ---------------------------
# 6. Save lookup table
# ---------------------------

# output_dir <- here("data", "reference")
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
# 
# output_path <- file.path(output_dir, "policy_program_lookup.csv")
# write_csv(policy_program_lookup, output_path)