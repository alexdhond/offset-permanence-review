# ===============================================
# Script:     ref_offset_policy_lookup.R
# Date:       2025-06-03
# Author:     Alex Dhond
# Purpose:    Standardize and create a lookup table for policy names
# ===============================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # For reading Excel files
library(tidyverse) # For data wrangling
library(here)      # For relative file paths
library(janitor)   # For cleaning column names

# ---------------------------
# 2. Load and preprocess data
# ---------------------------

# load in data
data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

# ---------------------------
# 3. Split semicolon-delimited policy fields
# ---------------------------

# Explode all relevant policy fields
policy_exploded <- data %>%
  select(policy_legal_instrument_name, year_of_policy_adoption, policy_jurisdiction) %>%
  mutate(across(everything(), ~ str_split(.x, ";\\s*"))) %>%
  unnest_longer(policy_legal_instrument_name) %>%
  unnest_longer(year_of_policy_adoption) %>%
  unnest_longer(policy_jurisdiction) %>%
  filter(!is.na(policy_legal_instrument_name))

# Check policies
unique(policy_exploded$policy_legal_instrument_name)


# ---------------------------
# 4. Clean and standardize policy names
# ---------------------------

# Create lookup table
policy_lookup <- policy_exploded %>%
  filter(!is.na(policy_legal_instrument_name)) %>%
  mutate(
    policy_year = str_extract(policy_legal_instrument_name, "\\(?(19|20)\\d{2}\\)?"),     # Extract 4-digit year starting with 19 or 20
    policy_year = str_remove_all(policy_year, "[()]"),                                    # Remove parentheses
    policy_name_clean = str_remove(policy_legal_instrument_name, "\\s*\\(?(19|20)\\d{2}\\)?$"), # Remove trailing year (w/ or w/o parens)
    policy_name_std = case_when(
      
      # US Policies
      policy_legal_instrument_name %in% c("US Clean Water Act Section 404 (1972)", "US Clean Water Act section 404 (1972)") ~ "US Clean Water Act Section 404",
      policy_legal_instrument_name == "US Endangered Species Act (1973)" ~ "US Endangered Species Act",
      policy_legal_instrument_name == "California AB 32 Global Warming Solutions Act (2006)" ~ "California Global Warming Solutions Act",
      policy_legal_instrument_name == "Massachusetts Wetlands Protection Act (1963)" ~ "Massachusetts Wetland Protection Act",
      
      
      # Canada
      policy_legal_instrument_name %in% c("Canada Fisheries Act (1985)", "Canadian Fisheries Act (1985)", 
                                          "Canada Fisheries Act section 35(2) (1976)", "Canada Fisheries Act section 35(2) (1985)") ~ "Canada Fisheries Act",
      policy_legal_instrument_name == "Federal Policy on Wetland Conservation (1991)" ~ "Canada Wetland Policy",
      policy_legal_instrument_name == "Quebec Environment Quality Act Section 22 (2006)" ~ "Quebec Environment Quality Act",
      policy_legal_instrument_name == "Wetland Conservation Policy for Prince Edward Island (2003)" ~ "PEI Wetland Policy",
      policy_legal_instrument_name == "Newfoundland Policy Directive for Development in Wetlands (1997)" ~ "Newfoundland Wetland Directive",
      policy_legal_instrument_name == "Nova Scotia Wetlands Designation Policy (2006)" ~ "Nova Scotia Wetlands Policy",
      policy_legal_instrument_name == "New Brunswick Wetlands Conservation Policy (2002)" ~ "New Brunswick Wetlands Policy",
      
      # Australia
      policy_legal_instrument_name == "Environment Protection and Biodiversity Conservation Act (1999)" ~ "Australia EPBC Act",
      policy_legal_instrument_name == "Australia Carbon Pricing Mechanism (2011)" ~ "Australia Carbon Pricing Mechanism",
      policy_legal_instrument_name %in% c("Australia Carbon Credits (Carbon Farming Initiative) Act (2011)", "Carbon Credits (Carbon Farming Initiative) Act (2011)") ~ "Australia CFI Act",
      policy_legal_instrument_name == "NSW Biodiversity Conservation Act (2016)" ~ "NSW Biodiversity Conservation Act",
      policy_legal_instrument_name == "NSW Native Vegetation Act (2005)" ~ "NSW Native Vegetation Act",
      policy_legal_instrument_name == "NSW Threatened Species Conservation Act (1995)" ~ "NSW Threatened Species Conservation Act",
      policy_legal_instrument_name == "NSW Threatened Species Conservation Amendment (Biodiversity Banking) Act (2006)" ~ "NSW Biodiversity Banking Amendment Act",
      policy_legal_instrument_name == "Victoria Planning and Environment Act (1987)" ~ "Victoria Planning and Environment Act",
      policy_legal_instrument_name %in% c("Victoria Native Vegetation Management Framework (2002)", "Victoria's Native Vegetation Management Framework") ~ "Victoria Native Vegetation Framework",
      policy_legal_instrument_name == "WA Environmental Offsets Policy (2011)" ~ "WA Environmental Offsets Policy",
      policy_legal_instrument_name == "Western Australia Environmental Protection Act (1986)" ~ "WA Environmental Protection Act",
      
      # New Zealand
      policy_legal_instrument_name == "New Zealand Resource Management Act (1991)" ~ "NZ Resource Management Act",
      policy_legal_instrument_name == "New Zealand Climate Change Response Act (2002)" ~ "NZ Climate Change Response Act",
      policy_legal_instrument_name == "NZ Conservation Act (1987)" ~ "NZ Conservation Act",
      
      # UK + EU
      policy_legal_instrument_name %in% c("Conservation of Habitats and Species Regulations (2010)", "Conservation of Habitats and Species Regulations (2017)") ~ "UK Habitats and Species Regulations",
      policy_legal_instrument_name == "Environment Act (2021)" ~ "UK Environment Act",
      policy_legal_instrument_name == "UK Wildlife and Countryside Act (1981)" ~ "UK Wildlife and Countryside Act",
      policy_legal_instrument_name == "UK National Planning Policy Framework (2012)" ~ "UK Planning Policy Framework",
      policy_legal_instrument_name %in% c("EU Habitats Directive (1992)", "EU Habitats Directive (92/43/EEC)") ~ "EU Habitats Directive",
      policy_legal_instrument_name == "EU Birds Directive (2009)" ~ "EU Birds Directive",
      policy_legal_instrument_name == "EU Water Framework Directive (2000)" ~ "EU Water Framework Directive",
      
      # Other International
      policy_legal_instrument_name == "Kyoto Protocol (1997)" ~ "Kyoto Protocol",
      policy_legal_instrument_name == "UNFCCC Framework Convention (1992)" ~ "UNFCCC Convention",
      policy_legal_instrument_name == "UNFCCC REDD+ mechanism" ~ "UNFCCC REDD+",
      policy_legal_instrument_name == "World Bank safeguard policy OP/BP 4.36 (Forests) (2002)" ~ "World Bank Forests Safeguard",
      policy_legal_instrument_name == "RGGI Model Rule (2005)" ~ "RGGI Model Rule",
      policy_legal_instrument_name == "Interim Measures for CCERs (2012)" ~ "China CCER Interim Measures",
      
      # Voluntary registries
      policy_legal_instrument_name %in% c("Verified Carbon Standard (VCS)", "Verra (VCS Methodology V0007 V1.6)") ~ "Verra VCS",
      policy_legal_instrument_name == "American Carbon Registry (ACR)" ~ "American Carbon Registry (ACR)",
      policy_legal_instrument_name == "Climate Action Reserve (CAR)" ~ "Climate Action Reserve (CAR)",
      
      # Fallback
      TRUE ~ NA_character_
    )
  )

# ---------------------------
# 5. Check for duplicates or missing
# ---------------------------

missing_policy_names <- policy_lookup %>%
  filter(is.na(policy_name_std)) %>%
  distinct(policy_legal_instrument_name)

if (nrow(missing_policy_names) > 0) {
  warning("Some policy names could not be standardized:")
  print(missing_policy_names)
} else {
  message("All policy names were successfully standardized.")
}

# ---------------------------
# 6. Export lookup table
# ---------------------------

unique(policy_lookup$policy_name_std)

# write_csv(policy_lookup, here("data", "reference", "policy_lookup.csv"))

# Preview
# print(policy_lookup, n = 50)
