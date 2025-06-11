# ================================================================
# Script:     ref_delivery_type_lookup.R
# Date:       2025-06-09
# Author:     Alex Dhond
# Purpose:    Extract and standardize delivery type and reasoning
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)
library(tidyverse)
library(janitor)
library(here)

# ---------------------------
# 2. Load and prepare data
# ---------------------------

data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

# ---------------------------
# 3. Explode delivery types
# ---------------------------

delivery_type_df <- data %>%
  select(offset_delivery_type) %>%
  separate_rows(offset_delivery_type, sep = ";\\s*") %>%
  mutate(offset_delivery_type = str_trim(offset_delivery_type)) %>%
  filter(offset_delivery_type != "", !is.na(offset_delivery_type)) %>%
  distinct()

# ---------------------------
# 4. Define standardization lookup
# ---------------------------

delivery_type_lookup <- tribble(
  ~raw_delivery_type,               ~standardized_delivery_type,
  "permittee-responsible",          "Permittee-Responsible",    
  "mitigation banking",             "Mitigation Banking",        
  "habitat banking",                "Mitigation Banking",         
  "species conservation banking",   "Mitigation Banking",          
  "in-lieu fee",                    "In-Lieu Fee",                
  "NGO-led",                        "NGO-Delivered",             
  "project developer-led",          "Developer-Delivered",       
  "government-led",                 "Government-Delivered",    
  "government-implemented",         "Government-Delivered",      
  "government implemented",         "Government-Delivered",   
)

# ---------------------------
# 5. Join and flag unmapped entries
# ---------------------------

delivery_type_mapped <- delivery_type_df %>%
  rename(raw_delivery_type = offset_delivery_type) %>%
  left_join(delivery_type_lookup, by = "raw_delivery_type") %>%
  mutate(mapping_status = if_else(is.na(standardized_delivery_type), "unmapped", "mapped"))

# ---------------------------
# 6. Review unmapped entries (optional)
# ---------------------------

unmapped_delivery_types <- delivery_type_mapped %>%
  filter(mapping_status == "unmapped") %>%
  distinct(raw_delivery_type) %>%
  arrange(raw_delivery_type)

print(unmapped_delivery_types)

# ---------------------------
# 7. Save outputs
# ---------------------------

output_dir <- here("data", "reference")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(delivery_type_mapped, file.path(output_dir, "offset_delivery_type_lookup.csv"))
