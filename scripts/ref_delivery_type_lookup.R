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
# 3. Explode delivery type 
# ---------------------------

# Explode delivery types
delivery_type_df <- data %>%
  select(offset_delivery_type) %>%
  separate_rows(offset_delivery_type, sep = ";\\s*") %>%
  mutate(offset_delivery_type = str_trim(offset_delivery_type)) %>%
  filter(offset_delivery_type != "", !is.na(offset_delivery_type)) %>%
  distinct()

# ---------------------------
# 4. Create lookup tables with placeholder standardization
# ---------------------------

# Type lookup
delivery_type_lookup <- delivery_type_df %>%
  rename(raw_delivery_type = offset_delivery_type) %>%
  mutate(
    standardized_delivery_type = NA_character_,
    delivery_category = NA_character_,
    mapping_status = "unmapped"
  )

tribble(
  ~raw_delivery_type,           ~standardized_delivery_type, ~mapping_status,
  "permittee-responsible",      "Permittee-Responsible",      "mapped",
  "mitigation banking",         "Mitigation Bank",            "mapped",
  "habitat banking",            "Mitigation Bank",            "mapped",
  "species conservation banking", "Mitigation Bank",          "mapped",
  "in-lieu fee",                "In-Lieu Fee",                "mapped",
  "NGO-led",                    "NGO-Delivered",              "mapped",
  "project developer-led",      "Developer-Delivered",        "mapped",
  "government-led",             "Government-Delivered",       "mapped",
  "government-implemented",     "Government-Delivered",       "mapped",
  "government implemented",     "Government-Delivered",       "mapped"
)



# ---------------------------
# 5. Save outputs
# ---------------------------

output_dir <- here("data", "reference")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(delivery_type_lookup, file.path(output_dir, "offset_delivery_type_lookup.csv"))
write_csv(delivery_reasoning_lookup, file.path(output_dir, "offset_delivery_type_reasoning_lookup.csv"))

message("✅ Lookup tables saved: delivery type and reasoning")
