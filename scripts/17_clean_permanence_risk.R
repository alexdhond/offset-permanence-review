# ================================================================
# Script:     17_clean_permanence_risk.R
# Date:       2025-06-11
# Author:     Alex Dhond
# Purpose:    Standardize permanence risk subcategories using typology
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(tidyverse)
library(readxl)
library(here)
library(janitor)

# ---------------------------
# 2. Load raw data
# ---------------------------

raw_data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(row_id = row_number())

# ---------------------------
# 3. Load permanence risk typology
# ---------------------------

risk_typology <- read_csv(here("data", "reference", "permanence_risk_typology_lookup.csv")) %>%
  mutate(sub_risk = str_trim(sub_risk))

# ---------------------------
# 4. Explode by sub_risk only
# ---------------------------

# Split semicolon-separated `permanence_risk_subcategory` entries into rows
exploded <- raw_data %>%
  select(study_id, row_id, study_title, permanence_risk_subcategory) %>%
  mutate(sub_risk = str_split(permanence_risk_subcategory, ";\\s*")) %>%
  unnest(sub_risk) %>%
  mutate(sub_risk = str_trim(sub_risk)) %>%
  filter(sub_risk != "", !is.na(sub_risk)) %>%
  select(-permanence_risk_subcategory)  # DROP here

# ---------------------------
# 5. Join with typology to assign broader categories
# ---------------------------

permanence_cleaned <- exploded %>%
  left_join(risk_typology, by = "sub_risk") %>%
  mutate(valid_flag = !is.na(specific))  # Optional diagnostic flag

# ---------------------------
# 6. Diagnostics: Unmatched entries (optional)
# ---------------------------

unmatched_risks <- permanence_cleaned %>%
  filter(!valid_flag) %>%
  distinct(sub_risk) %>%
  arrange(sub_risk)

if (nrow(unmatched_risks) > 0) {
  warning("⚠️ Some sub_risk values were not matched in the typology.")
  print(unmatched_risks)
} else {
  message("✅ All permanence sub_risks successfully matched to typology.")
}

# ---------------------------
# 7. Export cleaned dataset
# ---------------------------

permanence_export <- permanence_cleaned %>%
  select(
    study_id,
    study_title,
    row_id,
    broad,
    specific,
    sub_risk
  )

output_dir <- here("data", "intermediate")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(permanence_export, file.path(output_dir, "permanence_risk_cleaned_long.csv"))
message("✅ Permanence risk cleaning complete. Output saved to 'data/intermediate'.")
