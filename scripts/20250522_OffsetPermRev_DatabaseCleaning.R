# ===============================================
# Script:     inspect_excel_columns.R
# Date:       2025-05-21
# Author:     Alex Dhond
# Purpose:    Load Excel data and inspect column structure, and create lookup table
# ===============================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # read Excel files
library(janitor)   # clean column names
library(tidyverse) # data wrangling and manipulation
library(here)      # file paths

# ---------------------------
# 2. Read Excel file
# ---------------------------

# Load excel file from correct folder path
excel_file <- here("data", "offset_perm_rev_database.xlsx")

# load data into R
data <- read_excel(excel_file) %>%
  clean_names()  # standardize column names

# ---------------------------
# 3. Basic column analysis
# ---------------------------

# View column names and types
glimpse(data.class(data))

# Summary of missing values per column
missing_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
  arrange(desc(missing_count))

print(missing_summary)

# Unique value count per column
unique_summary <- data %>%
  summarise(across(everything(), ~ n_distinct(.))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "unique_values") %>%
  arrange(unique_values)

print(unique_summary)

# ---------------------------
# 1. Publication year summary
# ---------------------------

data %>%
  count(publication_year, sort = TRUE) %>%
  ggplot(aes(x = publication_year, y = n)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Number of Studies by Publication Year", x = "Year", y = "Study Count")

# ---------------------------
# 2. Offset category overview
# ---------------------------

data %>%
  count(offset_category_general, sort = TRUE) %>%
  ggplot(aes(x = reorder(offset_category_general, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Studies by Offset Category", x = "Category", y = "Count")

# ---------------------------
# 3. Number of unique countries
# ---------------------------

n_distinct(data$country)

# ---------------------------
# 4. Top countries by study count
# ---------------------------

data %>%
  count(country, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Countries by Study Count", x = "Country", y = "Count")

# ---------------------------
# 5. Carbon vs Biodiversity proportion
# ---------------------------

data %>%
  count(offset_category_general) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = "", y = prop, fill = offset_category_general)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Proportion of Carbon vs Biodiversity Studies")


data %>%
  count(permanence_risk_broad) %>%
  ggplot(aes(x = permanence_risk_broad, y = n)) +
  geom_col(fill = "firebrick") +
  theme_minimal() +
  labs(title = "Permanence Risks (Broad)", x = NULL, y = "Study Count")


# ---------------------------
# 4. Extract and prepare project type lookup
# ---------------------------

# Separate semicolon-delimited terms and clean whitespace
all_terms <- data %>%
  select(project_type_specific) %>%
  filter(!is.na(project_type_specific)) %>%
  separate_rows(project_type_specific, sep = ";") %>%
  mutate(project_type_specific = str_trim(project_type_specific)) %>%
  distinct(project_type_specific)

# Create blank lookup table for manual tagging
lookup_tbl <- all_terms %>%
  mutate(ecological_action_category = NA_character_)

lookup_tbl <- tribble(
  ~project_type_specific,                ~ecological_action_category,
  # --- Restoration ---
  "wetland restoration",                 "Restoration",
  "stream restoration",                  "Restoration",
  "forest restoration",                  "Restoration",
  "lake restoration",                    "Restoration",
  "river restoration",                   "Restoration",
  "seagrass restoration",                "Restoration",
  "grassland restoration",               "Restoration",
  "habitat restoration",                 "Restoration",
  "vegetation regeneration",             "Restoration",
  "forest regeneration",                 "Restoration",
  "habitat rehabilitation",              "Restoration",
  "peatland rewetting",                  "Restoration",
  "afforestation",                       "Restoration",
  "reforestation",                       "Restoration",
  "human-induced regeneration",          "Restoration",
  "enrichment planting",                 "Restoration",
  "restoration",                         "Restoration",
  
  # --- Creation ---
  "wetland creation",                    "Creation",
  "vernal pool creation",                "Creation",
  "pond creation",                       "Creation",
  "habitat creation",                    "Creation",
  "meadow creation",                     "Creation",
  "temporary pool creation",             "Creation",
  "linear plantation creation",          "Creation",
  "micro-habitat creation",              "Creation",
  "plantation establishment",            "Creation",
  "tree planting",                       "Creation",
  "planting",                            "Creation",
  "environmental plantings",             "Creation",
  "nest box installation",               "Creation",
  "species introduction",                "Creation",
  
  # --- Protection ---
  "wetland preservation",                "Protection",
  "stream preservation",                 "Protection",
  "habitat protection",                  "Protection",
  "wetland protection",                  "Protection",
  "forest protection",                   "Protection",
  "forest conservation",                 "Protection",
  "mangrove conservation",               "Protection",
  "avoided deforestation",               "Protection",
  "avoided deforestation/forest conservation", "Protection",
  "avoided conversion",                  "Protection",
  "avoided loss",                        "Protection",
  "land acquisition",                    "Protection",
  "REDD/REDD+ pilot",                    "Protection",
  "species conservation banking",        "Protection",
  "forest carbon offsets",               "Protection",
  "biodiversity offsets",                "Protection",
  "habitat banking",                     "Protection",
  "averted loss",                        "Protection",
  
  # --- Enhancement ---
  "wetland enhancement",                 "Enhancement",
  "stream enhancement",                  "Enhancement",
  "habitat enhancement",                 "Enhancement",
  "vegetation enhancement",              "Enhancement",
  "species conservation offset",         "Enhancement",
  
  # --- Management ---
  "habitat maintenance",                 "Management",
  "habitat management",                  "Management",
  "pest control",                        "Management",
  "predator control",                    "Management",
  "weed control",                        "Management",
  "fire management",                     "Management",
  "controlled fire management",          "Management",
  "prescribed burning",                 "Management",
  "thinning",                            "Management",
  "fuel reduction",                      "Management",
  "grazing management",                  "Management",
  "active management",                   "Management",
  "threat management",                   "Management",
  "sustainable forest management",       "Management",
  "improved forest management",          "Management",
  "community-based forest management",   "Management",
  "agroforestation",                     "Management",
  "agroforestry",                        "Management",
  
  # --- Modifications ---
  "wetland replication",                 "Modifications",
  "sediment modification",               "Modifications",
  "sediment removal",                    "Modifications",
  "sediment fill",                       "Modifications",
  "sediment tubes",                      "Modifications",
  "vessel damage repair",                "Modifications",
  "vessel removal",                      "Modifications",
  "breakwater",                          "Modifications",
  "land conversion",                     "Modifications",
  "natural channel design",              "Modifications"
)

# Optional: Save for manual editing
# write_csv(lookup_tbl, here("data", "lookup_table_to_tag.csv"))


# Step 1: Explode semicolon-delimited values
classified_long <- data %>%
  select(study_id, project_type_specific) %>%
  filter(!is.na(project_type_specific)) %>%
  separate_rows(project_type_specific, sep = ";") %>%
  mutate(project_type_specific = str_trim(project_type_specific)) %>%
  left_join(lookup_tbl, by = "project_type_specific") %>%
  mutate(ecological_action_category = coalesce(ecological_action_category, "Unclear/Other"))

# Step 2: Recombine classified results per study_id
classified_summary <- classified_long %>%
  group_by(study_id) %>%
  summarise(
    project_type_specific = paste(unique(project_type_specific), collapse = "; "),
    ecological_action_category = paste(unique(ecological_action_category), collapse = "; "),
    .groups = "drop"
  )

# Step 3: Merge classification back into full dataset
data_classified <- data %>%
  select(-project_type_specific) %>%
  left_join(classified_summary, by = "study_id")
