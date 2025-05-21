# ============================================
# Script:     20250112_OffsetPermRev_DataProcessMetagearSetup.r
# Date:       2025-01-12
# Author:     Alex Dhond
# Purpose:    Process Scopus & Web of Science exports for screening
#             Set up screening effort using metagear
# ============================================

# ---------------------------
# 1. Load packages
# ---------------------------
library(tidyverse)   # Data manipulation
library(metagear)    # Screening tool for systematic reviews
library(readxl)      # Read Excel files
library(here)        # project-based file paths

# ---------------------------
# 2. Read input files
# ---------------------------
scopus_raw <- read_csv(here("data", "20250112_Scopus_export.csv"))
wos_raw <- read_excel(here("data", "20250112_WOS_export.xlsx"))

# ---------------------------
# 3. Filter and harmonize columns
# ---------------------------

# Filter Scopus
scopus_filtered <- scopus_raw %>%
  select(Title, Year, Abstract, DOI) %>%
  rename(TITLE = Title, YEAR = Year, ABSTRACT = Abstract)

# Filter Web of Science
wos_filtered <- wos_raw %>%
  select(`Article Title`, `Publication Year`, Abstract, DOI) %>%
  rename(TITLE = `Article Title`, YEAR = `Publication Year`, ABSTRACT = Abstract)

# ---------------------------
# 4. Combine and deduplicate
# ---------------------------

combined_refs <- bind_rows(scopus_filtered, wos_filtered) %>%
  distinct(DOI, .keep_all = TRUE)  # Remove duplicate DOIs

# ---------------------------
# 5. Initialize metagear screening
# ---------------------------
refs_for_screening <- effort_initialize(combined_refs)

# Assign screening effort to reviewer, but don't write file yet
reviewer_names <- c("AKD")
refs_distributed <- effort_distribute(refs_for_screening, reviewers = reviewer_names, save_split = FALSE)

# ---------------------------
# 6. Export
# ---------------------------

# Save to output folder manually
write_csv(refs_distributed, here("output", "effort_AKD.csv"))

# ---------------------------
# 7. Inspect result
# ---------------------------
glimpse(refs_distributed)