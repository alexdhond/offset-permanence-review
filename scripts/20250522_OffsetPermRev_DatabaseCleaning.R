# ===============================================
# Script:     inspect_excel_columns.R
# Date:       2025-05-21
# Author:     Alex Dhond
# Purpose:    Load Excel data and inspect column structure
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