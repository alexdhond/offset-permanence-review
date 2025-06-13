# ================================================================
# Script:     00_database_information_extraction.R
# Date:       2025-05-30
# Author:     Alex Dhond
# Purpose:    Extract key info from database
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(readxl)    # Read Excel files
library(tidyverse) # Data manipulation (includes dplyr, tidyr)
library(here)      # Reproducible file paths
library(janitor)   # Clean column names
library(ggplot2)   # Plotting

# ---------------------------
# 2. Load raw data and aliases
# ---------------------------

data_raw <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

# ---------------------------
# 3. Get some summary stats about interesting questions
# ---------------------------

# How many total papers?
num_papers <- n_distinct(data_raw$study_title)
print(paste("There are", num_papers, "papers in the dataset."))

# What is the distribution of publication years?
print(paste("Earliest publication year:", min(data_raw$publication_year, na.rm = TRUE)))
print(paste("Latest publication year:", max(data_raw$publication_year, na.rm = TRUE)))

# Top 5 years with the most papers
top_years <- data_raw %>%
  count(publication_year, sort = TRUE) %>%
  slice_max(n, n = 5)

print("Top 5 publication years by number of papers:")
print(top_years)

# Histogram of publication years
ggplot(data_raw, aes(x = publication_year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Publication Years",
    x = "Publication Year",
    y = "Number of Papers"
  ) +
  theme_minimal()


# General offset category - how many of each category (e.g., biodiversity, carbon)?
category_counts <- data_raw %>%
  count(offset_category_general, sort = TRUE)
print("Offset category (general) counts:")
print(category_counts)

# Resource type - how many studies came from each?
resource_counts <- data_raw %>%
  count(evidence_type, sort = TRUE)
print("Resource type counts:")
print(resource_counts)

# Bar plot of resource types
ggplot(data_raw, aes(x = fct_infreq(evidence_type))) +
  geom_bar(fill = "darkgreen", color = "black") +
  labs(
    title = "Number of Studies by Resource Type",
    x = "Resource Type",
    y = "Count"
  ) +
  coord_flip() +  # Flips axes for better readability if many types
  theme_minimal()


ggplot(data_raw %>% filter(!is.na(evidence_type), !is.na(offset_category_general)),
       aes(x = fct_infreq(evidence_type), fill = offset_category_general)) +
  geom_bar(position = "dodge", color = "black") +
  labs(
    title = "Studies by Resource Type and Offset Category (Side-by-Side)",
    x = "Resource Type",
    y = "Count",
    fill = "Offset Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
