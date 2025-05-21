# 2025-01-12 Data Processing and Metagear Setup
# Alex Dhond

# This script is used to process data from Scopus and Web of Science exports.
# It sets up the data for screening using the Metagear package.
# The data is filtered and duplicates are removed
# Metagear package is loaded and distributes screening effort to reviewer

# Load necessary libraries
library(tidyverse) # data manipulation
library(metagear) # for meta analysis/literature review
library(readxl) # for reading excel files

# Load data
scopus_raw <- read_csv("12-01-2025 scopus export.csv")
wos_raw <- read_excel("WOS 12-01-2025.xlsx")

# Inspect data
glimpse(scopus_raw)

# Check for duplicates and remove as necessary
# Filter data - select title, abstract, year, DOI.
scopus_filter <- scopus_raw %>%
  select(Title, Year, Abstract, DOI)
wos_filter <- wos_raw %>%
  select(`Article Title`, `Publication Year`, Abstract, DOI)

# change column names
colnames(scopus_filter) <- c("TITLE", "YEAR", "ABSTRACT", "DOI")
colnames(wos_filter) <- c("TITLE", "YEAR", "ABSTRACT", "DOI")

# Bind the data together
filtered_data <- rbind(scopus_filter, wos_filter)

# Remove duplicates using the distinct function
distinct_filtered_data <- distinct(filtered_data, .keep_all = TRUE)

# Remove duplicates using manual method based on DOI
distinct_filtered_data <- distinct_filtered_data[!duplicated(distinct_filtered_data[, c("DOI")]), ]

# Manually view and check for duplicates (filter by alphabetical order)
View(distinct_filtered_data)

# Set up metagear package

# Metagear loading
the_refs <- effort_initialize(distinct_filtered_data)
names(the_refs)

# Distribute screening effort to myself
the_team <- c("AKD")
the_refs_unscreened <- effort_distribute(the_refs, reviewers = the_team, save_split = TRUE)

# Take a look
glimpse(the_refs_unscreened)