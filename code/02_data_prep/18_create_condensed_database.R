# =============================================================================
# 18_create_condensed_database.R
# =============================================================================
# Purpose: Create a condensed (wide-format) database with one row per study
#
# Input:  data/final/offset_perm_rev_long_cleaned.csv (682K rows, one per combination)
# Output: data/final/offset_perm_rev_condensed.csv (137 rows, one per study)
#         data/final/offset_perm_rev_condensed.rds
#
# Note: reviewer_notes column is excluded from the output for publication.
#
# This produces a user-friendly database where multi-value fields (countries,
# species, risks, etc.) are collapsed into semicolon-separated strings.
# =============================================================================

library(here)
library(dplyr)
library(tidyr)
library(readr)

# Load the long-format cleaned data
message("Loading long-format data...")
long_df <- read_csv(

  here("data", "final", "offset_perm_rev_long_cleaned.csv"),
  col_types = cols(.default = col_character())
)

message("  Loaded ", nrow(long_df), " rows from ", n_distinct(long_df$study_title), " studies")

# Helper function to collapse unique non-NA values with semicolon separator
collapse_unique <- function(x) {

  unique_vals <- unique(x[!is.na(x) & x != "" & x != "NA"])
  if (length(unique_vals) == 0) return(NA_character_)
  paste(unique_vals, collapse = "; ")
}

# Columns to collapse (multi-value fields)
collapse_cols <- c(
  "continent",
  "country",
  "subnational_region",
  "subnational_region_type",
  "species_common_name",
  "species_scientific_name",
  "species_taxonomic_group",
  "ecosystem_type",
  "ecosystem_broad_type",
  "project_type",
  "project_broad_type",
  "program_name",
  "program_type",
  "program_mechanism_type",
  "program_status",
  "program_scope_level",
  "program_scope_location",
  "program_related",
  "project_delivery_type",
  "policy_name",
  "policy_type",
  "policy_note",
  "policy_jurisdiction_level",
  "policy_jurisdiction_note",
  "policy_jurisdiction_location",
  "policy_status",
  "policy_year_adopted",
  "policy_description",
  "permanence_risk_domain",
  "permanence_risk_category",
  "permanence_risk_type",
  "permanence_risk_evidence_and_reasoning"
)

# Columns that should be the same for all rows of a study (take first value)
single_cols <- c(
  "study_title",
  "study_publication_year",
  "study_evidence_type",
  "offset_category_general"
)

message("Condensing to one row per study...")

# Create condensed dataframe
condensed_df <- long_df %>%

  group_by(study_title) %>%
  summarise(
    # Single-value columns (take first non-NA value)
    study_publication_year = first(na.omit(study_publication_year)),
    study_evidence_type = first(na.omit(study_evidence_type)),
    offset_category_general = first(na.omit(offset_category_general)),

    # Multi-value columns (collapse unique values)
    across(
      all_of(intersect(collapse_cols, names(long_df))),
      collapse_unique
    ),

    .groups = "drop"
  ) %>%
  # Add study_id

  mutate(study_id = row_number()) %>%
  # Reorder columns to match expected structure

  select(
    study_id,
    study_title,
    study_publication_year,
    everything()
  )

message("  Created condensed database with ", nrow(condensed_df), " studies and ", ncol(condensed_df), " columns")

# Save as CSV
csv_path <- here("data", "final", "offset_perm_rev_condensed.csv")
write_csv(condensed_df, csv_path)
message("  Saved CSV: ", csv_path)

# Save as RDS (for faster loading in Shiny)
rds_path <- here("data", "final", "offset_perm_rev_condensed.rds")
saveRDS(condensed_df, rds_path)
message("  Saved RDS: ", rds_path)

message("Done!")
