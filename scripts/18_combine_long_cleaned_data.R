# ================================================================
# Script:     18_combine_long_cleaned_data.R
# Date:       2025-06-11
# Author:     Alex Dhond
# Purpose:    Join cleaned long-format datasets and run diagnostics
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(tidyverse)
library(here)
library(readxl)
library(janitor)

# ---------------------------
# 2. Load raw data for diagnostics
# ---------------------------

raw_df <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(across(c(study_id, study_title), as.character),
         row_id = row_number())

# ---------------------------
# 3. Load cleaned datasets into named list
# ---------------------------

cleaned_data_list <- list(
  country     = list(file = "country_clean_valid.csv",                  raw_col = "country"),
  species     = list(file = "focal_species_cleaned_long.csv",           raw_col = "focal_species"),
  ecosystem   = list(file = "ecosystem_cleaned_long.csv",               raw_col = "ecosystem_type_specific"),
  project     = list(file = "project_type_cleaned_long.csv",            raw_col = "project_type_specific"),
  delivery    = list(file = "offset_delivery_type_cleaned_long.csv",    raw_col = "offset_delivery_type"),
  program     = list(file = "program_type_cleaned_long.csv",            raw_col = "offset_program_name"),
  policy      = list(file = "policy_type_cleaned_long.csv",             raw_col = "policy_legal_instrument_name"),
  permanence  = list(file = "permanence_risk_cleaned_long.csv",         raw_col = "permanence_risk_subcategory")
)

# Load and clean each dataset
for (name in names(cleaned_data_list)) {
  cleaned_data_list[[name]]$df <- read_csv(here("data", "intermediate", cleaned_data_list[[name]]$file)) %>%
    mutate(across(c(study_id, study_title), as.character))
}

# ---------------------------
# 4. Combine datasets by study_title
# ---------------------------

joined_df <- cleaned_data_list$country$df

for (name in setdiff(names(cleaned_data_list), "country")) {
  joined_df <- full_join(joined_df, cleaned_data_list[[name]]$df, by = "study_title")
}

# ---------------------------
# 5. Diagnostic helpers
# ---------------------------

diag_summary <- function(df, name) {
  tibble(
    dataset = name,
    rows = nrow(df),
    unique_studies = n_distinct(df$study_title),
    unique_row_ids = n_distinct(df$row_id)
  )
}

run_missing_diagnostics <- function(raw_df, cleaned_df, raw_col, cleaned_name = "cleaned", id_col = "study_title") {
  raw_has_data <- raw_df %>%
    select(!!sym(id_col), !!sym(raw_col)) %>%
    filter(!is.na(!!sym(raw_col)), !!sym(raw_col) != "") %>%
    distinct() %>%
    mutate(in_raw = TRUE)
  
  cleaned_has_data <- cleaned_df %>%
    select(!!sym(id_col)) %>%
    distinct() %>%
    mutate(in_cleaned = TRUE)
  
  diagnostic <- full_join(raw_has_data, cleaned_has_data, by = id_col) %>%
    mutate(in_raw = replace_na(in_raw, FALSE),
           in_cleaned = replace_na(in_cleaned, FALSE),
           status = case_when(
             in_raw & in_cleaned  ~ "✅ Present in both",
             in_raw & !in_cleaned ~ "⚠️ Missing in cleaned (check cleaning/matching)",
             !in_raw & in_cleaned ~ "⚠️ Unexpected: cleaned has data, raw did not",
             TRUE                 ~ "✅ Missing in both (expected)"
           ))
  
  message("\n📊 Diagnostic Summary for: ", cleaned_name)
  print(diagnostic %>% count(status, name = "count") %>% arrange(desc(count)))
  
  invisible(diagnostic)
}

# ---------------------------
# 6. Run diagnostics
# ---------------------------

# Coverage summary
n_unique_studies <- n_distinct(raw_df$study_title)
coverage_diagnostics <- bind_rows(
  diag_summary(raw_df,        "raw"),
  diag_summary(joined_df,     "joined"),
  diag_summary(cleaned_data_list$country$df,    "country"),
  diag_summary(cleaned_data_list$species$df,    "species"),
  diag_summary(cleaned_data_list$ecosystem$df,  "ecosystem"),
  diag_summary(cleaned_data_list$project$df,    "project"),
  diag_summary(cleaned_data_list$delivery$df,   "delivery"),
  diag_summary(cleaned_data_list$program$df,    "program"),
  diag_summary(cleaned_data_list$policy$df,     "policy"),
  diag_summary(cleaned_data_list$permanence$df, "permanence")
) %>%
  mutate(
    coverage = paste0(unique_studies, " of ", n_unique_studies, 
                      " (", round(100 * unique_studies / n_unique_studies, 1), "%)")
  )

print(coverage_diagnostics %>% select(dataset, rows, unique_studies, coverage))

# Row-level diagnostics
diagnostics_results <- list()

for (name in names(cleaned_data_list)) {
  item <- cleaned_data_list[[name]]
  diagnostics_results[[name]] <- run_missing_diagnostics(
    raw_df      = raw_df,
    cleaned_df  = item$df,
    raw_col     = item$raw_col,
    cleaned_name = name
  )
}

# ---------------------------
# 7. Merge back in key metadata from raw
# ---------------------------

metadata_cols <- raw_df %>%
  select(study_title, study_id, row_id, publication_year, evidence_type, offset_category_general, permanence_solutions_recommendations_discussed, reviewer_notes)

final_df <- joined_df %>%
  left_join(metadata_cols, by = "study_title")

# ---------------------------
# 8. Clean and rename final dataset
# ---------------------------

# Keep a clean study_id before dropping all suffix versions
final_df <- final_df %>%
  mutate(study_id = coalesce(study_id.x, study_id.y))  # or prioritize whichever version is better

# Then drop messy ones
drop_cols <- c(
  "study_id.x", "study_id.y", "study_id.x.x", "study_id.y.y", "study_id.x.x.x", "study_id.y.y.y",
  "row_id.x", "row_id.y", "row_id.x.x", "row_id.y.y", "row_id.x.x.x", "row_id.y.y.y", "study_id.y.y.y.y", 
  "row_id.y.y.y.y", "raw_delivery_type", "focal_species", "offset_program_name", "note",
  "policy_type", "jurisdiction_level", "status.y", "policy_legal_instrument_name","study_id.x.x.x.x", "row_id.x.x.x.x"  
)

final_df <- final_df %>%
  select(-any_of(drop_cols)) %>%
  rename(
    species_common_name       = standard_common_name,
    species_scientific_name   = standard_scientific_name,
    species_taxonomic_group   = taxonomic_group,
    ecosystem_type            = ecosystem_type_specific,
    ecosystem_broad_type      = broad_ecosystem,
    project_delivery_type     = standardized_delivery_type,
    project_type              = project_type_specific,
    project_broad_type        = intervention_type,
    program_name              = standardized_name.x,
    program_status            = status.x,
    program_mechanism_type    = mechanism_type,
    program_scope_level       = scope_level,
    program_scope_location    = scope_location,
    program_related           = related_program,
    policy_name               = standardized_name.y,
    policy_type               = policy_type_standardized,
    policy_note               = policy_type_notes,
    policy_jurisdiction_level = jurisdiction_level_standardized,
    policy_jurisdiction_note  = jurisdiction_level_notes,
    policy_jurisdiction_location = jurisdiction_location,
    policy_status             = status_standardized,
    policy_year_adopted       = year_adopted,
    policy_description        = description,
    permanence_risk_domain    = broad,
    permanence_risk_category  = specific,
    permanence_risk_type      = sub_risk,
    study_publication_year    = publication_year,
    study_evidence_type       = evidence_type
  )

# ---------------------------
# 9. Clean up columns before exporting
# ---------------------------

final_df <- final_df %>%
  select(
    # 1. Metadata
    study_title, study_id, row_id, study_publication_year, study_evidence_type, offset_category_general, reviewer_notes,
    
    # 2. Geography
    continent, country, subnational_region, subnational_region_type,
    
    # 3. Species & Ecosystem
    species_common_name, species_scientific_name, species_taxonomic_group,
    ecosystem_type, ecosystem_broad_type,
    
    # 4. Project
    project_type, project_broad_type,
    
    # 5. Program
    program_name, program_type, program_mechanism_type, program_status,
    program_scope_level, program_scope_location, program_related,
    
    # 6. Delivery
    project_delivery_type,
    
    # 7. Policy
    policy_name, policy_type, policy_note, policy_jurisdiction_level,
    policy_jurisdiction_note, policy_jurisdiction_location,
    policy_status, policy_year_adopted, policy_description,
    
    # 8. Permanence
    permanence_risk_domain, permanence_risk_category, permanence_risk_type,
    permanence_solutions_recommendations_discussed
  )

# ---------------------------
# 10. Export final dataset
# ---------------------------

output_dir <- here("data", "derived")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Correct: build full path using file.path()
write_csv(final_df, file.path(output_dir, "offset_perm_rev_long_cleaned.csv"))

message("✅ Cleaned and renamed long-format dataset saved to 'data/derived/offset_perm_rev_long_cleaned.csv'")
