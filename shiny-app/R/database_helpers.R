# ============================================================================
# database_helpers.R - Database Helper Functions and Data Loading
# ============================================================================
# Purpose: Load and prepare study data for the database tab
#
# Key Variables Exported:
#   - studies_data: Full database with all columns
#   - studies_data_display: Columns shown in Database tab table
#   - studies_data_download: Columns included in CSV download
#   - db_summary_stats: Precomputed summary statistics
#   - typology: Risk typology data
#
# Data Dependencies:
#   - data/offset_perm_rev_condensed.rds
#   - data/offset_perm_rev_database.rds
#   - data/study_citations.csv
#   - data/db_summary_stats.rds
#   - data/permanence_risk_typology_full_descriptions.rds
# ============================================================================

# Load condensed study data
studies_data <- readRDS("data/offset_perm_rev_condensed.rds")

# Join study_abstract and doi from excel_data (loaded in global.R)
excel_subset <- excel_data |> dplyr::select(study_title, study_abstract, doi, permanence_risk_evidence_and_reasoning)
studies_data <- studies_data |> dplyr::left_join(excel_subset, by = "study_title")

# --- Load citation lookup and join by closest study title ---
citations <- read.csv("data/study_citations.csv", stringsAsFactors = FALSE)

# Standardize column names for join
names(citations)[names(citations) == "Study.Title"] <- "study_title"
names(citations)[names(citations) == "Publication.Year"] <- "study_publication_year"

# Ensure types match for join
citations$study_publication_year <- as.character(citations$study_publication_year)
studies_data$study_publication_year <- as.character(studies_data$study_publication_year)

# Left join by study_title and study_publication_year
studies_data <- dplyr::left_join(
  studies_data,
  citations[, c("study_title", "study_publication_year", "Reference")],
  by = c("study_title", "study_publication_year")
)

# Rename Reference column to reference
names(studies_data)[names(studies_data) == "Reference"] <- "reference"


# ---- COLUMN CONTROL ----

# Specify columns to show in the Database tab table
db_display_columns <- c(
  "study_title",                  # Title of the study
  "study_publication_year",       # Year the study was published
  "offset_category_general",      # Offset type: biodiversity or carbon
  "study_evidence_type",          # Type of evidence in the study
  "country",                      # Country where study/project is based
  "subnational_region",           # Subnational region (if any)
  "ecosystem_broad_type",         # Broad ecosystem type (e.g., forest, wetland)
  "species_taxonomic_group",      # Taxonomic group(s) studied
  "project_type",                 # Type of offset project
  "program_name",                 # Name of program (if any)
  "policy_name",                  # Name of policy (if any)
  "permanence_risk_domain",       # High-level risk domain (e.g., governance)
  "permanence_risk_category",     # Risk category within domain
  "permanence_risk_type",         # Specific risk type
  "doi",                          # Digital Object Identifier for the study
  "reference"                     # Citation reference
)


# Specify columns to include in the Database tab download
db_download_columns <- c(
  "study_title",                        # Title of the study
  "study_publication_year",             # Year the study was published
  "offset_category_general",            # Offset type: biodiversity or carbon
  "study_evidence_type",                # Type of evidence in the study
  "country",                            # Country where study/project is based
  "subnational_region",                 # Subnational region (if any)
  "ecosystem_broad_type",               # Broad ecosystem type (e.g., forest, wetland)
  "species_taxonomic_group",            # Taxonomic group(s) studied
  "project_type",                       # Type of offset project
  "program_name",                       # Name of program (if any)
  "policy_name",                        # Name of policy (if any)
  "permanence_risk_domain",             # High-level risk domain (e.g., governance)
  "permanence_risk_category",           # Risk category within domain
  "permanence_risk_type",               # Specific risk type
  "doi",                                # Digital Object Identifier for the study
  "study_abstract",                     # Abstract of the study
  "permanence_risk_evidence_and_reasoning", # Textual evidence and reasoning for risk
  "reference"                           # Citation reference
)

# Data frame for table display in Database tab
studies_data_display <- studies_data[, db_display_columns, drop = FALSE]
# Data frame for download in Database tab
studies_data_download <- studies_data[, db_download_columns, drop = FALSE]



# Note: db_summary_stats and typology are loaded in global.R
# No need to reload them here

# Helper function to split semicolon-delimited risks
get_risks <- function(risk_type_str) {
  base::unlist(base::strsplit(base::as.character(risk_type_str), "; ?"))
}
