# ================================================================
# Script:     ref_permanence_risk_typology_lookup.R
# Date:       2025-06-09
# Author:     Alex Dhond
# Purpose:    Create and export lookup table for permanence risks:
#             subcategory → specific → broad
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(tidyverse)
library(here)
library(readxl)
library(janitor)

# ---------------------------
# 2. Define permanence risk typology
# ---------------------------

risk_typology <- tribble(
  ~broad,       ~specific,                               ~sub_risk,
  "Physical",   "Direct Anthropogenic Disturbances",     "Local-scale Activities",
  "Physical",   "Direct Anthropogenic Disturbances",     "Landscape-level Pressures",
  "Physical",   "Climate and Natural Disturbances",      "Fire",
  "Physical",   "Climate and Natural Disturbances",      "Drought",
  "Physical",   "Climate and Natural Disturbances",      "Flooding",
  "Physical",   "Climate and Natural Disturbances",      "Invasive Species, Insects, and Pathogens",
  "Physical",   "Climate and Natural Disturbances",      "Extreme Weather and Force Majeure Events",
  "Physical",   "Climate and Natural Disturbances",      "Sea-level Rise",
  "Physical",   "Climate and Natural Disturbances",      "Climate Change",
  "Physical",   "Methodological Failures",               "Poor Ecological Design and Planning",
  "Physical",   "Methodological Failures",               "Ineffective Restoration Techniques",
  "Physical",   "Methodological Failures",               "Misaligned Metrics and Standards",
  
  "Non-physical", "Compliance, Legal, and Governance Risks", "Inadequate Policy Design",
  "Non-physical", "Compliance, Legal, and Governance Risks", "Policy Non-compliance",
  "Non-physical", "Compliance, Legal, and Governance Risks", "Weak Governance and Enforcement",
  "Non-physical", "Compliance, Legal, and Governance Risks", "Corruption and Institutional Capture",
  "Non-physical", "Compliance, Legal, and Governance Risks", "Insufficient Legal Protections",
  "Non-physical", "Compliance, Legal, and Governance Risks", "Lack of Liability and Accountability",
  
  "Non-physical", "Data, Transparency, and Capacity Issues", "Limited Data Transparency",
  "Non-physical", "Data, Transparency, and Capacity Issues", "Poor Management and Monitoring",
  "Non-physical", "Data, Transparency, and Capacity Issues", "Capacity, Expertise, and Resource Gaps",
  
  "Non-physical", "Financial Risks", "Financial Mismanagement and Failure",
  "Non-physical", "Financial Risks", "Inadequate Financial Planning",
  "Non-physical", "Financial Risks", "Market Instability and Volatility",
  
  "Non-physical", "Political Risks", "Regime Changes and Policy Reversal",
  "Non-physical", "Political Risks", "Weak Political Will and Enforcement",
  "Non-physical", "Political Risks", "Regulatory Inconsistency and Institutional Fragmentation",
  
  "Non-physical", "Socioeconomic and Equity Risks", "Land Tenure and Access Conflicts",
  "Non-physical", "Socioeconomic and Equity Risks", "Community Exclusion and Compensation Gaps",
  "Non-physical", "Socioeconomic and Equity Risks", "Incentive Misalignment, Opportunity Costs, and Landowner Behavior"
)


# ---------------------------
# 3. Export lookup
# ---------------------------

# Create export directory if needed
output_dir <- here("data", "reference")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(risk_typology, file.path(output_dir, "permanence_risk_typology_lookup.csv"))

