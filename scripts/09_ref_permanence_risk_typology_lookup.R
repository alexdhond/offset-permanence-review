# ================================================================
# Script:     09_ref_permanence_risk_typology_lookup.R
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
library(tibble)

# ---------------------------
# 2. Define permanence risk typology
# ---------------------------

risk_typology <- tribble(
  ~broad,         ~specific,                                          ~sub_risk,
  
  # --- Physical Permanence Risks ---
  "physical",     "Direct Anthropogenic Disturbances",               "Local-scale Activities",
  "physical",     "Direct Anthropogenic Disturbances",               "Landscape-level Pressures",
  "physical",     "Climate and Environmental Disturbances",          "Fire",
  "physical",     "Climate and Environmental Disturbances",          "Drought",
  "physical",     "Climate and Environmental Disturbances",          "Flooding",
  "physical",     "Climate and Environmental Disturbances",          "Invasive Species, Insects, and Pathogens",
  "physical",     "Climate and Environmental Disturbances",          "Extreme Weather and Force Majeure Events",
  "physical",     "Climate and Environmental Disturbances",          "Sea-level Rise",
  "physical",     "Climate and Environmental Disturbances",          "Soil Degradation",
  "physical",     "Climate and Environmental Disturbances",          "Climate Change",
  
  # --- Methodological Risks ---
  "methodological", "Ecological Design and Implementation Failures", "Poor Ecological Design",
  "methodological", "Ecological Design and Implementation Failures", "Implementation Failures",
  "methodological", "Misaligned Metrics, Standards, and Performance Criteria", "Oversimplified and Ecologically Misaligned Metrics",
  "methodological", "Misaligned Metrics, Standards, and Performance Criteria", "Weak Performance Standards",
  "methodological", "Systemic Oversights and Risk Management Gaps", "Project Risk Oversight Gaps",
  "methodological", "Systemic Oversights and Risk Management Gaps", "Buffer Pool Shortfalls",
  
  # --- Non-Physical Risks: Legal & Governance ---
  "non-physical", "Compliance, Legal, and Governance Risks",         "Inadequate Policy Design",
  "non-physical", "Compliance, Legal, and Governance Risks",         "Policy Non-compliance",
  "non-physical", "Compliance, Legal, and Governance Risks",         "Weak Governance and Enforcement",
  "non-physical", "Compliance, Legal, and Governance Risks",         "Corruption and Institutional Capture",
  "non-physical", "Compliance, Legal, and Governance Risks",         "Insufficient Legal Protections",
  "non-physical", "Compliance, Legal, and Governance Risks",         "Lack of Liability and Accountability",
  
  # --- Non-Physical Risks: Data & Capacity ---
  "non-physical", "Data, Transparency, and Capacity Issues",         "Limited Data Transparency",
  "non-physical", "Data, Transparency, and Capacity Issues",         "Poor Management and Monitoring",
  "non-physical", "Data, Transparency, and Capacity Issues",         "Capacity, Expertise, and Resource Gaps",
  
  # --- Non-Physical Risks: Financial ---
  "non-physical", "Financial Risks",                                 "Financial Mismanagement and Failure",
  "non-physical", "Financial Risks",                                 "Inadequate Financial Planning",
  "non-physical", "Financial Risks",                                 "Market Instability and Volatility",
  
  # --- Non-Physical Risks: Political ---
  "non-physical", "Political Risks",                                 "Regime Changes and Policy Reversal",
  "non-physical", "Political Risks",                                 "Weak Political Will and Enforcement",
  "non-physical", "Political Risks",                                 "Regulatory Inconsistency and Institutional Fragmentation",
  
  # --- Non-Physical Risks: Social & Equity ---
  "non-physical", "Socioeconomic and Equity Risks",                  "Land Tenure and Access Conflicts",
  "non-physical", "Socioeconomic and Equity Risks",                  "Community Exclusion and Compensation Gaps",
  "non-physical", "Socioeconomic and Equity Risks",                  "Incentive Misalignment, Opportunity Costs, and Landowner Behaviour"
)

# ---------------------------
# 3. Export lookup table
# ---------------------------

output_dir <- here("data", "reference")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(risk_typology, file.path(output_dir, "permanence_risk_typology_lookup.csv"))
