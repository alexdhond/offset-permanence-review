# ---------------------------
# 1. Load packages
# ---------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(here)

# ---------------------------
# 2. Load messy data
# ---------------------------

data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names()

# ---------------------------
# 3. Define risk typology (from your tribble)
# ---------------------------

library(tibble)

risk_typology <- tribble(
  ~broad, ~specific, ~sub_risk,
  
  # Physical Permanence Risks
  "physical", "Direct Anthropogenic Disturbances", "Local-scale Activities",
  "physical", "Direct Anthropogenic Disturbances", "Landscape-level Pressures",
  "physical", "Climate and Environmental Disturbances", "Fire",
  "physical", "Climate and Environmental Disturbances", "Drought",
  "physical", "Climate and Environmental Disturbances", "Flooding",
  "physical", "Climate and Environmental Disturbances", "Invasive Species, Insects, and Pathogens",
  "physical", "Climate and Environmental Disturbances", "Extreme Weather and Force Majeure Events",
  "physical", "Climate and Environmental Disturbances", "Sea-level Rise",
  "physical", "Climate and Environmental Disturbances", "Soil Degradation",
  "physical", "Climate and Environmental Disturbances", "Climate Change",
  
  # Methodological, Technical, and Structural
  "methodological", "Ecological Design and Implementation Failures", "Poor Ecological Design",
  "methodological", "Ecological Design and Implementation Failures", "Implementation Failures",
  "methodological", "Misaligned Metrics, Standards, and Performance Criteria", "Oversimplified and Ecologically Misaligned Metrics",
  "methodological", "Misaligned Metrics, Standards, and Performance Criteria", "Weak Performance Standards",
  "methodological", "Systemic Oversights and Risk Management Gaps", "Project Risk Oversight Gaps",
  "methodological", "Systemic Oversights and Risk Management Gaps", "Buffer Pool Shortfalls",
  
  # Non-Physical Permanence Risks
  "non-physical", "Compliance, Legal, and Governance Risks", "Inadequate Policy Design",
  "non-physical", "Compliance, Legal, and Governance Risks", "Policy Non-compliance",
  "non-physical", "Compliance, Legal, and Governance Risks", "Weak Governance and Enforcement",
  "non-physical", "Compliance, Legal, and Governance Risks", "Corruption and Institutional Capture",
  "non-physical", "Compliance, Legal, and Governance Risks", "Insufficient Legal Protections",
  "non-physical", "Compliance, Legal, and Governance Risks", "Lack of Liability and Accountability",
  
  "non-physical", "Data, Transparency, and Capacity Issues", "Limited Data Transparency",
  "non-physical", "Data, Transparency, and Capacity Issues", "Poor Management and Monitoring",
  "non-physical", "Data, Transparency, and Capacity Issues", "Capacity, Expertise, and Resource Gaps",
  
  "non-physical", "Financial Risks", "Financial Mismanagement and Failure",
  "non-physical", "Financial Risks", "Inadequate Financial Planning",
  "non-physical", "Financial Risks", "Market Instability and Volatility",
  
  "non-physical", "Political Risks", "Regime Changes and Policy Reversal",
  "non-physical", "Political Risks", "Weak Political Will and Enforcement",
  "non-physical", "Political Risks", "Regulatory Inconsistency and Institutional Fragmentation",
  
  "non-physical", "Socioeconomic and Equity Risks", "Land Tenure and Access Conflicts",
  "non-physical", "Socioeconomic and Equity Risks", "Community Exclusion and Compensation Gaps",
  "non-physical", "Socioeconomic and Equity Risks", "Incentive Misalignment, Opportunity Costs, and Landowner Behaviour"
)

# ---------------------------
# 4. Explode by sub_risk only, then match via typology
# ---------------------------

exploded <- data %>%
  rowwise() %>%
  mutate(sub_risk = list(str_split(permanence_risk_subcategory, ";")[[1]] %>% str_trim())) %>%
  ungroup() %>%
  select(study_title, sub_risk) %>%
  unnest(sub_risk)

# ---------------------------
# 5. Join to get full typology info
# ---------------------------

cleaned_data <- exploded %>%
  left_join(risk_typology, by = "sub_risk")

# ---------------------------
# 6. Find unmatched (optional)
# ---------------------------

unmatched <- cleaned_data %>%
  filter(is.na(broad) | is.na(specific))

# ---------------------------
# 7. Export
# ---------------------------

write_csv(cleaned_data, here("data", "processed", "cleaned_permanence_risks.csv"))
write_csv(unmatched, here("data", "intermediate", "unmatched_permanence_risks.csv"))
