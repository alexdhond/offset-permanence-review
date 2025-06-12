# ================================================================
# Script:     ref_offset_policy_lookup.R
# Date:       2025-06-12
# Author:     Alex Dhond
# Purpose:    Create a standardized lookup table for offset-related policy names
# ================================================================

# ---------------------------
# 1. Load packages
# ---------------------------

library(tibble)
library(tidyverse)
library(here)
library(readxl)
library(janitor)

# ---------------------------
# 2. Load and prepare data
# ---------------------------

data <- read_excel(here("data", "offset_perm_rev_database.xlsx")) %>%
  clean_names() %>%
  mutate(row_id = row_number())

# Explode semicolon-separated policy fields
policy_exploded <- data %>%
  select(row_id, study_title, policy_legal_instrument_name, year_of_policy_adoption, policy_jurisdiction) %>%
  mutate(across(everything(), ~ str_split(.x, ";\\s*"))) %>%
  unnest_longer(policy_legal_instrument_name) %>%
  unnest_longer(year_of_policy_adoption) %>%
  unnest_longer(policy_jurisdiction) %>%
  mutate(across(everything(), str_trim)) %>%
  filter(!is.na(policy_legal_instrument_name), policy_legal_instrument_name != "")

# ---------------------------
# 3. Define policy lookup table
# ---------------------------

policy_lookup <- tribble(
  ~original_name,                                                               ~standardized_name,                               ~policy_type,               ~jurisdiction_level,   ~jurisdiction_location,     ~status,     ~year_adopted, ~description,
  
  # --- USA ---
  "US Clean Water Act section 404 (1972)",                                      "US Clean Water Act Section 404",                 "legislation",              "national",          "USA",                  "active",    1972,         "Section 404 governs permitting for wetland and stream impacts.",
  "US Clean Water Act Section 404 (1972)",                                      "US Clean Water Act Section 404",                 "legislation",              "national",          "USA",                  "active",    1972,         "Section 404 governs permitting for wetland and stream impacts.",
  "US Endangered Species Act (1973)",                                           "US Endangered Species Act",                      "legislation",              "national",          "USA",                  "active",    1973,         "Establishes protections for listed species and their habitats and enables use of conservation banking.",
  "California AB 32 Global Warming Solutions Act (2006)",                       "California Global Warming Solutions Act",        "legislation",              "subnational",       "USA",                  "active",    2006,         "Mandated GHG emissions reductions and enabled the creation of California's Cap-and-Trade Program.",
  "Global Warming Solutions Act (2006)",                                        "California Global Warming Solutions Act",        "legislation",              "subnational",       "USA",                  "active",    2006,         "Mandated GHG emissions reductions and enabled the creation of California's Cap-and-Trade Program.",
  "Massachusetts Wetlands Protection Act (1963)",                               "Massachusetts Wetland Protection Act",           "legislation",              "subnational", "USA", "active", 1963, "Provides state-level protection of wetlands in Massachusetts, requiring permits and mitigation for activities that may alter wetland areas.",
  "Michigan Natural Resources and Environmental Protection Act (1994)",         "Michigan NREPA Part 303",                        "legislation",              "subnational", "USA", "active", 1994, "Regulates wetlands protection in Michigan, including permitting and mitigation provisions.",
  "RGGI Model Rule (2005)",                                                     "RGGI Model Rule",                                "model rule",               "multi-jurisdiction", "USA", "active", 2005, "Template regulation adopted by participating northeastern US states under the Regional Greenhouse Gas Initiative (RGGI), enabling a cap-and-trade system including limited offset use.",
  
  # --- Canada ---
  "Canada Fisheries Act section 35(2) (1985)",                                  "Canada Fisheries Act",                           "legislation",              "national",          "Canada",               "active",    1985,         "Federal act regulating the protection and conservation of fish and fish habitat; key offsetting provisions under s.35.",
  "Canadian Fisheries Act (1985)",                                              "Canada Fisheries Act",                           "legislation",              "national",          "Canada",               "active",    1985,         "Federal act regulating the protection and conservation of fish and fish habitat; key offsetting provisions under s.35.",
  "Canada Fisheries Act section 35(2) (1976)",                                  "Canada Fisheries Act",                           "legislation",              "national",          "Canada",               "active",    1985,         "Federal act regulating the protection and conservation of fish and fish habitat; key offsetting provisions under s.35.",
  "Canada Fisheries Act (1985)",                                                "Canada Fisheries Act",                           "legislation",              "national",          "Canada",               "active",    1985,         "Federal act regulating the protection and conservation of fish and fish habitat; key offsetting provisions under s.35.",
  "Federal Policy on Wetland Conservation (1991)",                              "Canada Wetland Policy",                           "policy",                  "national",          "Canada",               "active",    1991,         "Non-binding federal guidance.",
  "Quebec Environment Quality Act Section 22 (2006)",                           "Quebec Environment Quality Act",                  "legislation",              "subnational",       "Canada",               "active",    2006,         "Provides the legal basis for environmental assessment in Quebec, including requirements for compensatory measures such as habitat offsets under certain project approvals.",
  "Alberta Water Act (2000)",                                                   "Alberta Water Act",                               "legislation",              "subnational",       "Canada",               "active",    2000,         "Regulates water use and protection in Alberta, including wetlands management.",
  "New Brunswick Wetlands Conservation Policy (2002)",                          "New Brunswick Wetlands Policy",                   "policy",                  "subnational",       "Canada",               "active",    2002,         "Provincial policy guiding wetlands conservation and potential offsets in New Brunswick.",
  "Newfoundland Policy Directive for Development in Wetlands (1997)",           "Newfoundland Wetland Directive",                  "policy directive",         "subnational",       "Canada",               "active",    1997,         "Development guidance for projects in wetland areas in Newfoundland.",
  "Nova Scotia Wetlands Designation Policy (2006)",                             "Nova Scotia Wetlands Policy",                     "policy",                  "subnational",       "Canada",               "active",    2006,         "Provincial policy for designating and managing wetlands in Nova Scotia.",
  "Wetland Conservation Policy for Prince Edward Island (2003)",                "PEI Wetland Policy",                              "policy",                  "subnational",       "Canada",               "active",    2003,         "Guides wetland conservation and potential offsetting in Prince Edward Island.",
  
  # --- Australia ---
  "Australia Carbon Pricing Mechanism (2011)",                                  "Australia Clean Energy Act 2011",                 "legislation",              "national",          "Australia",            "repealed",  2011,         "Established the Carbon Pricing Mechanism; implemented a fixed-price carbon tax from 2012–2014 with planned transition to emissions trading before repeal.",
  "Australia Carbon Credits (Carbon Farming Initiative) Act (2011)",            "Australia CFI Act",                               "legislation",              "national",          "Australia",            "active",    2011,         "Established the legal framework for issuing Australian Carbon Credit Units (ACCUs) for eligible emissions reduction and sequestration projects, including biodiversity co-benefits.",
  "Carbon Credits (Carbon Farming Initiative) Act (2011)",                      "Australia CFI Act",                               "legislation",              "national",          "Australia",            "active",    2011,         "Established the legal framework for issuing Australian Carbon Credit Units (ACCUs) for eligible emissions reduction and sequestration projects, including biodiversity co-benefits.",
  "Environment Protection and Biodiversity Conservation Act (1999)",            "Australia EPBC Act",                              "legislation",              "national",          "Australia",            "active",    1999,         "Primary environmental legislation in Australia; includes provisions for biodiversity conservation and offsets under specific approvals.",
  "Australian EPBC Act Environmental Offsets Policy",                           "Australia EPBC Offsets Policy",                   "policy",                  "national",          "Australia",            "active",    2012,           "Outlines how environmental offsets are implemented under the EPBC Act.",
  "Australian EPBC Act Environmental Offsets Policy (2012)",                    "Australia EPBC Offsets Policy",                   "policy",                  "national",          "Australia",            "active",    2012,         "Outlines how environmental offsets are implemented under the EPBC Act.",
  
  
  ### New South Wales ###
  "NSW Biodiversity Conservation Act (2016)",                                   "NSW Biodiversity Conservation Act",               "legislation",              "subnational",       "Australia",            "active",    2016,         "Modernized biodiversity legislation in NSW that integrates offsetting via the Biodiversity Assessment Method and biodiversity credits.",
  "NSW Native Vegetation Act (2005)",                                           "NSW Native Vegetation Act (2005)",                       "legislation",              "subnational",       "Australia",            "replaced",  2005,         "Repealed and replaced by Biodiversity Act.",
  "NSW Threatened Species Conservation Act (1995)",                             "NSW Threatened Species Conservation Act", "legislation", "subnational", "Australia", "repealed", 1995, "Provided protections for threatened species in NSW and laid groundwork for biodiversity offsetting before being replaced in 2016.",
  "NSW Threatened Species Conservation Amendment (Biodiversity Banking) Act (2006)", "NSW BioBanking Amendment Act", "legislation", "subnational", "Australia", "repealed", 2006, "Established the BioBanking Scheme under the NSW Threatened Species Act; created a biodiversity credit system for offsets.",
  "New South Wales Native Vegetation Act (2003)",                               "NSW Native Vegetation Act (2003)", "legislation", "subnational", "Australia", "repealed", 2003, "Regulated native vegetation clearing in NSW and allowed offsets for approved clearing; repealed and replaced by the 2005 Act.",
  "Environmental Planning and Assessment Act (1979) (NSW)",                     "NSW Environmental Planning Act",                  "legislation",              "subnational",       "Australia",            "active",    1979,         "Governs planning and development in NSW, with provisions for biodiversity offsets.",
  
  ### Victoria ###
  "Victoria Native Vegetation Management Framework (2002)",                     "Victoria Native Vegetation Framework",            "policy",                  "subnational",       "Australia",            "active",    2002,         "Establishes 'avoid, minimise, offset' requirements for native vegetation removal in Victoria; cornerstone of offsetting policy in the state.",
  "Victoria's Native Vegetation Management Framework",                          "Victoria Native Vegetation Framework",            "policy",                  "subnational",       "Australia",            "active",    2002,         "Outlines how to avoid, minimise and offset native vegetation loss in Victoria.",
  "Victoria Native Vegetation Framework (2002)",                                "Victoria Native Vegetation Framework",            "policy",                   "subnational", "Australia", "active", 2002,                       "Outlines Victoria’s 'avoid, minimise, offset' framework for native vegetation loss; foundational guidance for biodiversity offsetting in the state.",
  "Victoria Planning and Environment Act (1987)",                               "Victoria Planning and Environment Act",           "legislation",         "subnational", "Australia", "active", 1987, "Establishes the legal framework for land use planning and development in Victoria, including environmental impact assessment and integration of biodiversity offsetting policy.",
  "Planning and Environment Act (VIC, 1987)",                                   "Victoria Planning and Environment Act",           "legislation",              "subnational",       "Australia",            "active",    1987,         "Framework legislation for planning and development, including biodiversity considerations.",
  
  
  ### Western Australia ###
  "Western Australia Environmental Protection Act (1986)",                      "WA Environmental Protection Act", "legislation", "subnational", "Australia", "active", 1986, "Provides the framework for environmental impact assessment and approval processes in WA, including conditions for biodiversity offsets.",
  "Environmental Protection Act (1986) (WA)",                                   "WA Environmental Protection Act",                 "legislation",              "subnational",       "Australia",            "active",    1986,         "Regulates environmental approvals in WA, including offset conditions.",
  "WA Environmental Offsets Policy (2011)",                                     "WA Environmental Offsets Policy",                 "policy",                  "subnational",       "Australia",            "active",    2011,         "Guides use of offsets under WA Environmental Protection Act.",
  
  ### Capital Territory ### 
  "ACT Environmental Offsets Policy (2015)",                                    "ACT Environmental Offsets Policy",                "policy",                  "subnational",       "Australia",            "active",    2015,         "Guides offset requirements for environmental impacts in the Australian Capital Territory.",
  
  ### Other Aus states ### 
  "Forest Practices Act (TAS, 1985)",                                           "Tasmania Forest Practices Act",                   "legislation",              "subnational",       "Australia",            "active",    1985,         "Regulates forest management and includes provisions for biodiversity protection in Tasmania.",
  "Native Vegetation Act (SA, 1991)",                                           "SA Native Vegetation Act",                        "legislation",              "subnational",       "Australia",            "active",    1991,         "Governs clearing and offsetting of native vegetation in South Australia.",
  "Queensland Environmental Offsets Act (2014)",                                "Queensland Environmental Offsets Act",            "legislation",              "subnational",       "Australia",            "active",    2014,         "Provides legal framework for biodiversity and environmental offsets in Queensland.",

  # --- New Zealand ---
  "New Zealand Resource Management Act (1991)",                                 "NZ Resource Management Act",                      "legislation",              "national",          "New Zealand",          "active",    1991,         "Framework for land and environmental planning.",
  "NZ Resource Management Act (1991)",                                          "NZ Resource Management Act",                      "legislation",              "national",          "New Zealand",          "active",    1991,         "Framework for land and environmental planning.",
  "New Zealand Climate Change Response Act (2002)",                             "NZ Climate Change Response Act",                  "legislation",              "national",          "New Zealand",          "active",    2002,         "Established legislative foundation for NZ ETS",
  "Climate Change Response Act (2002)",                                         "NZ Climate Change Response Act",                  "legislation",              "national",          "New Zealand",          "active",    2002,         "Established legislative foundation for NZ ETS",
  "NZ Conservation Act (1987)",                                                 "NZ Conservation Act",             "legislation", "national", "New Zealand", "active", 1987, "Established the Department of Conservation and provides the framework for conservation of natural resources; while not an offsetting law, it underpins biodiversity management relevant to offsets.",
  
  # --- UK ---
  "Conservation of Habitats and Species Regulations (2017)",                    "UK Habitats and Species Regulations",             "regulation",               "national",          "UK",                   "active",    2017,         "Implements EU Habitats Directive in UK law; includes compensatory measures when damage to protected sites cannot be avoided.",
  "Conservation of Habitats and Species Regulations (2010)",                    "UK Habitats and Species Regulations",             "regulation",               "national",          "UK",                   "active",    2017,         "Implements EU Habitats Directive in UK law; includes compensatory measures when damage to protected sites cannot be avoided.",
  "UK Conservation of Habitats and Species Regulations (1994)",                 "UK Habitats and Species Regulations",             "regulation",               "national",          "UK",                   "active",    2017,         "Implements EU Habitats Directive in UK law; includes compensatory measures when damage to protected sites cannot be avoided.",
  "Environment Act (2021)",                                                     "UK Environment Act",                              "legislation",              "national",          "UK",                   "active",    2021,         "Mandated biodiversity net gain in planning.",
  "UK Environment Act (2021)",                                                  "UK Environment Act",                              "legislation",              "national",          "UK",                   "active",    2021,         "Mandated biodiversity net gain in planning.",
  "UK National Planning Policy Framework (2012)",                               "UK Planning Policy Framework", "policy", "national", "UK", "active", 2012, "Provides overarching planning guidance in England, including expectations for biodiversity conservation and support for biodiversity offsetting as part of sustainable development.",
  "UK Wildlife and Countryside Act (1981)",                                     "UK Wildlife and Countryside Act", "legislation", "national", "UK", "active", 1981, "Core UK legislation for wildlife protection, including species and habitat conservation; provides a basis for designations such as Sites of Special Scientific Interest (SSSIs).",
  
  # --- EU ---
  "EU Habitats Directive (92/43/EEC)",                                          "EU Habitats Directive",                           "directive",                "multi-jurisdiction", "EU",                   "active",    1992,         "Requires Member States to ensure conservation of habitats and species; includes obligation for compensatory measures (offsets) under Article 6.4.",
  "EU Habitats Directive (1992)",                                               "EU Habitats Directive",                           "directive",                "multi-jurisdiction", "EU",                   "active",    1992,         "Requires Member States to ensure conservation of habitats and species; includes obligation for compensatory measures (offsets) under Article 6.4.",
  "EU Water Framework Directive (2000)",                                        "EU Water Framework Directive",                    "directive",                "multi-jurisdiction", "EU",                   "active",    2000,         "Framework for protecting and restoring water bodies in the EU; may trigger offset-like mitigation where hydrological impacts are significant.",
  "Dutch Nature Protection Act",                                                "Netherlands Nature Protection Act", "legislation", "national", "Netherlands", "repealed", 1998, "Implemented aspects of the EU Habitats Directive, including provisions requiring compensation for impacts on protected areas; relevant precursor to biodiversity offset practice and no net loss policies in the Netherlands.",
  "EU Birds Directive (2009)",                                                  "EU Birds Directive", "directive", "multi-jurisdiction", "EU", "active", 2009, "Protects all wild bird species in the EU; enables habitat protection that may trigger compensatory measures under the Habitats Directive when Natura 2000 sites are impacted.",
  
  # --- Germany ---
  "Federal Nature Conservation Act (1976)",                                     "Germany Nature Conservation Act", "legislation", "national", "Germany", "active", 1976, "Established the Eingriffsregelung system, requiring avoidance, minimization, and ecological compensation (offsets) for significant impacts on nature and landscape.",
  "German Federal Nature Conservation Act (1976)",                              "Germany Nature Conservation Act", "legislation", "national", "Germany", "active", 1976, "Established the Eingriffsregelung system, requiring avoidance, minimization, and ecological compensation (offsets) for significant impacts on nature and landscape.",
  
  # --- Italy ---
  "Italian Decree No. 357/1997 (1997)",                                         "Italy Decree 357/1997", "decree", "national", "Italy", "active", 1997, "Transposes the EU Habitats Directive; requires compensatory measures for adverse impacts on Natura 2000 sites.",
  "Italian Decree No. 227/2001 (2001)",                                         "Italy Forestry Decree 227/2001", "decree", "national", "Italy", "active", 2001, "Promotes sustainable forest management and incentives for afforestation; indirectly supports ecological compensation in planning.",
  
  # --- France ---
  "French Biodiversity Law No. 2016-1087",                                      "France Biodiversity Law",               "legislation",              "national",          "France",               "active",    2016,         "Codified offsetting rules and strengthened the mitigation hierarchy in France.",
  "French Law on Biodiversity (2016)",                                          "France Biodiversity Law",               "legislation",              "national",          "France",               "active",    2016,         "Codified offsetting rules and strengthened the mitigation hierarchy in France.",
  "France Biodiversity Law (2016)",                                             "France Biodiversity Law",               "legislation",              "national",          "France",               "active",    2016,         "Codified offsetting rules and strengthened the mitigation hierarchy in France.",
  "French Environmental Code",                                                  "French Environmental Code",             "code",                    "national",          "France",               "active",    2000,           "Consolidated legal code governing environmental protection, including biodiversity.",
  
  # --- Spain --- 
  "Spain Environmental Assessment Law (2013)", "Spain Environmental Assessment Law", "legislation", "national", "Spain", "active", 2013, "Governs environmental assessment processes in Spain and includes provisions for applying the mitigation hierarchy, including biodiversity offsets where significant impacts occur.",
  
  # --- Sweden ---
  "Swedish Environmental Code", "Sweden Environmental Code", "code", "national", "Sweden", "active", 1999, "Comprehensive environmental legislation consolidating multiple acts, including provisions for ecological compensation and the application of the mitigation hierarchy.",
  
  # --- India ---
  "Compensatory Afforestation Fund Act (2016)",                                 "India Compensatory Afforestation Fund Act", "legislation", "national", "India", "active", 2016, "Establishes a national fund and authority (CAMPA) to manage compensatory afforestation and net present value payments for diverted forest land, supporting afforestation, biodiversity conservation, and ecological restoration.",
  "Forest Conservation Act (1980)",                                             "India Forest Conservation Act", "legislation", "national", "India", "active", 1980, "Requires central approval for non-forest use of forest land and mandates compensatory afforestation as an offset mechanism.",
  
  # --- China ---
  "Interim Measures for CCERs (2012)",                                          "China CCER Interim Measures", "regulation", "national", "China", "suspended", 2012, "Established the legal framework for China’s Certified Emission Reductions (CCERs) under the voluntary carbon market; suspended in 2017 pending reform.",
  
  # --- South Africa ---
  "South Africa Biodiversity Act (2004)",                                       "South Africa Biodiversity Act", "legislation", "national", "South Africa", "active", 2004, "Provides a framework for managing and conserving biodiversity in South Africa, including provisions that support the use of biodiversity offsets under certain conditions.",
  "South Africa National Environmental Management Act (1998)",                  "South Africa NEMA", "framework legislation", "national", "South Africa", "active", 1998, "Core environmental management legislation establishing the principles for environmental decision-making, including environmental impact assessments (EIAs) and support for biodiversity offset frameworks.",
  "South Africa Protected Areas Act (2003)",                                    "South Africa Protected Areas Act", "legislation", "national", "South Africa", "active", 2003, "Establishes legal protection and management for protected areas; supports the use of offsets to secure additional conservation outcomes in designated areas.",
  
  
  # --- International ---
  "Kyoto Protocol (1997)",                                                      "Kyoto Protocol",                         "international agreement",  "international",     "Global",               "historical",1997,         "Introduced CDM for carbon offsets.",
  "UNFCCC Framework Convention (1992)",                                         "UNFCCC REDD+", "framework", "international", "Global", "active", 2005, "Developed under the UNFCCC to incentivize emissions reductions through avoided deforestation and forest degradation; includes safeguards and provisions for biodiversity co-benefits.",
  "UNFCCC REDD+ mechanism",                                                     "UNFCCC REDD+", "framework", "international", "Global", "active", 2005, "Developed under the UNFCCC to incentivize emissions reductions through avoided deforestation and forest degradation; includes safeguards and provisions for biodiversity co-benefits."
  
)

# ---------------------------
# 4. Join main dataset with lookup
# ---------------------------

policy_standardized <- policy_exploded %>%
  left_join(policy_lookup, by = c("policy_legal_instrument_name" = "original_name"))

# ---------------------------
# 5. Identify unmapped and incomplete entries
# ---------------------------

# Policies not found in the lookup
unmapped_policies <- policy_standardized %>%
  filter(is.na(standardized_name)) %>%
  distinct(policy_legal_instrument_name, study_title) %>%
  arrange(policy_legal_instrument_name)

unmapped_policy_counts <- policy_standardized %>%
  filter(is.na(standardized_name)) %>%
  count(policy_legal_instrument_name, sort = TRUE)

# Policies in lookup with missing key fields
policy_lookup_nas <- policy_lookup %>%
  filter(if_any(c(
    standardized_name, policy_type,
    jurisdiction_level, jurisdiction_location,
    status, year_adopted, description
  ), is.na))

# ---------------------------
# 6. Output diagnostics
# ---------------------------

if (nrow(unmapped_policies) > 0) {
  warning("⚠️ Some policy names were not standardized. See `unmapped_policies` and `unmapped_policy_counts`.")
  print(unmapped_policy_counts)
} else {
  message("✅ All policy names successfully standardized.")
}

if (nrow(policy_lookup_nas) > 0) {
  warning("⚠️ The policy lookup table has missing values. See `policy_lookup_nas` for details.")
  print(policy_lookup_nas)
}

# ---------------------------
# 7. Standardize values
# ---------------------------

# (a) Standardize `status`
policy_lookup <- policy_lookup %>%
  mutate(status_standardized = case_when(
    status == "active" ~ "active",
    status %in% c("repealed", "replaced", "suspended", "historical") ~ "inactive",
    TRUE ~ NA_character_
  ))

# (b) Standardize `policy_type` and annotate
policy_lookup <- policy_lookup %>%
  mutate(
    policy_type_standardized = case_when(
      policy_type == "legislation" ~ "legislation",
      policy_type == "regulation" ~ "regulation",
      policy_type == "directive" ~ "directive",
      policy_type == "model rule" ~ "model_rule",
      policy_type %in% c("policy", "policy directive") ~ "policy",
      policy_type == "code" ~ "code",
      policy_type == "decree" ~ "decree",
      policy_type %in% c("framework", "framework legislation") ~ "framework",
      policy_type == "international agreement" ~ "international_agreement",
      TRUE ~ NA_character_
    ),
    policy_type_notes = case_when(
      policy_type_standardized == "legislation" ~ "Statutory law passed by a legislature.",
      policy_type_standardized == "regulation" ~ "Binding rules issued under legislation.",
      policy_type_standardized == "directive" ~ "EU legal instrument binding member states to objectives.",
      policy_type_standardized == "model_rule" ~ "Template legal text for jurisdictions to adopt.",
      policy_type_standardized == "policy" ~ "Non-binding guidance or strategy.",
      policy_type_standardized == "code" ~ "Codified set of laws or regulations.",
      policy_type_standardized == "decree" ~ "Legally binding executive order or administrative rule.",
      policy_type_standardized == "framework" ~ "Overarching legislation or policy providing foundational principles.",
      policy_type_standardized == "international_agreement" ~ "Legally binding agreement among countries.",
      TRUE ~ NA_character_
    )
  )

# (c) Standardize `jurisdiction_level`
policy_lookup <- policy_lookup %>%
  mutate(
    jurisdiction_level_standardized = case_when(
      jurisdiction_level == "national" ~ "national",
      jurisdiction_level == "subnational" ~ "subnational",
      jurisdiction_level == "multi-jurisdiction" ~ "regional",
      jurisdiction_level == "international" ~ "international",
      TRUE ~ NA_character_
    ),
    jurisdiction_level_notes = case_when(
      jurisdiction_level_standardized == "national" ~ "Applies to a single country at federal level.",
      jurisdiction_level_standardized == "subnational" ~ "Applies to states, provinces, or regions.",
      jurisdiction_level_standardized == "regional" ~ "Applies across multiple countries within a region.",
      jurisdiction_level_standardized == "international" ~ "Applies globally or through international institutions.",
      TRUE ~ NA_character_
    )
  )

# ---------------------------
# 8. Quick summaries
# ---------------------------

# Number of unique values per column
policy_lookup %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_unique_values") %>%
  print()

# ---------------------------
# 9. Save final lookup table
# ---------------------------

write_csv(policy_lookup, here("data", "reference", "offset_policy_lookup.csv"))
