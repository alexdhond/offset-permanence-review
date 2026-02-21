# Script to harmonize country names in geo_lookup.csv to match those used by normalize_country()

library(dplyr)

# Read geo_lookup
geo_lookup <- read.csv("data/geo_lookup.csv", stringsAsFactors = FALSE)

# Manual mapping as in normalize_country()
country_map <- c(
  "United States of America" = "United States",
  "USA" = "United States",
  "US" = "United States",
  "UK" = "United Kingdom",
  "Republic of Korea" = "South Korea",
  "Russian Federation" = "Russia",
  "Viet Nam" = "Vietnam",
  "Czechia" = "Czech Republic",
  "Democratic Republic of the Congo" = "DRC",
  "Republic of the Congo" = "Congo",
  "Lao People's Democratic Republic" = "Laos",
  "Iran (Islamic Republic of)" = "Iran",
  "Syrian Arab Republic" = "Syria",
  "United Republic of Tanzania" = "Tanzania",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "Brunei Darussalam" = "Brunei",
  "Republic of Moldova" = "Moldova",
  "Republic of North Macedonia" = "North Macedonia",
  "Palestine, State of" = "Palestine",
  "Micronesia (Federated States of)" = "Micronesia",
  "Saint Kitts and Nevis" = "St. Kitts and Nevis",
  "Saint Lucia" = "St. Lucia",
  "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines"
)

# Apply mapping
geo_lookup$country <- ifelse(geo_lookup$country %in% names(country_map), country_map[geo_lookup$country], geo_lookup$country)

# Write harmonized file
write.csv(geo_lookup, "data/geo_lookup_harmonized.csv", row.names = FALSE)

cat("Harmonized geo_lookup written to data/geo_lookup_harmonized.csv\n")
