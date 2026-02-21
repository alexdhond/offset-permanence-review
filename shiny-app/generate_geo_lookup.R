# ============================================================================
# generate_geo_lookup.R - Geographic Lookup Table Generator
# ============================================================================
# Purpose: Extract all unique country/subnational_region pairs and generate
#          geo_lookup.rds with coordinates for mapping
#
# Usage: Run this script manually when country/region data changes:
#        source("R/generate_geo_lookup.R")
#
# Data Sources:
#   - data/country_clean_valid.rds
#   - data/offset_perm_rev_long_cleaned.rds
#
# Output:
#   - data/geo_lookup.rds (country, subnational_region, lat, lng)
# ============================================================================

library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)



# Load all unique pairs from both sources, robust to NA/blank/"NA" subnational
dat1 <- readRDS("data/country_clean_valid.rds")
dat2 <- readRDS("data/offset_perm_rev_long_cleaned.rds")
all_pairs <- (
  dplyr::bind_rows(
    dat1 |> dplyr::select(country, subnational_region),
    dat2 |> dplyr::select(country, subnational_region)
  )
) |> 
  dplyr::mutate(
    country = trimws(as.character(country)),
    subnational_region = trimws(as.character(subnational_region)),
    subnational_region = ifelse(is.na(subnational_region) | tolower(subnational_region) %in% c("na", "", " "), "", subnational_region)
  ) |> 
  dplyr::distinct() |> 
  dplyr::arrange(country, subnational_region)



# Load existing geo_lookup.rds if present
rds_path <- "data/geo_lookup.rds"
if (file.exists(rds_path)) {
  geo_lookup <- readRDS(rds_path)
} else {
  geo_lookup <- data.frame(country=character(), subnational_region=character(), lat=double(), lng=double(), stringsAsFactors=FALSE)
}

# Left join to preserve any existing lat/lng
out <- dplyr::left_join(all_pairs, geo_lookup, by = c("country", "subnational_region"))

# Helper: get country centroid
get_country_centroid <- function(country_name) {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  match <- world[world$name == country_name | world$admin == country_name, ]
  if (nrow(match) == 0) return(c(NA, NA))
  centroid <- suppressWarnings(st_centroid(st_geometry(match)))
  coords <- st_coordinates(centroid)[1,]
  c(coords[2], coords[1]) # lat, lng
}


# Helper: get subnational centroid (first-level admin)
get_subnat_centroid <- function(country_name, subnat_name) {
  if (country_name %in% c("United States", "United States of America")) {
    # Use us_states shapefile loaded in global.R
    match <- us_states[us_states$name == subnat_name, ]
    if (nrow(match) == 0) return(NULL)
    centroid <- suppressWarnings(st_centroid(st_geometry(match)))
    coords <- st_coordinates(centroid)[1,]
    return(c(coords[2], coords[1])) # lat, lng
  } else {
    adm1 <- tryCatch(ne_states(country = country_name, returnclass = "sf"), error = function(e) NULL)
    if (is.null(adm1)) return(NULL)
    match <- adm1[adm1$name == subnat_name | adm1$name_en == subnat_name, ]
    if (nrow(match) == 0) return(NULL)
    centroid <- suppressWarnings(st_centroid(st_geometry(match)))
    coords <- st_coordinates(centroid)[1,]
    return(c(coords[2], coords[1])) # lat, lng
  }
}

# Fill missing lat/lng for all pairs, always try country centroid if subnational is blank/NA
for (i in seq_len(nrow(out))) {
  if (is.na(out$lat[i]) || is.na(out$lng[i])) {
    subnat <- out$subnational_region[i]
    country <- out$country[i]
    coords <- NULL
    if (!is.na(subnat) && nzchar(subnat)) {
      coords <- get_subnat_centroid(country, subnat)
    }
    # If subnational is blank, or subnational lookup failed, always try country centroid
    if (is.null(coords) || any(is.na(coords)) || subnat == "") {
      coords <- get_country_centroid(country)
    }
    if (!is.null(coords) && !any(is.na(coords))) {
      out$lat[i] <- coords[1]
      out$lng[i] <- coords[2]
    }
  }
}

# Explicitly assign lat/lon for European Union (centered at Brussels)
eu_idx <- which(out$country == "European Union" & (is.na(out$lat) | is.na(out$lng) | out$lat == "" | out$lng == ""))
if (length(eu_idx) > 0) {
  out$lat[eu_idx] <- 50.8503
  out$lng[eu_idx] <- 4.3517
}

# Harmonize country names to match mapping code
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
  "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
  "European Union" = "European Union")
out$country <- ifelse(out$country %in% names(country_map), country_map[out$country], out$country)


# Write updated lookup as RDS only
dir.create("data", showWarnings = FALSE)
saveRDS(out, rds_path)