# ============================================================================
# geo_helpers.R - Geographic Helper Functions
# ============================================================================
# Purpose: Functions for geographic lookups, country normalization, and
#          coordinate retrieval for study locations
#
# Key Functions:
#   - get_country_bbox(): Get bounding box for a country
#   - get_valid_country_region_pairs(): Load valid country/region pairs
#   - normalize_country(): Standardize country names
#   - get_study_coordinates_all(): Get coordinates for study locations
#   - expand_and_validate_locations(): Expand and validate location data
#
# Data Dependencies:
#   - data/geo_lookup.rds (loaded in global.R as geo_lookup)
#   - data/country_clean_valid.rds (loaded in global.R as valid_country_pairs)
#   - data/us_state_centroids.csv (for US state coordinates)
#   - data/country_name_aliases.csv (for country name mappings)
# ============================================================================

#' Get bounding box for a country (min_lng, min_lat, max_lng, max_lat)
#' Returns NULL if country not found
get_country_bbox <- function(country) {
  country <- normalize_country(country)
  bbox_lookup <- list(
    "United States" = c(min_lng = -125, min_lat = 24, max_lng = -66.9, max_lat = 49.4),
    "Canada" = c(min_lng = -141, min_lat = 41.7, max_lng = -52.6, max_lat = 83.1),
    "Australia" = c(min_lng = 112.9, min_lat = -43.7, max_lng = 153.6, max_lat = -10.7),
    "Brazil" = c(min_lng = -73.99, min_lat = -33.75, max_lng = -34.8, max_lat = 5.27),
    "China" = c(min_lng = 73.5, min_lat = 18.1, max_lng = 135.1, max_lat = 53.6),
    "India" = c(min_lng = 68.1, min_lat = 6.5, max_lng = 97.4, max_lat = 35.7),
    "Russia" = c(min_lng = 19.6, min_lat = 41.2, max_lng = 180, max_lat = 81.9),
    "United Kingdom" = c(min_lng = -8.6, min_lat = 49.9, max_lng = 1.8, max_lat = 60.9),
    "Mexico" = c(min_lng = -118.4, min_lat = 14.5, max_lng = -86.7, max_lat = 32.7),
    "South Africa" = c(min_lng = 16.5, min_lat = -34.8, max_lng = 32.9, max_lat = -22.1),
    "Argentina" = c(min_lng = -73.6, min_lat = -55.1, max_lng = -53.7, max_lat = -21.8),
    "Indonesia" = c(min_lng = 95, min_lat = -10.9, max_lng = 141, max_lat = 5.9),
    "European Union" = c(min_lng = -31.3, min_lat = 34.5, max_lng = 39.6, max_lat = 71.2)
    # Add more as needed
  )
  if (country %in% names(bbox_lookup)) {
    return(bbox_lookup[[country]])
  } else {
    return(NULL)
  }
}

#' Load valid country/subnational region pairs
#' Uses globally loaded valid_country_pairs from global.R
get_valid_country_region_pairs <- function() {
  # Use globally loaded data if available, otherwise load from file
  if (exists("valid_country_pairs", envir = globalenv())) {
    valid_df <- get("valid_country_pairs", envir = globalenv())
  } else {
    valid_path <- file.path("data", "country_clean_valid.rds")
    if (!file.exists(valid_path)) {
      stop("country_clean_valid.rds not found in data folder.")
    }
    valid_df <- readRDS(valid_path)
  }
  # Expect columns: country, subnational_region
  valid_df$country <- trimws(valid_df$country)
  valid_df$subnational_region <- trimws(valid_df$subnational_region)
  valid_df
}

#' Validate or correct a country/subnational_region pair
#' Returns a list with $country, $subnational_region, $valid (TRUE/FALSE)
validate_country_region_pair <- function(country, subnational_region, valid_pairs = NULL) {
  if (is.null(valid_pairs)) valid_pairs <- get_valid_country_region_pairs()
  country <- trimws(country)
  subnational_region <- trimws(subnational_region)
  # Accept NA/empty region as valid (country-only)
  if (is.na(subnational_region) || subnational_region == "") {
    valid <- country %in% valid_pairs$country
    return(list(country = country, subnational_region = subnational_region, valid = valid))
  }
  # Check for valid pair
  match <- valid_pairs$country == country & valid_pairs$subnational_region == subnational_region
  valid <- any(match)
  return(list(country = country, subnational_region = subnational_region, valid = valid))
}

#' Vectorized validation for a data.frame with country/subnational_region columns
validate_country_region_df <- function(df, valid_pairs = NULL) {
  if (is.null(valid_pairs)) valid_pairs <- get_valid_country_region_pairs()
  df$country <- trimws(df$country)
  df$subnational_region <- trimws(df$subnational_region)
  df$valid_pair <- mapply(function(cn, rn) validate_country_region_pair(cn, rn, valid_pairs)$valid, df$country, df$subnational_region)
  df
}

# ---- Load US State Centroids from CSV ----
# This allows easy maintenance of coordinates without code changes
load_us_state_centroids <- function() {
  csv_path <- "data/us_state_centroids.csv"
  if (file.exists(csv_path)) {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
    # Convert to named list format for backward compatibility
    coords <- list()
    for (i in seq_len(nrow(df))) {
      state <- df$state[i]
      if (state == "United States") {
        key <- "United States"
      } else {
        key <- paste0(state, ",United States")
      }
      coords[[key]] <- list(lat = df$lat[i], lng = df$lng[i])
    }
    return(coords)
  } else {
    # Fallback: return minimal default if CSV not found
    warning("us_state_centroids.csv not found, using minimal defaults")
    return(list("United States" = list(lat = 38.5, lng = -98.0)))
  }
}

# Custom coordinates for important subnational regions
# Loaded from CSV for maintainability
custom_region_coords <- load_us_state_centroids()
# ---- Load Country Name Aliases from CSV ----
# This allows easy maintenance of country name mappings without code changes
load_country_aliases <- function() {
  csv_path <- "data/country_name_aliases.csv"
  if (file.exists(csv_path)) {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
    # Convert to named vector for lookup
    aliases <- setNames(df$standard_name, df$alias)
    return(aliases)
  } else {
    # Fallback: return minimal default if CSV not found
    warning("country_name_aliases.csv not found, using minimal defaults")
    return(c("USA" = "United States", "US" = "United States", "UK" = "United Kingdom"))
  }
}

## Robust expansion and validation of country/subregion pairs for mapping
## Returns a data.frame with all valid (country, subnational_region, study_title, study_id) pairs
normalize_country <- function(country) {
  # Load aliases from CSV (cached after first load)
  custom_match <- load_country_aliases()

    country <- trimws(country)
    country <- ifelse(country %in% names(custom_match), custom_match[country], country)
    # Always accept 'European Union' as itself
    country[country == "European Union"] <- "European Union"
  if (requireNamespace("countrycode", quietly = TRUE)) {
    country <- countrycode::countrycode(country, "country.name", "country.name", warn = FALSE, custom_match = custom_match)
  }
  country[country == "United States of America"] <- "United States"
  country
}

expand_and_validate_locations <- function(study_df, valid_pairs_path = "data/country_clean_valid.rds") {
  # Load valid pairs
  valid_pairs <- readRDS(valid_pairs_path)
  valid_pairs$country <- trimws(as.character(valid_pairs$country))
  valid_pairs$subnational_region <- trimws(as.character(valid_pairs$subnational_region))
  # Helper to expand one row
    expand_row <- function(study) {
      # Diagnostic: print country/subnational_region for each study

    countries <- unlist(strsplit(as.character(study$country), ";|,| and "))
    countries <- trimws(countries[nzchar(countries)])
    subregions <- unlist(strsplit(as.character(study$subnational_region), ";|,| and "))
    subregions <- trimws(subregions[nzchar(subregions)])
    # If both are empty, skip
    # Special handling: if both 'Newfoundland' and 'Labrador' appear, combine as 'Newfoundland and Labrador'
    if ("Newfoundland" %in% subregions & "Labrador" %in% subregions) {
      subregions <- setdiff(subregions, c("Newfoundland", "Labrador"))
      subregions <- c(subregions, "Newfoundland and Labrador")
    }
    if (length(countries) == 0 && length(subregions) == 0) return(NULL)
    # If only countries, pair with NA
    if (length(subregions) == 0) subregions <- NA
    # If only subregions, pair with NA
    if (length(countries) == 0) countries <- NA
    # Cross-product all pairs
    expand.grid(
      country = countries,
      subnational_region = subregions,
      study_title = as.character(study$study_title),
      study_id = if ("study_id" %in% names(study)) as.character(study$study_id) else as.character(study$study_title),
      stringsAsFactors = FALSE
    )
  }
  expanded <- do.call(rbind, lapply(seq_len(nrow(study_df)), function(i) expand_row(study_df[i, ])))
  # Validate against country_clean_valid.rds
  expanded$valid_pair <- mapply(function(c, s) {
    any(valid_pairs$country == c & (is.na(valid_pairs$subnational_region) | valid_pairs$subnational_region == s))
  }, expanded$country, expanded$subnational_region)
  expanded
}
# geo_helpers.R
# Helper functions for geographic lookup and coordinates for study summary
# Note: geo_lookup is loaded globally in global.R - do not load here


#' Get all coordinates for a study's locations
#' @param countries Vector of country names
#' @param subnationals Vector of subnational region names (may be NA)
#' @return Data frame of lat, lng, label for all valid locations

get_study_coordinates_all <- function(countries, subnationals) {
  # Centralized normalization
  countries <- normalize_country(countries)
  subnationals <- as.character(subnationals)
  # Convert NA or blank subnational regions to empty string for join compatibility
  subnationals[is.na(subnationals) | tolower(subnationals) %in% c("na", "", " ")] <- ""
  # If input is a data frame, standardize subnational_region column as well
  if (is.data.frame(subnationals)) {
    subnationals$subnational_region <- as.character(subnationals$subnational_region)
    subnationals$subnational_region[is.na(subnationals$subnational_region) | tolower(subnationals$subnational_region) %in% c("na", "", " ")] <- ""
  }
  locs <- data.frame(country = countries, subnational_region = subnationals, stringsAsFactors = FALSE)
  # Ensure NA subnational_region is converted to empty string for join compatibility
  locs$subnational_region[is.na(locs$subnational_region)] <- ""

  # Defensive: Check geo_lookup exists and is valid
  if (!exists("geo_lookup") || !is.data.frame(geo_lookup)) {
    stop("geo_lookup.rds not loaded or invalid. Please check data/geo_lookup.rds.")
  }
  geo_lookup$country <- trimws(as.character(geo_lookup$country))
  geo_lookup$subnational_region[is.na(geo_lookup$subnational_region) | tolower(geo_lookup$subnational_region) %in% c("na", "", " ")] <- ""
  geo_lookup$subnational_region <- trimws(geo_lookup$subnational_region)

  # Special case: European Union always gets default coordinate
  eu_idx <- which(locs$country == "European Union")
  eu_coords <- NULL
  if (length(eu_idx) > 0) {
    eu_coords <- data.frame(
      country = rep("European Union", length(eu_idx)),
      subnational_region = locs$subnational_region[eu_idx],
      lat = rep(54.5260, length(eu_idx)),
      lng = rep(15.2551, length(eu_idx)),
      label = rep("European Union", length(eu_idx)),
      stringsAsFactors = FALSE
    )
    locs_no_eu <- locs[-eu_idx, , drop = FALSE]
  } else {
    locs_no_eu <- locs
  }

  # Join all pairs to geo_lookup
  joined <- suppressWarnings(dplyr::left_join(locs_no_eu, geo_lookup, by = c("country", "subnational_region")))

  # Fallback: If no subnational match, try country-only (subnational_region == "")
  for (i in seq_len(nrow(joined))) {
    if ((is.na(joined$lat[i]) || is.na(joined$lng[i])) && !is.na(joined$country[i])) {
      match <- geo_lookup[geo_lookup$country == joined$country[i] & geo_lookup$subnational_region == "", ]
      if (nrow(match) >= 1) {
        joined$lat[i] <- match$lat[1]
        joined$lng[i] <- match$lng[1]
      }
    }
  }

  # Add back EU rows
  if (!is.null(eu_coords)) {
    joined <- dplyr::bind_rows(joined, eu_coords)
  }

  # Step 3: Remove invalid combos (not in geo_lookup)
  valid_lookup <- geo_lookup[, c("country", "subnational_region")]
  joined <- dplyr::semi_join(joined, valid_lookup, by = c("country", "subnational_region"))

  # Step 4: Standardize names for output
  joined$country <- trimws(as.character(joined$country))
  joined$subnational_region[is.na(joined$subnational_region) | tolower(joined$subnational_region) %in% c("na", "", " ")] <- ""
  joined$subnational_region <- trimws(joined$subnational_region)
  joined$label <- ifelse(!is.na(joined$subnational_region) & joined$subnational_region != "", paste(joined$subnational_region, joined$country, sep = ", "), joined$country)
  joined <- joined[, c("lat", "lng", "country", "subnational_region", "label")]

  # Diagnostics: Print unmatched pairs and NA values
  unmatched <- dplyr::anti_join(locs, geo_lookup[, c("country", "subnational_region")], by = c("country", "subnational_region"))
  if (nrow(unmatched) > 0) {
    message("Unmatched (country, subnational_region) pairs after join:")
    print(unique(unmatched))
  }
  na_rows <- unmatched[is.na(unmatched$country) | is.na(unmatched$subnational_region), ]
  if (nrow(na_rows) > 0) {
    message("NA values in unmatched pairs:")
    print(na_rows)
  }

  # If join failed completely or row count is wrong, forcibly rebuild output to match input
  if (nrow(joined) != length(countries)) {
    joined <- data.frame(
      lat = rep(NA_real_, length(countries)),
      lng = rep(NA_real_, length(countries)),
      country = countries,
      subnational_region = subnationals,
      label = ifelse(!is.na(subnationals) & subnationals != "", paste(subnationals, countries, sep = ", "), countries),
      stringsAsFactors = FALSE
    )
    message("Coordinate join failed or incomplete. Returning NA rows for unmatched locations.")
  }
  joined
}
