# ============================================================================
# compute_db_summary_stats.R
# ============================================================================
# Precomputes all summary statistics and chart data for the Shiny app.
# Run this script manually when the long database is updated:
#   source("compute_db_summary_stats.R")
# ============================================================================

library(dplyr)
library(tidyr)

# Load the long-format database
df <- readRDS("data/offset_perm_rev_long_cleaned.rds")

# ---- 1. Basic summary stats ----
years <- as.numeric(unique(df$study_publication_year[
  !is.na(df$study_publication_year) & nzchar(df$study_publication_year)
]))

summary_stats <- list(
  n_studies = length(unique(df$study_title)),
  n_biodiv = length(unique(df$study_title[df$offset_category_general == "biodiversity"])),
  n_carbon = length(unique(df$study_title[df$offset_category_general == "carbon"])),
  n_ecosystems = length(unique(df$ecosystem_broad_type[
    !is.na(df$ecosystem_broad_type) & nzchar(df$ecosystem_broad_type)
  ])),
  n_countries = length(unique(df$country[!is.na(df$country) & nzchar(df$country)])),
  n_project_types = length(unique(df$project_type[
    !is.na(df$project_type) & nzchar(df$project_type)
  ])),
  n_years = length(years),
  n_risk_types = length(unique(df$permanence_risk_type[
    !is.na(df$permanence_risk_type) & nzchar(df$permanence_risk_type)
  ])),
  year_range = c(min(years, na.rm = TRUE), max(years, na.rm = TRUE))
)

# ---- 2. Publication timeline ----
timeline_df <- df |>
  filter(!is.na(study_publication_year), !is.na(offset_category_general)) |>
  distinct(study_id, study_publication_year, offset_category_general) |>
  count(study_publication_year, offset_category_general, name = "n_studies") |>
  mutate(study_publication_year = as.numeric(study_publication_year))

# ---- 3. Evidence types by offset category ----
evidence_name_map <- c(
  "direct empirical" = "Direct Empirical",
  "legal and policy" = "Conceptual/Legal/Policy",
  "review and discussion" = "Reviews/Discussions",
  "modeling" = "Modeling"
)

evidence_df <- df |>
  filter(!is.na(study_evidence_type), !is.na(offset_category_general)) |>
  distinct(study_id, study_evidence_type, offset_category_general) |>
  mutate(study_evidence_type = ifelse(
    study_evidence_type %in% names(evidence_name_map),
    evidence_name_map[study_evidence_type],
    study_evidence_type
  )) |>
  count(study_evidence_type, offset_category_general, name = "n")

# ---- 4. Ecosystem types by offset category (top 8) ----
ecosystem_df <- df |>
  filter(!is.na(ecosystem_broad_type), nzchar(ecosystem_broad_type),
         !is.na(offset_category_general)) |>
  distinct(study_id, ecosystem_broad_type, offset_category_general) |>
  count(ecosystem_broad_type, offset_category_general, name = "n")

# ---- 5. Top countries by offset category ----
country_df <- df |>
  filter(!is.na(country), nzchar(country), !is.na(offset_category_general)) |>
  distinct(study_id, country, offset_category_general) |>
  mutate(country = case_when(
    country == "United States of America" ~ "USA",
    country == "United Kingdom" ~ "UK",
    TRUE ~ country
  )) |>
  count(country, offset_category_general, name = "n")

# ---- 6. Top programs by offset category ----
program_df <- df |>
  filter(!is.na(program_name), nzchar(program_name),
         !is.na(offset_category_general)) |>
  distinct(study_id, program_name, offset_category_general) |>
  count(program_name, offset_category_general, name = "n")

# ---- 7. Risk type prevalence by offset category ----
total_by_offset <- df |>
  filter(!is.na(offset_category_general)) |>
  distinct(study_id, offset_category_general) |>
  count(offset_category_general, name = "total_n")

risk_prevalence_df <- df |>
  filter(!is.na(permanence_risk_domain), !is.na(permanence_risk_category),
         !is.na(permanence_risk_type), !is.na(offset_category_general)) |>
  distinct(study_id, permanence_risk_domain, permanence_risk_category,
           permanence_risk_type, offset_category_general) |>
  count(permanence_risk_domain, permanence_risk_category,
        permanence_risk_type, offset_category_general, name = "n") |>
  left_join(total_by_offset, by = "offset_category_general") |>
  mutate(pct = round((n / total_n) * 100, 1))

# ---- 8. Risk co-occurrence (top pairs) ----
# Get study-risk pairs
study_risks <- df |>
  filter(!is.na(permanence_risk_type), !is.na(offset_category_general)) |>
  distinct(study_id, permanence_risk_type, offset_category_general)

# Self-join to get pairs
cooccur_df <- study_risks |>
  inner_join(study_risks, by = c("study_id", "offset_category_general"),
             suffix = c("_1", "_2"), relationship = "many-to-many") |>
  filter(permanence_risk_type_1 < permanence_risk_type_2) |>
  count(permanence_risk_type_1, permanence_risk_type_2,
        offset_category_general, name = "n_studies") |>
  group_by(offset_category_general) |>
  slice_max(n_studies, n = 10, with_ties = FALSE) |>
  ungroup()

# ---- 9. Temporal trends by risk domain (5-year bins) ----
temporal_domain_df <- df |>
  filter(!is.na(permanence_risk_domain), !is.na(study_publication_year)) |>
  mutate(
    year_num = as.numeric(study_publication_year),
    period = cut(
      year_num,
      breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2026),
      labels = c("1990-1994", "1995-1999", "2000-2004", "2005-2009",
                 "2010-2014", "2015-2019", "2020-2025"),
      right = FALSE
    )
  ) |>
  filter(!is.na(period)) |>
  distinct(study_id, period, permanence_risk_domain) |>
  count(period, permanence_risk_domain, name = "n_studies") |>
  complete(period, permanence_risk_domain, fill = list(n_studies = 0))

# ---- 10. Temporal trends by risk category (for top programs) ----
temporal_program_df <- df |>
  filter(!is.na(permanence_risk_category), !is.na(study_publication_year),
         !is.na(program_name), nzchar(program_name)) |>
  mutate(
    year_num = as.numeric(study_publication_year),
    period = cut(
      year_num,
      breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2026),
      labels = c("1990-1994", "1995-1999", "2000-2004", "2005-2009",
                 "2010-2014", "2015-2019", "2020-2025"),
      right = FALSE
    ),
    program_group = case_when(
      program_name %in% c("US CWA 404 Permitting", "US Mitigation Banking") ~
        "U.S. Compensatory Mitigation",
      TRUE ~ program_name
    )
  ) |>
  filter(!is.na(period)) |>
  distinct(study_id, program_group, period, permanence_risk_category) |>
  group_by(study_id, program_group, period) |>
  mutate(weight = 1 / n()) |>
  ungroup() |>
  group_by(program_group, period, permanence_risk_category) |>
  summarise(weighted_sum = sum(weight), .groups = "drop")

# Total studies per program-period for labels
temporal_program_totals <- df |>
  filter(!is.na(permanence_risk_category), !is.na(study_publication_year),
         !is.na(program_name), nzchar(program_name)) |>
  mutate(
    year_num = as.numeric(study_publication_year),
    period = cut(
      year_num,
      breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2026),
      labels = c("1990-1994", "1995-1999", "2000-2004", "2005-2009",
                 "2010-2014", "2015-2019", "2020-2025"),
      right = FALSE
    ),
    program_group = case_when(
      program_name %in% c("US CWA 404 Permitting", "US Mitigation Banking") ~
        "U.S. Compensatory Mitigation",
      TRUE ~ program_name
    )
  ) |>
  filter(!is.na(period)) |>
  distinct(study_id, program_group, period) |>
  count(program_group, period, name = "total_studies")

# ---- 11. Map locations (precomputed) ----
# Uses valid_country_pairs (already expanded/validated) + geo_lookup for coords
# + condensed_data for offset_category_general and study_publication_year

condensed <- readRDS("data/offset_perm_rev_condensed.rds")
valid_pairs <- readRDS("data/country_clean_valid.rds")
geo_lookup <- readRDS("data/geo_lookup.rds")
aliases <- read.csv("data/country_name_aliases.csv", stringsAsFactors = FALSE)
alias_map <- setNames(aliases$standard_name, aliases$alias)

# Select relevant columns from valid_pairs
map_locs <- valid_pairs |>
  select(study_title, study_id, country, subnational_region)

# Normalize country names to match geo_lookup
map_locs$country_clean <- trimws(map_locs$country)
for (i in seq_len(nrow(map_locs))) {
  cn <- map_locs$country_clean[i]
  if (cn %in% names(alias_map)) {
    map_locs$country_clean[i] <- alias_map[[cn]]
  }
}

# Standardize subnational_region
map_locs$subnational_region <- trimws(as.character(map_locs$subnational_region))
map_locs$subnational_region[is.na(map_locs$subnational_region)] <- ""

# Standardize geo_lookup for join
geo_lookup$country <- trimws(as.character(geo_lookup$country))
geo_lookup$subnational_region <- trimws(as.character(geo_lookup$subnational_region))
geo_lookup$subnational_region[is.na(geo_lookup$subnational_region)] <- ""

# Join with geo_lookup for coordinates (exact match on country + subnational)
map_locs <- map_locs |>
  left_join(geo_lookup, by = c("country_clean" = "country",
                                "subnational_region" = "subnational_region"))

# Fallback: for rows with no coords, try country-level match (subnational_region == "")
missing <- is.na(map_locs$lat) | is.na(map_locs$lng)
if (any(missing)) {
  country_only <- geo_lookup |> filter(subnational_region == "")
  for (i in which(missing)) {
    cn <- map_locs$country_clean[i]
    match <- country_only[country_only$country == cn, ]
    if (nrow(match) > 0) {
      map_locs$lat[i] <- match$lat[1]
      map_locs$lng[i] <- match$lng[1]
    }
  }
}

# EU special case
eu_idx <- which(map_locs$country_clean == "European Union" & is.na(map_locs$lat))
if (length(eu_idx) > 0) {
  map_locs$lat[eu_idx] <- 54.526
  map_locs$lng[eu_idx] <- 15.255
}

# Join offset_category_general and study_publication_year from condensed data
study_meta <- condensed |>
  distinct(study_title, offset_category_general, study_publication_year)
map_locs <- map_locs |>
  left_join(study_meta, by = "study_title")

# Remove rows without coordinates
map_locs <- map_locs |>
  filter(!is.na(lat), !is.na(lng))

# ---- Merge nearby markers from the same study ----
# Haversine distance in km (pure base R)
haversine_km <- function(lat1, lng1, lat2, lng2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlng <- (lng2 - lng1) * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlng / 2)^2
  R * 2 * atan2(sqrt(a), sqrt(1 - a))
}

# Build a location label from country + optional subnational region
make_loc_label <- function(country, region) {
  ifelse(nzchar(region), paste0(region, ", ", country), country)
}

map_locs$loc_label <- make_loc_label(map_locs$country_clean,
                                     map_locs$subnational_region)

# Split by study_title for merging (study_id is not unique — e.g. "alt_source")
study_titles <- unique(map_locs$study_title)
merged_rows <- vector("list", length(study_titles))

for (si in seq_along(study_titles)) {
  rows <- map_locs[map_locs$study_title == study_titles[si], , drop = FALSE]

  if (nrow(rows) == 1) {
    # Single-location study: pass through
    merged_rows[[si]] <- data.frame(
      study_title = rows$study_title[1],
      study_id = rows$study_id[1],
      lat = rows$lat[1],
      lng = rows$lng[1],
      offset_category_general = rows$offset_category_general[1],
      study_publication_year = rows$study_publication_year[1],
      n_locations = 1L,
      location_label = rows$loc_label[1],
      countries_all = rows$country_clean[1],
      stringsAsFactors = FALSE
    )
  } else {
    # Multi-location study: cluster within 500 km
    n <- nrow(rows)
    dist_mat <- matrix(0, n, n)
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        d <- haversine_km(rows$lat[i], rows$lng[i], rows$lat[j], rows$lng[j])
        dist_mat[i, j] <- d
        dist_mat[j, i] <- d
      }
    }
    clusters <- cutree(hclust(as.dist(dist_mat), method = "single"), h = 500)

    cluster_ids <- unique(clusters)
    cluster_rows <- vector("list", length(cluster_ids))
    for (ci in seq_along(cluster_ids)) {
      idx <- which(clusters == cluster_ids[ci])
      cluster_rows[[ci]] <- data.frame(
        study_title = rows$study_title[1],
        study_id = rows$study_id[1],
        lat = mean(rows$lat[idx]),
        lng = mean(rows$lng[idx]),
        offset_category_general = rows$offset_category_general[1],
        study_publication_year = rows$study_publication_year[1],
        n_locations = length(idx),
        location_label = paste(rows$loc_label[idx], collapse = "; "),
        countries_all = paste(unique(rows$country_clean[idx]), collapse = "; "),
        stringsAsFactors = FALSE
      )
    }
    merged_rows[[si]] <- do.call(rbind, cluster_rows)
  }
}

map_locations <- do.call(rbind, merged_rows)
rownames(map_locations) <- NULL

# ---- Save everything ----
explore_data <- list(
  timeline = timeline_df,
  evidence = evidence_df,
  ecosystem = ecosystem_df,
  country = country_df,
  program = program_df,
  risk_prevalence = risk_prevalence_df,
  cooccurrence = cooccur_df,
  temporal_domain = temporal_domain_df,
  temporal_program = temporal_program_df,
  temporal_program_totals = temporal_program_totals
)

saveRDS(summary_stats, file = "data/db_summary_stats.rds")
saveRDS(explore_data, file = "data/explore_data.rds")
saveRDS(map_locations, file = "data/map_locations.rds")

cat("Summary stats saved to data/db_summary_stats.rds\n")
cat("Explore chart data saved to data/explore_data.rds\n")
cat("Map locations saved to data/map_locations.rds\n")
print(summary_stats)
cat("\nExplore data components:\n")
cat(paste(" -", names(explore_data), ":", sapply(explore_data, nrow), "rows\n"))
cat("\nMap locations:", nrow(map_locations), "rows\n")
