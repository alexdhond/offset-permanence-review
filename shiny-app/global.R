# ============================================================================
# global.R - Global Configuration and Data Loading
# ============================================================================
# Purpose: Load all packages and shared data once at app startup
# This file is sourced before ui.R and server.R
# ============================================================================

# ---- Packages ----
library(shiny)
library(shinyjs)
library(shinycssloaders)  # For loading spinners
library(bslib)            # Modern UI framework
library(plotly)           # Interactive charts
library(DT)
library(leaflet)
library(dplyr)
library(magrittr)
library(htmltools)

# ---- App-wide data (loaded once, RDS only for fast startup) ----

# Core database (used by database_helpers.R for joining extra columns)
excel_data <- readRDS("data/offset_perm_rev_database.rds")

# Typology for risk classification (used by explore + database tabs)
typology <- readRDS("data/permanence_risk_typology_full_descriptions.rds")

# Geographic lookup table (used by database tab mini-map via geo_helpers.R)
geo_lookup <- readRDS("data/geo_lookup.rds")

# Precomputed summary statistics (used by about tab value boxes)
db_summary_stats <- tryCatch({
  readRDS("data/db_summary_stats.rds")
}, error = function(e) NULL)

# Precomputed chart data for Explore tab
explore_data <- tryCatch({
  readRDS("data/explore_data.rds")
}, error = function(e) NULL)

# Precomputed map locations (study -> lat/lng with offset type)
map_locations <- tryCatch({
  readRDS("data/map_locations.rds")
}, error = function(e) NULL)

# ---- Data timestamp for "last updated" display ----
data_last_updated <- file.info("data/offset_perm_rev_database.rds")$mtime
