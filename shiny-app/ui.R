# ============================================================================
# ui.R - Main UI Definition
# ============================================================================
# Purpose: Define the app layout using bslib page_navbar with 5 tabs
# Dependencies: bslib, shinyjs (loaded in global.R)
# ============================================================================

# Source all tab UI files
source("R/about_tab.R")
source("R/explore_tab.R")
source("R/risks_tab.R")
source("R/database_tab.R")
source("R/map_tab.R")

ui <- page_navbar(
  title = "Offset Permanence Review",
  id = "main_tabs",
  theme = bs_theme(bootswatch = "flatly"),
  header = tagList(
    shinyjs::useShinyjs(),
    tags$script(src = "custom-handlers.js")
  ),
  nav_panel(
    title = "About",
    icon = icon("circle-info"),
    aboutTabUI()
  ),
  nav_panel(
    title = "Explore",
    icon = icon("chart-bar"),
    exploreTabUI()
  ),
  nav_panel(
    title = "Risks",
    icon = icon("triangle-exclamation"),
    risksTabUI()
  ),
  nav_panel(
    title = "Database",
    icon = icon("database"),
    databaseTabUI()
  ),
  nav_panel(
    title = "Map",
    icon = icon("map-location-dot"),
    mapTabUI()
  )
)
