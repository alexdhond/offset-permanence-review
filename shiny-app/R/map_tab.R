# ============================================================================
# map_tab.R - Map Tab UI
# ============================================================================
# Purpose: UI definition for the interactive study map
# Dependencies: leaflet, shinycssloaders, bslib (loaded in global.R)
# ============================================================================

mapTabUI <- function() {
  tagList(
    layout_columns(
      col_widths = c(3, 3, -6),
      selectInput(
        inputId = "map_country_filter",
        label = "Filter by country:",
        choices = c("All"),
        selected = "All"
      ),
      radioButtons(
        inputId = "map_offset_type",
        label = "Offset type:",
        choices = c("All", "Biodiversity" = "biodiversity", "Carbon" = "carbon"),
        selected = "All",
        inline = TRUE
      )
    ),
    withSpinner(
      leaflet::leafletOutput("study_map", height = "calc(100vh - 200px)"),
      type = 4,
      color = "#3c8dbc"
    )
  )
}
