# ============================================================================
# database_tab.R - Database Tab UI
# ============================================================================
# Purpose: Unified database browser with inline study detail panel
# Dependencies: DT, shinycssloaders, bslib, shinyjs (loaded in global.R)
# ============================================================================

databaseTabUI <- function() {
  tagList(
    # ---- Header: country filter + download ----
    layout_columns(
      col_widths = c(4, 4, 4),
      uiOutput("country_filter_ui"),
      div(),
      div(
        style = "padding-top: 25px; text-align: right;",
        downloadButton("download_extended_db", "Download CSV", class = "btn-sm")
      )
    ),
    # ---- Main content: table (+ optional detail panel) ----
    uiOutput("db_main_content")
  )
}
