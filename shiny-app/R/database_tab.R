# ============================================================================
# database_tab.R - Database Tab UI
# ============================================================================
# Purpose: Unified database browser with inline study detail panel
# Dependencies: DT, shinycssloaders, bslib, shinyjs (loaded in global.R)
# ============================================================================

databaseTabUI <- function() {
  tagList(
    # ---- Header: download button ----
    div(
      style = "text-align: right; margin-bottom: 0.5rem;",
      downloadButton("download_extended_db", "Download CSV", class = "btn-sm")
    ),
    # ---- Main content: table (+ optional detail panel) ----
    uiOutput("db_main_content")
  )
}
