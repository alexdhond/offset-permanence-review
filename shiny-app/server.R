# ============================================================================
# server.R - Main Server Logic
# ============================================================================
# Purpose: Core server function coordinating all tab modules and navigation
# Dependencies: database_helpers.R, database_tab_server.R, map_tab_server.R,
#               explore_tab_server.R
# ============================================================================

# Source helpers and tab server files
source("R/database_helpers.R")
source("R/database_tab_server.R")
source("R/map_tab_server.R")
source("R/explore_tab_server.R")
source("R/risks_tab_server.R")

server <- function(input, output, session) {
  # --- Listen for go_to_tab message from JS and update navbar ---
  observeEvent(input$go_to_tab, {
    nav_select("main_tabs", input$go_to_tab, session = session)
  }, ignoreInit = TRUE)

  # --- Handle JS bundled study navigation (select_and_go) ---
  observeEvent(input$select_and_go, {
    req(input$select_and_go$digest)
    digest_val <- input$select_and_go$digest
    tab <- input$select_and_go$tab %||% "Database"
    # Find study by digest in full dataset
    alldigests <- sapply(studies_data$study_title, function(title) digest::digest(trimws(title)))
    idx <- which(alldigests == digest_val)[1]
    nav_select("main_tabs", tab, session = session)
    if (!is.na(idx) && idx > 0) {
      shinyjs::delay(200, {
        DT::dataTableProxy("table") %>% DT::selectRows(idx)
      })
    }
  }, ignoreInit = TRUE)

  # --- Handle JS map marker navigation (go_to_study_from_map) ---
  observeEvent(input$go_to_study_from_map, {
    req(input$go_to_study_from_map$title)
    study_title <- as.character(input$go_to_study_from_map$title)
    tab <- input$go_to_study_from_map$tab %||% "Database"
    idx <- which(studies_data$study_title == study_title)[1]
    nav_select("main_tabs", tab, session = session)
    if (!is.na(idx) && idx > 0) {
      shinyjs::delay(200, {
        DT::dataTableProxy("table") %>% DT::selectRows(idx)
      })
    }
  }, ignoreInit = TRUE)

  # --- Handle JS risk navigation (selected_risk_info) ---
  observeEvent(input$selected_risk_info, {
    req(input$selected_risk_info$domain)
    nav_select("main_tabs", input$selected_risk_info$tab %||% "Risks", session = session)
  }, ignoreInit = TRUE)

  # Handler for study navigation from other tabs
  observeEvent(input$go_to_study, {
    study_title <- as.character(input$go_to_study)
    idx <- which(studies_data$study_title == study_title)[1]
    if (!is.na(idx) && idx > 0) {
      nav_select("main_tabs", "Database", session = session)
      shinyjs::delay(200, {
        DT::dataTableProxy("table") %>% DT::selectRows(idx)
      })
    }
  })

  # Tab-to-tab linking: view on map
  observeEvent(input$view_on_map, {
    nav_select("main_tabs", "Map", session = session)
  })

  # ---- Module Servers ----
  databaseTabServer(input, output, session, studies_data_display, studies_data_download, studies_data)
  mapTabServer(input, output, session, NULL)
  exploreTabServer(input, output, session)
  risksTabServer(input, output, session)
}
