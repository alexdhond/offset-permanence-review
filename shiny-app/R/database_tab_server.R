# ============================================================================
# database_tab_server.R - Database Tab Server Logic
# ============================================================================
# Purpose: Server-side rendering for the unified database browser with
#          inline study detail panel (merged Database + Studies tabs)
#
# Key Functions:
#   - databaseTabServer(): Main server function
#
# Dependencies:
#   - database_helpers.R (studies_data, studies_data_display, studies_data_download)
#   - geo_helpers.R (sourced by map_tab_server.R, provides normalize_country,
#     get_study_coordinates_all, get_country_bbox)
#   - study_summary_ui.R (studySummaryUI function)
#   - DT, digest, leaflet packages
# ============================================================================

databaseTabServer <- function(input, output, session, studies_data_display, studies_data_download, studies_data_full) {

  # ---- Reactive: selected study index ----
  selected_row <- reactiveVal(NULL)

  observeEvent(input$table_rows_selected, {
    sel <- input$table_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      selected_row(sel)
    }
  })

  observeEvent(input$close_detail, {
    selected_row(NULL)
    DT::dataTableProxy("table") %>% DT::selectRows(NULL)
  })

  # ---- Main layout: switches between full-width table and split view ----
  output$db_main_content <- renderUI({
    sel <- selected_row()
    if (is.null(sel)) {
      # Full-width table view
      withSpinner(
        DT::dataTableOutput("table"),
        type = 4, color = "#3c8dbc"
      )
    } else {
      # Split view: table left, detail right
      # Use flexbox with align-items: flex-start to prevent the detail
      # card from stretching to match the tall DT table height
      div(
        style = "display: flex; gap: 1rem; align-items: flex-start;",
        div(
          style = "flex: 0 0 42%; overflow-x: auto;",
          DT::dataTableOutput("table")
        ),
        div(
          style = "flex: 0 0 56%; position: sticky; top: 70px;",
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span("Study Detail"),
              actionButton("close_detail", label = NULL, icon = icon("xmark"),
                           class = "btn-sm btn-outline-secondary")
            ),
            card_body(
              style = "overflow-y: auto; max-height: 80vh;",
              uiOutput("study_detail_panel")
            )
          )
        )
      )
    }
  })

  # ---- Database table ----
  output$table <- DT::renderDataTable({
    df <- studies_data_display
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(), options = list(pageLength = 10)))
    }
    DT::datatable(
      df,
      escape = FALSE,
      selection = "single",
      filter = "top",
      colnames = c(
        "Title" = "study_title",
        "Year" = "study_publication_year",
        "Offset Type" = "offset_category_general",
        "Evidence Type" = "study_evidence_type",
        "Country" = "country",
        "Region" = "subnational_region",
        "Ecosystem" = "ecosystem_broad_type",
        "Species Group" = "species_taxonomic_group",
        "Project Type" = "project_type",
        "Program" = "program_name",
        "Policy" = "policy_name",
        "Risk Domain" = "permanence_risk_domain",
        "Risk Category" = "permanence_risk_category",
        "Risk Type" = "permanence_risk_type",
        "DOI" = "doi",
        "Reference" = "reference"
      ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          # Truncate long columns for display
          list(
            targets = c(0, 3, 7, 8, 9, 10, 13, 14, 15),
            render = DT::JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display' && data !== null && data.length > 40) {",
              "    return '<span title=\"' + data + '\">' + data.substr(0, 37) + '...</span>';",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      )
    )
  })

  # ---- Study detail panel rendering ----
  # Helper to expand country/subnational pairs for a study
  expand_study_locations <- function(study) {
    split_semi <- function(x) {
      x <- as.character(x)
      if (length(x) == 0 || is.na(x) || x == "") return("")
      unlist(strsplit(x, ";"))
    }
    countries <- trimws(split_semi(study$country))
    countries <- normalize_country(countries)
    if ("subnational_region" %in% names(study)) {
      subnationals <- trimws(split_semi(study$subnational_region))
      if (length(subnationals) != length(countries)) {
        if (length(subnationals) == 1) {
          subnationals <- rep(subnationals, length(countries))
        } else if (length(countries) == 1) {
          countries <- rep(countries, length(subnationals))
        } else {
          n <- max(length(countries), length(subnationals))
          length(countries) <- n
          length(subnationals) <- n
        }
      }
    } else {
      subnationals <- rep(NA, length(countries))
    }
    list(countries = countries, subnationals = subnationals)
  }

  output$study_detail_panel <- renderUI({
    sel <- selected_row()
    if (is.null(sel)) return(NULL)
    study <- studies_data_full[sel, ]
    expanded <- expand_study_locations(study)
    coords_all <- get_study_coordinates_all(expanded$countries, expanded$subnationals)
    study$no_geo <- nrow(coords_all) == 0
    source("R/study_summary_ui.R", local = TRUE)
    studySummaryUI(study, typology, coords_all = coords_all)
  })

  # ---- Mini Leaflet map for study detail ----
  output$study_geo_map <- leaflet::renderLeaflet({
    sel <- selected_row()
    empty_map <- leaflet::leaflet(options = leaflet::leafletOptions(
      zoomControl = FALSE, dragging = FALSE, doubleClickZoom = FALSE,
      scrollWheelZoom = FALSE, boxZoom = FALSE, keyboard = FALSE
    )) |> leaflet::addTiles()

    if (is.null(sel)) return(empty_map)

    study <- studies_data_full[sel, ]
    expanded <- expand_study_locations(study)
    coords_all <- get_study_coordinates_all(expanded$countries, expanded$subnationals)

    if (nrow(coords_all) == 0) return(empty_map)

    m <- leaflet::leaflet(options = leaflet::leafletOptions(
      zoomControl = FALSE, dragging = FALSE, doubleClickZoom = FALSE,
      scrollWheelZoom = FALSE, boxZoom = FALSE, keyboard = FALSE
    )) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        data = coords_all, lng = ~lng, lat = ~lat,
        radius = 8, color = "red", fillOpacity = 0.8, label = ~label
      )

    # Fit map bounds based on location data
    if (all(is.na(expanded$subnationals) | expanded$subnationals == "")) {
      if (length(expanded$countries) == 1) {
        bbox <- get_country_bbox(expanded$countries[1])
        if (!is.null(bbox)) {
          bbox_list <- as.list(bbox)
          m <- m |> leaflet::fitBounds(
            lng1 = bbox_list$min_lng, lat1 = bbox_list$min_lat,
            lng2 = bbox_list$max_lng, lat2 = bbox_list$max_lat
          )
        } else {
          m <- m |> leaflet::setView(lng = coords_all$lng[1], lat = coords_all$lat[1], zoom = 4)
        }
      } else if (length(expanded$countries) > 1) {
        bboxes <- lapply(expanded$countries, get_country_bbox)
        bboxes <- bboxes[!sapply(bboxes, is.null)]
        if (length(bboxes) > 0) {
          m <- m |> leaflet::fitBounds(
            lng1 = min(sapply(bboxes, function(b) as.list(b)$min_lng)),
            lat1 = min(sapply(bboxes, function(b) as.list(b)$min_lat)),
            lng2 = max(sapply(bboxes, function(b) as.list(b)$max_lng)),
            lat2 = max(sapply(bboxes, function(b) as.list(b)$max_lat))
          )
        } else {
          m <- m |> leaflet::fitBounds(
            lng1 = min(coords_all$lng), lat1 = min(coords_all$lat),
            lng2 = max(coords_all$lng), lat2 = max(coords_all$lat)
          )
        }
      }
    } else if (nrow(coords_all) > 1) {
      m <- m |> leaflet::fitBounds(
        lng1 = min(coords_all$lng), lat1 = min(coords_all$lat),
        lng2 = max(coords_all$lng), lat2 = max(coords_all$lat)
      )
    } else {
      m <- m |> leaflet::setView(lng = coords_all$lng[1], lat = coords_all$lat[1], zoom = 6)
    }
    m
  })

  # ---- Download handler ----
  output$download_extended_db <- downloadHandler(
    filename = function() {
      paste0("offset_permanence_extended_database_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(studies_data_download, file, row.names = FALSE, na = "")
    }
  )
}
