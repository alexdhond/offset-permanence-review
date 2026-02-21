# ============================================================================
# map_tab_server.R - Server Logic for Map Tab
# ============================================================================
# Purpose: Render interactive map with colored markers, clustering, and popups
# Dependencies: leaflet (loaded in global.R)
#               map_locations precomputed in global.R
#               geo_helpers.R for database tab mini-map only
# ============================================================================

# Note: geo_helpers.R is auto-sourced by Shiny from R/ directory

# Marker colors matching offset_colors from explore_tab_server.R
map_marker_colors <- list(
  biodiversity = "#B19CD9",
  carbon       = "#F0C05A"
)

mapTabServer <- function(input, output, session, data) {

  # ---- Populate country filter from precomputed locations ----
  observe({
    req(map_locations)
    countries <- sort(unique(map_locations$country_clean))
    updateSelectInput(session, "map_country_filter",
      choices = c("All", countries),
      selected = isolate(input$map_country_filter) %||% "All"
    )
  })

  # ---- Filtered locations reactive ----
  filtered_map_data <- reactive({
    req(map_locations)
    d <- map_locations

    # Country filter
    sel_country <- input$map_country_filter
    if (!is.null(sel_country) && sel_country != "All") {
      d <- d[d$country_clean == sel_country, , drop = FALSE]
    }

    # Offset type filter
    sel_offset <- input$map_offset_type
    if (!is.null(sel_offset) && sel_offset != "All") {
      d <- d[d$offset_category_general == sel_offset, , drop = FALSE]
    }

    d
  })

  # ---- Build popup HTML ----
  build_popup <- function(d) {
    badge_color <- ifelse(d$offset_category_general == "carbon", "#F0C05A", "#B19CD9")
    badge_text <- ifelse(d$offset_category_general == "carbon", "Carbon", "Biodiversity")

    location_label <- ifelse(
      !is.na(d$subnational_region) & nzchar(d$subnational_region),
      paste0(d$subnational_region, ", ", d$country_clean),
      d$country_clean
    )

    sprintf(
      paste0(
        '<div style="min-width:220px; max-width:300px; font-family:system-ui,sans-serif;">',
          '<div style="font-weight:600; font-size:0.95em; margin-bottom:4px;">%s</div>',
          '<div style="margin-bottom:6px;">',
            '<span style="background:%s; color:#333; padding:1px 8px; border-radius:10px; ',
                   'font-size:0.8em; font-weight:500;">%s</span>',
            '<span style="color:#666; font-size:0.85em; margin-left:6px;">%s</span>',
          '</div>',
          '<div style="color:#555; font-size:0.85em; margin-bottom:6px;">%s</div>',
          '<button class="go-to-study-btn btn btn-sm btn-outline-primary" ',
                  'data-title="%s" style="font-size:0.8em;">',
            'View in Database',
          '</button>',
        '</div>'
      ),
      htmltools::htmlEscape(d$study_title),
      badge_color,
      badge_text,
      htmltools::htmlEscape(as.character(d$study_publication_year)),
      htmltools::htmlEscape(location_label),
      htmltools::htmlEscape(d$study_title)
    )
  }

  # ---- Render map ----
  output$study_map <- renderLeaflet({
    d <- filtered_map_data()

    if (is.null(d) || nrow(d) == 0) {
      return(
        leaflet::leaflet() |>
          leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
          leaflet::setView(lng = 0, lat = 20, zoom = 2)
      )
    }

    # Assign marker colors based on offset type
    d$marker_color <- ifelse(
      d$offset_category_general == "carbon",
      map_marker_colors$carbon,
      map_marker_colors$biodiversity
    )

    # Build popups
    popups <- build_popup(d)

    # Compute view bounds
    lat_range <- range(d$lat, na.rm = TRUE)
    lng_range <- range(d$lng, na.rm = TRUE)
    center_lat <- mean(lat_range)
    center_lng <- mean(lng_range)
    span <- max(diff(lat_range), diff(lng_range))
    zoom_level <- if (span < 5) 6 else if (span < 15) 4 else if (span < 50) 3 else 2

    leaflet::leaflet(d) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::setView(lng = center_lng, lat = center_lat, zoom = zoom_level) |>
      leaflet::addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        label = ~study_title,
        popup = popups,
        radius = 7,
        color = ~marker_color,
        fillColor = ~marker_color,
        fillOpacity = 0.75,
        opacity = 0.9,
        weight = 1,
        clusterOptions = leaflet::markerClusterOptions(
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          showCoverageOnHover = TRUE,
          maxClusterRadius = 45
        )
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors = c(map_marker_colors$biodiversity, map_marker_colors$carbon),
        labels = c("Biodiversity", "Carbon"),
        title = "Offset Type",
        opacity = 0.9
      )
  })
}
