# ============================================================================
# about_tab.R - About Tab UI
# ============================================================================
# Purpose: Clean landing page with key stats, brief intro, navigation cards,
#          and a compact footer
# Dependencies: bslib (loaded in global.R)
# ============================================================================

aboutTabUI <- function() {
  tagList(
    # ---- Key Stats Row ----
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Studies Reviewed",
        value = if (!is.null(db_summary_stats)) db_summary_stats$n_studies else "---",
        showcase = icon("book-open"),
        theme = "primary"
      ),
      value_box(
        title = "Countries Covered",
        value = if (!is.null(db_summary_stats)) db_summary_stats$n_countries else "---",
        showcase = icon("globe"),
        theme = "success"
      ),
      value_box(
        title = "Risk Types Identified",
        value = if (!is.null(db_summary_stats)) {
          if (!is.null(db_summary_stats$n_risk_types)) db_summary_stats$n_risk_types else nrow(typology)
        } else nrow(typology),
        showcase = icon("triangle-exclamation"),
        theme = "warning"
      ),
      value_box(
        title = "Publication Span",
        value = if (!is.null(db_summary_stats) && !is.null(db_summary_stats$year_range)) {
          paste(db_summary_stats$year_range, collapse = "\u2013")
        } else "---",
        showcase = icon("calendar"),
        theme = "info"
      )
    ),

    # ---- Brief Intro ----
    div(
      style = "max-width: 800px; margin: 1.5rem auto 1rem auto; text-align: center;",
      p(
        style = "font-size: 1.05rem; color: #555; line-height: 1.6;",
        "This interactive application accompanies the paper",
        em("Permanence Risks to Biodiversity and Nature-based Carbon Offsets."),
        "Explore a systematically collated database of peer-reviewed studies
         examining risks to the long-term persistence of biodiversity and
         nature-based carbon offsets."
      )
    ),

    # ---- Navigation Cards ----
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      card(
        class = "text-center",
        card_body(
          style = "cursor: pointer; padding: 1.5rem;",
          onclick = "Shiny.setInputValue('go_to_tab', 'Explore', {priority: 'event'})",
          div(icon("chart-bar", class = "fa-2x text-primary"), style = "margin-bottom: 0.75rem;"),
          h5("Explore", style = "margin-bottom: 0.5rem;"),
          p(class = "text-muted mb-0",
            "Interactive visualisations of study patterns and temporal trends")
        )
      ),
      card(
        class = "text-center",
        card_body(
          style = "cursor: pointer; padding: 1.5rem;",
          onclick = "Shiny.setInputValue('go_to_tab', 'Risks', {priority: 'event'})",
          div(icon("triangle-exclamation", class = "fa-2x text-warning"), style = "margin-bottom: 0.75rem;"),
          h5("Risks", style = "margin-bottom: 0.5rem;"),
          p(class = "text-muted mb-0",
            "Browse the risk typology, prevalence data, and management strategies")
        )
      ),
      card(
        class = "text-center",
        card_body(
          style = "cursor: pointer; padding: 1.5rem;",
          onclick = "Shiny.setInputValue('go_to_tab', 'Database', {priority: 'event'})",
          div(icon("database", class = "fa-2x text-success"), style = "margin-bottom: 0.75rem;"),
          h5("Database", style = "margin-bottom: 0.5rem;"),
          p(class = "text-muted mb-0",
            "Filter, search, and download the full dataset; click any study for detailed information")
        )
      ),
      card(
        class = "text-center",
        card_body(
          style = "cursor: pointer; padding: 1.5rem;",
          onclick = "Shiny.setInputValue('go_to_tab', 'Map', {priority: 'event'})",
          div(icon("map-location-dot", class = "fa-2x text-info"), style = "margin-bottom: 0.75rem;"),
          h5("Map", style = "margin-bottom: 0.5rem;"),
          p(class = "text-muted mb-0",
            "Visualise the spatial distribution of studies with interactive markers")
        )
      )
    ),

    # ---- Compact Footer ----
    div(
      style = "text-align: center; margin-top: 1.5rem; padding-top: 1rem;
               border-top: 1px solid #dee2e6; color: #888; font-size: 0.85rem;",
      p(
        "Developed by Alexander Dhond \u2022 Bull Group, University of Oxford",
        tags$br(),
        tags$a(href = "mailto:alexander.dhond@biology.ox.ac.uk",
               "alexander.dhond@biology.ox.ac.uk"),
        if (exists("data_last_updated") && !is.null(data_last_updated)) {
          paste0(" \u2022 Data last updated: ", format(data_last_updated, "%Y-%m-%d"))
        }
      )
    )
  )
}
