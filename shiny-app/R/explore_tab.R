# ============================================================================
# explore_tab.R - Explore Tab UI
# ============================================================================
# Purpose: Interactive visualizations for study overview, co-occurrence, and
#          temporal trends. Risk-specific content is in risks_tab.R.
# Dependencies: bslib, plotly, shinycssloaders (loaded in global.R)
# ============================================================================

exploreTabUI <- function() {
  layout_sidebar(
    sidebar = sidebar(
      title = "Filters",
      width = 250,
      radioButtons(
        inputId = "explore_offset_type",
        label = "Offset Type",
        choices = c("All", "Biodiversity" = "biodiversity", "Carbon" = "carbon"),
        selected = "All"
      ),
      checkboxGroupInput(
        inputId = "explore_risk_domain",
        label = "Risk Domain",
        choices = c(
          "Physical" = "physical",
          "Non-Physical" = "non-physical",
          "Methodological" = "methodological"
        ),
        selected = c("physical", "non-physical", "methodological")
      )
    ),
    # ---- Main Panel: Collapsible Cards ----
    accordion(
      id = "explore_accordion",
      open = TRUE,

      # Card 1: Study Overview
      accordion_panel(
        title = "Study Overview",
        icon = icon("chart-bar"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Publication Timeline"),
            card_body(
              withSpinner(plotlyOutput("plot_timeline", height = "300px"),
                          type = 4, color = "#3c8dbc")
            )
          ),
          card(
            card_header("Evidence Types"),
            card_body(
              withSpinner(plotlyOutput("plot_evidence", height = "300px"),
                          type = 4, color = "#3c8dbc")
            )
          )
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            card_header("Top Ecosystems"),
            card_body(
              withSpinner(plotlyOutput("plot_ecosystem", height = "300px"),
                          type = 4, color = "#3c8dbc")
            )
          ),
          card(
            card_header("Top Countries"),
            card_body(
              withSpinner(plotlyOutput("plot_country", height = "300px"),
                          type = 4, color = "#3c8dbc")
            )
          ),
          card(
            card_header("Top Programs"),
            card_body(
              withSpinner(plotlyOutput("plot_program", height = "300px"),
                          type = 4, color = "#3c8dbc")
            )
          )
        )
      ),

      # Card 2: Risk Co-occurrence
      accordion_panel(
        title = "Risk Co-occurrence",
        icon = icon("grip"),
        card(
          card_header("Top Risk Type Pairs"),
          card_body(
            p(tags$small("Most frequently co-occurring risk type pairs within studies.")),
            withSpinner(plotlyOutput("plot_cooccurrence", height = "500px"),
                        type = 4, color = "#3c8dbc")
          )
        )
      ),

      # Card 3: Temporal Trends
      accordion_panel(
        title = "Temporal Trends",
        icon = icon("chart-line"),
        radioButtons(
          inputId = "temporal_view",
          label = "View by:",
          choices = c("Risk Domain" = "domain", "Top Programs" = "program"),
          selected = "domain",
          inline = TRUE
        ),
        card(
          card_body(
            withSpinner(plotlyOutput("plot_temporal", height = "450px"),
                        type = 4, color = "#3c8dbc")
          )
        )
      )
    )
  )
}
