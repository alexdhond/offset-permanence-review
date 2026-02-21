# ============================================================================
# risks_tab.R - Risks Tab UI
# ============================================================================
# Purpose: Dedicated tab for risk prevalence, typology browser, and management
# Dependencies: bslib, plotly, shinycssloaders (loaded in global.R)
# ============================================================================

risksTabUI <- function() {
  accordion(
    id = "risks_accordion",
    open = TRUE,

    # Section 1: Risk Prevalence
    accordion_panel(
      title = "Risk Prevalence",
      icon = icon("chart-column"),
      card(
        card_header("Risk Type Prevalence by Offset Category"),
        card_body(
          p(tags$small("Percentage of studies in each offset category reporting each risk type.")),
          withSpinner(plotlyOutput("plot_risk_prevalence", height = "600px"),
                      type = 4, color = "#3c8dbc")
        )
      )
    ),

    # Section 2: Risk Typology Browser
    accordion_panel(
      title = "Risk Typology Browser",
      icon = icon("sitemap"),
      layout_columns(
        col_widths = c(6, 6),
        div(
          h5("Risk Typology"),
          uiOutput("risk_hierarchy_list")
        ),
        div(
          conditionalPanel(
            condition = "output.riskSummaryVisible",
            card(
              card_header("Summary"),
              card_body(
                style = "overflow-y: auto; max-height: 60vh;",
                uiOutput("risk_summary_panel")
              )
            )
          )
        )
      )
    ),

    # Section 3: Management Framework (placeholder)
    accordion_panel(
      title = "Management Framework",
      icon = icon("clipboard-check"),
      card(
        card_body(
          div(
            style = "text-align: center; padding: 2rem;",
            icon("clipboard-check", class = "fa-3x text-muted"),
            h4("Management & Mitigation Strategies", style = "margin-top: 1rem;"),
            p(
              class = "text-muted",
              "A structured framework for managing and mitigating permanence risks,",
              "linking risk types to evidence-based strategies from the reviewed literature."
            ),
            tags$span(
              class = "badge bg-info",
              style = "font-size: 0.9rem; padding: 0.5em 1em;",
              "Coming soon"
            )
          )
        )
      )
    )
  )
}
