# ============================================================================
# explore_tab_server.R - Explore Tab Server Logic
# ============================================================================
# Purpose: Server-side logic for study overview, co-occurrence, and temporal
#          trend visualizations. Risk-specific logic is in risks_tab_server.R.
# Dependencies: plotly, dplyr, bslib (loaded in global.R)
#               explore_data loaded in global.R
# ============================================================================

# Color palettes (matching the analysis documents)
offset_colors <- c("biodiversity" = "#B19CD9", "carbon" = "#F0C05A")

domain_colors <- c(
  "non-physical" = "#4E79A7",
  "physical"     = "#59A14F",
  "methodological" = "#E15759"
)

global_risk_colors <- c(
  "Climate and Environmental Disturbances" = "#a1d99b",
  "Direct Anthropogenic Disturbances" = "#31a354",
  "Compliance, Legal, and Governance Risks" = "#c6dbef",
  "Data, Transparency, and Capacity Issues" = "#9ecae1",
  "Financial Risks" = "#6baed6",
  "Political Risks" = "#3182bd",
  "Socioeconomic and Equity Risks" = "#08519c",
  "Ecological Design and Implementation Failures" = "#fc9272",
  "Misaligned Metrics, Standards, and Performance Criteria" = "#ef3b2c",
  "Systemic Oversights and Risk Management Gaps" = "#99000d"
)

exploreTabServer <- function(input, output, session) {
  if (is.null(explore_data)) return()

  # ---- Helper: filter by offset type ----
  filter_offset <- function(df, col = "offset_category_general") {
    ot <- input$explore_offset_type
    if (is.null(ot) || ot == "All") return(df)
    df[df[[col]] == ot, , drop = FALSE]
  }

  # ---- Helper: filter by risk domain ----
  filter_domain <- function(df, col = "permanence_risk_domain") {
    rd <- input$explore_risk_domain
    if (is.null(rd) || length(rd) == 0) return(df[FALSE, , drop = FALSE])
    df[df[[col]] %in% rd, , drop = FALSE]
  }

  # ---- Plotly layout defaults ----
  plotly_layout <- function(p, ...) {
    p |> layout(
      font = list(family = "system-ui, sans-serif"),
      margin = list(l = 10, r = 10, t = 30, b = 50),
      legend = list(orientation = "h", y = -0.3, x = 0.5, xanchor = "center"),
      ...
    )
  }

  # ==========================================================================
  # Card 1: Study Overview
  # ==========================================================================

  # ---- Timeline ----
  output$plot_timeline <- renderPlotly({
    d <- filter_offset(explore_data$timeline)
    if (nrow(d) == 0) return(plotly_empty())
    p <- plot_ly(d, x = ~study_publication_year, y = ~n_studies,
                 color = ~offset_category_general,
                 colors = offset_colors,
                 type = "bar",
                 hovertemplate = "%{x}: %{y} studies<extra>%{fullData.name}</extra>") |>
      plotly_layout(
        barmode = "stack",
        xaxis = list(title = "Year", dtick = 5),
        yaxis = list(title = "Number of Studies")
      )
    p
  })

  # ---- Evidence Types ----
  output$plot_evidence <- renderPlotly({
    d <- filter_offset(explore_data$evidence)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, y = ~study_evidence_type, x = ~n,
            color = ~offset_category_general,
            colors = offset_colors,
            type = "bar", orientation = "h",
            hovertemplate = "%{y}: %{x} studies<extra>%{fullData.name}</extra>") |>
      plotly_layout(
        barmode = "group",
        xaxis = list(title = "Number of Studies"),
        yaxis = list(title = "", categoryorder = "total ascending")
      )
  })

  # ---- Ecosystems ----
  output$plot_ecosystem <- renderPlotly({
    d <- filter_offset(explore_data$ecosystem)
    if (nrow(d) == 0) return(plotly_empty())
    # Top 7
    totals <- d |> dplyr::group_by(ecosystem_broad_type) |>
      dplyr::summarise(total = sum(n), .groups = "drop") |>
      dplyr::slice_max(total, n = 7)
    d <- d |> dplyr::filter(ecosystem_broad_type %in% totals$ecosystem_broad_type)
    plot_ly(d, y = ~ecosystem_broad_type, x = ~n,
            color = ~offset_category_general,
            colors = offset_colors,
            type = "bar", orientation = "h",
            hovertemplate = "%{y}: %{x} studies<extra>%{fullData.name}</extra>") |>
      plotly_layout(
        barmode = "group",
        xaxis = list(title = "Studies"),
        yaxis = list(title = "", categoryorder = "total ascending")
      )
  })

  # ---- Countries ----
  output$plot_country <- renderPlotly({
    d <- filter_offset(explore_data$country)
    if (nrow(d) == 0) return(plotly_empty())
    totals <- d |> dplyr::group_by(country) |>
      dplyr::summarise(total = sum(n), .groups = "drop") |>
      dplyr::slice_max(total, n = 7)
    d <- d |> dplyr::filter(country %in% totals$country)
    plot_ly(d, y = ~country, x = ~n,
            color = ~offset_category_general,
            colors = offset_colors,
            type = "bar", orientation = "h",
            hovertemplate = "%{y}: %{x} studies<extra>%{fullData.name}</extra>") |>
      plotly_layout(
        barmode = "group",
        xaxis = list(title = "Studies"),
        yaxis = list(title = "", categoryorder = "total ascending")
      )
  })

  # ---- Programs ----
  output$plot_program <- renderPlotly({
    d <- filter_offset(explore_data$program)
    if (nrow(d) == 0) return(plotly_empty())
    totals <- d |> dplyr::group_by(program_name) |>
      dplyr::summarise(total = sum(n), .groups = "drop") |>
      dplyr::slice_max(total, n = 7)
    d <- d |> dplyr::filter(program_name %in% totals$program_name)
    plot_ly(d, y = ~program_name, x = ~n,
            color = ~offset_category_general,
            colors = offset_colors,
            type = "bar", orientation = "h",
            hovertemplate = "%{y}: %{x} studies<extra>%{fullData.name}</extra>") |>
      plotly_layout(
        barmode = "group",
        xaxis = list(title = "Studies"),
        yaxis = list(title = "", categoryorder = "total ascending")
      )
  })

  # ==========================================================================
  # Card 2: Risk Prevalence (diverging bar chart)
  # ==========================================================================
  output$plot_risk_prevalence <- renderPlotly({
    d <- explore_data$risk_prevalence
    if (nrow(d) == 0) return(plotly_empty())

    # Pivot to wide for diverging layout
    d_wide <- d |>
      dplyr::select(permanence_risk_domain, permanence_risk_category,
                     permanence_risk_type, offset_category_general, pct) |>
      tidyr::pivot_wider(names_from = offset_category_general,
                         values_from = pct, values_fill = 0)

    # Ensure both columns exist
    if (!"biodiversity" %in% names(d_wide)) d_wide$biodiversity <- 0
    if (!"carbon" %in% names(d_wide)) d_wide$carbon <- 0

    # Order by domain then category
    d_wide <- d_wide |>
      dplyr::arrange(permanence_risk_domain, permanence_risk_category,
                      permanence_risk_type)

    # Create diverging data
    risk_labels <- d_wide$permanence_risk_type
    bio_vals <- -d_wide$biodiversity
    carbon_vals <- d_wide$carbon

    max_val <- max(abs(c(bio_vals, carbon_vals)), na.rm = TRUE) * 1.15

    p <- plot_ly() |>
      add_bars(y = risk_labels, x = bio_vals, name = "Biodiversity",
               marker = list(color = offset_colors["biodiversity"]),
               orientation = "h",
               hovertemplate = "%{y}: %{customdata}%<extra>Biodiversity</extra>",
               customdata = abs(bio_vals)) |>
      add_bars(y = risk_labels, x = carbon_vals, name = "Carbon",
               marker = list(color = offset_colors["carbon"]),
               orientation = "h",
               hovertemplate = "%{y}: %{x}%<extra>Carbon</extra>") |>
      plotly_layout(
        barmode = "relative",
        xaxis = list(
          title = "% of studies",
          range = c(-max_val, max_val),
          tickvals = seq(-100, 100, 20),
          ticktext = abs(seq(-100, 100, 20))
        ),
        yaxis = list(title = "", categoryorder = "array",
                     categoryarray = rev(risk_labels)),
        margin = list(l = 250)
      )
    p
  })

  # ==========================================================================
  # Card 3: Risk Co-occurrence
  # ==========================================================================
  output$plot_cooccurrence <- renderPlotly({
    d <- explore_data$cooccurrence
    ot <- input$explore_offset_type
    if (!is.null(ot) && ot != "All") {
      d <- d |> dplyr::filter(offset_category_general == ot)
    }
    if (nrow(d) == 0) return(plotly_empty())

    # Create readable pair labels
    d <- d |>
      dplyr::mutate(
        pair = paste0(permanence_risk_type_1, " + ", permanence_risk_type_2)
      ) |>
      dplyr::arrange(dplyr::desc(n_studies))

    plot_ly(d, y = ~reorder(pair, n_studies), x = ~n_studies,
            color = ~offset_category_general,
            colors = offset_colors,
            type = "bar", orientation = "h",
            hovertemplate = "%{y}: %{x} studies<extra>%{fullData.name}</extra>") |>
      plotly_layout(
        barmode = "group",
        xaxis = list(title = "Number of Studies"),
        yaxis = list(title = ""),
        margin = list(l = 350)
      )
  })

  # ==========================================================================
  # Card 4: Temporal Trends
  # ==========================================================================
  output$plot_temporal <- renderPlotly({
    view <- input$temporal_view
    if (is.null(view) || view == "domain") {
      # Risk domain view
      d <- explore_data$temporal_domain
      rd <- input$explore_risk_domain
      if (!is.null(rd) && length(rd) > 0) {
        d <- d |> dplyr::filter(permanence_risk_domain %in% rd)
      }
      if (nrow(d) == 0) return(plotly_empty())
      plot_ly(d, x = ~period, y = ~n_studies,
              color = ~permanence_risk_domain,
              colors = domain_colors,
              type = "bar",
              hovertemplate = "%{x}: %{y} studies<extra>%{fullData.name}</extra>") |>
        plotly_layout(
          barmode = "group",
          xaxis = list(title = "Period"),
          yaxis = list(title = "Number of Studies")
        )
    } else {
      # Program view (stacked by risk category)
      d <- explore_data$temporal_program
      totals <- explore_data$temporal_program_totals
      # Get top 2 programs by total studies
      top_programs <- totals |>
        dplyr::group_by(program_group) |>
        dplyr::summarise(total = sum(total_studies), .groups = "drop") |>
        dplyr::slice_max(total, n = 2)
      d <- d |> dplyr::filter(program_group %in% top_programs$program_group)
      totals <- totals |> dplyr::filter(program_group %in% top_programs$program_group)
      if (nrow(d) == 0) return(plotly_empty())

      # Compute percentages
      d <- d |>
        dplyr::left_join(totals, by = c("program_group", "period")) |>
        dplyr::mutate(pct = round((weighted_sum / total_studies) * 100, 1))

      # Use subplot for faceting by program
      programs <- unique(d$program_group)
      plots <- lapply(programs, function(prog) {
        dd <- d |> dplyr::filter(program_group == prog)
        plot_ly(dd, x = ~period, y = ~pct,
                color = ~permanence_risk_category,
                colors = global_risk_colors,
                type = "bar",
                legendgroup = ~permanence_risk_category,
                showlegend = (prog == programs[1]),
                hovertemplate = paste0(
                  "%{x}<br>%{fullData.name}: %{y}%<extra>", prog, "</extra>"
                )) |>
          layout(
            barmode = "stack",
            xaxis = list(title = if (prog == programs[length(programs)]) "Period" else ""),
            yaxis = list(title = "% of studies", range = c(0, 110)),
            annotations = list(
              list(text = prog, x = 0.5, y = 1.05, xref = "paper",
                   yref = "paper", showarrow = FALSE,
                   font = list(size = 13, weight = "bold"))
            )
          )
      })
      subplot(plots, nrows = length(programs), shareX = TRUE, shareY = TRUE,
              titleY = TRUE) |>
        plotly_layout()
    }
  })

}
