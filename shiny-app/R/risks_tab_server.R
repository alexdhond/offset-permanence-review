# ============================================================================
# risks_tab_server.R - Risks Tab Server Logic
# ============================================================================
# Purpose: Server-side logic for risk prevalence chart, typology browser,
#          and management framework
# Dependencies: plotly, dplyr, bslib (loaded in global.R)
#               explore_data and typology loaded in global.R
# ============================================================================

# Color palettes (matching the analysis documents)
risks_offset_colors <- c("biodiversity" = "#B19CD9", "carbon" = "#F0C05A")

risks_global_risk_colors <- c(
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

# Helper: render study links for risk summary
render_study_links <- function(studies) {
  if (is.null(studies) || nrow(studies) == 0) {
    return(p("No studies found."))
  }
  tagList(
    h5(paste0("Studies (", nrow(studies), "):") ),
    tags$ul(lapply(seq_len(nrow(studies)), function(i) {
      title <- studies$study_title[i]
      tags$li(actionLink(
        inputId = paste0("risk_study_", digest::digest(trimws(title))),
        label = title,
        class = "risk-study-link",
        `data-digest` = digest::digest(trimws(title))
      ))
    }))
  )
}

risksTabServer <- function(input, output, session) {
  if (is.null(explore_data)) return()

  # ---- Plotly layout defaults ----
  plotly_layout <- function(p, ...) {
    p |> layout(
      font = list(family = "system-ui, sans-serif"),
      margin = list(l = 10, r = 10, t = 30, b = 10),
      legend = list(orientation = "h", y = -0.15),
      ...
    )
  }

  # ==========================================================================
  # Section 1: Risk Prevalence (diverging bar chart)
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
    cat_colors <- risks_global_risk_colors[d_wide$permanence_risk_category]

    max_val <- max(abs(c(bio_vals, carbon_vals)), na.rm = TRUE) * 1.15

    p <- plot_ly() |>
      add_bars(y = risk_labels, x = bio_vals, name = "Biodiversity",
               marker = list(color = risks_offset_colors["biodiversity"]),
               orientation = "h",
               hovertemplate = "%{y}: %{customdata}%<extra>Biodiversity</extra>",
               customdata = abs(bio_vals)) |>
      add_bars(y = risk_labels, x = carbon_vals, name = "Carbon",
               marker = list(color = risks_offset_colors["carbon"]),
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
  # Section 2: Risk Typology Browser
  # ==========================================================================
  risk_domains_df <- typology |> dplyr::distinct(risk_domain, risk_domain_description)
  risk_cats_df <- typology |> dplyr::distinct(risk_domain, risk_category, risk_category_description)
  risk_types_df <- typology |> dplyr::distinct(risk_domain, risk_category, risk_type, risk_type_description)

  selected_item <- reactiveVal(list(level = NULL, domain = NULL, category = NULL, type = NULL))

  # Cross-tab navigation: Listen for selected_risk_info from JS
  observeEvent(input$selected_risk_info, {
    info <- input$selected_risk_info
    if (!is.null(info$type) && nzchar(info$type %||% "")) {
      selected_item(list(level = "type", domain = info$domain, category = info$category, type = info$type))
    } else if (!is.null(info$category) && nzchar(info$category %||% "")) {
      selected_item(list(level = "category", domain = info$domain, category = info$category, type = NULL))
    } else if (!is.null(info$domain)) {
      selected_item(list(level = "domain", domain = info$domain, category = NULL, type = NULL))
    }
  }, ignoreInit = TRUE)

  # Render the risk hierarchy
  output$risk_hierarchy_list <- renderUI({
    tags$ul(style = "list-style-type:none; padding-left:0;",
      lapply(seq_len(nrow(risk_domains_df)), function(i) {
        domain <- risk_domains_df$risk_domain[i]
        domain_desc <- risk_domains_df$risk_domain_description[i]
        domain_id <- paste0("risk_domain_", i)
        cats <- risk_cats_df |> dplyr::filter(risk_domain == domain)
        tags$li(
          span(
            icon("layer-group"),
            actionLink(
              inputId = domain_id,
              label = tags$span(domain, style = "color:#1a4fa0; font-size:1.2em; font-weight:bold;"),
              title = domain_desc
            )
          ),
          if (nrow(cats) > 0) tags$ul(style = "list-style-type:none; padding-left:1.5em;",
            lapply(seq_len(nrow(cats)), function(j) {
              cat_name <- cats$risk_category[j]
              cat_desc <- cats$risk_category_description[j]
              cat_id <- paste0("risk_cat_", i, "_", j)
              types <- risk_types_df |> dplyr::filter(risk_domain == domain, risk_category == cat_name)
              tags$li(
                span(
                  icon("sitemap"),
                  actionLink(
                    inputId = cat_id,
                    label = tags$span(cat_name, style = "color:#228B22; font-size:1.05em; font-weight:600;"),
                    title = cat_desc
                  )
                ),
                if (nrow(types) > 0) tags$ul(style = "list-style-type:none; padding-left:1.5em;",
                  lapply(seq_len(nrow(types)), function(k) {
                    type_name <- types$risk_type[k]
                    type_desc <- types$risk_type_description[k]
                    type_id <- paste0("risk_type_", i, "_", j, "_", k)
                    tags$li(
                      span(
                        icon("tag"),
                        actionLink(
                          inputId = type_id,
                          label = tags$span(type_name, style = "color:#d2691e; font-size:0.95em;"),
                          title = type_desc
                        )
                      )
                    )
                  })
                )
              )
            })
          )
        )
      })
    )
  })

  # Register click observers for the hierarchy
  observe({
    lapply(seq_len(nrow(risk_domains_df)), function(i) {
      domain <- risk_domains_df$risk_domain[i]
      observeEvent(input[[paste0("risk_domain_", i)]], {
        selected_item(list(level = "domain", domain = domain, category = NULL, type = NULL))
      }, ignoreInit = TRUE)
      cats <- risk_cats_df |> dplyr::filter(risk_domain == domain)
      lapply(seq_len(nrow(cats)), function(j) {
        cat_name <- cats$risk_category[j]
        observeEvent(input[[paste0("risk_cat_", i, "_", j)]], {
          selected_item(list(level = "category", domain = domain, category = cat_name, type = NULL))
        }, ignoreInit = TRUE)
        types <- risk_types_df |> dplyr::filter(risk_domain == domain, risk_category == cat_name)
        lapply(seq_len(nrow(types)), function(k) {
          type_name <- types$risk_type[k]
          observeEvent(input[[paste0("risk_type_", i, "_", j, "_", k)]], {
            selected_item(list(level = "type", domain = domain, category = cat_name, type = type_name))
          }, ignoreInit = TRUE)
        })
      })
    })
  })

  # Risk summary panel
  output$risk_summary_panel <- renderUI({
    sel <- selected_item()
    if (is.null(sel$level)) return(NULL)

    # Get studies_data from the parent environment (loaded in database_helpers.R)
    sd <- if (exists("studies_data")) studies_data else NULL

    if (sel$level == "domain") {
      domain_desc <- risk_domains_df$risk_domain_description[risk_domains_df$risk_domain == sel$domain]
      cats <- risk_cats_df |> dplyr::filter(risk_domain == sel$domain)
      studies <- if (!is.null(sd)) sd |> dplyr::filter(grepl(sel$domain, permanence_risk_domain, ignore.case = TRUE)) else NULL
      tagList(
        h4(sel$domain),
        p(domain_desc),
        h5("Categories:"),
        tags$ul(lapply(seq_len(nrow(cats)), function(j) {
          tags$li(tags$b(cats$risk_category[j]), ":", cats$risk_category_description[j])
        })),
        render_study_links(studies)
      )
    } else if (sel$level == "category") {
      cat_desc <- risk_cats_df$risk_category_description[
        risk_cats_df$risk_domain == sel$domain & risk_cats_df$risk_category == sel$category
      ]
      types <- risk_types_df |> dplyr::filter(risk_domain == sel$domain, risk_category == sel$category)
      studies <- if (!is.null(sd)) sd |> dplyr::filter(grepl(sel$category, permanence_risk_category, ignore.case = TRUE)) else NULL
      tagList(
        h4(sel$category),
        p(cat_desc),
        h5("Types:"),
        tags$ul(lapply(seq_len(nrow(types)), function(k) {
          tags$li(tags$b(types$risk_type[k]), ":", types$risk_type_description[k])
        })),
        render_study_links(studies)
      )
    } else if (sel$level == "type") {
      type_desc <- risk_types_df$risk_type_description[
        risk_types_df$risk_domain == sel$domain &
        risk_types_df$risk_category == sel$category &
        risk_types_df$risk_type == sel$type
      ]
      studies <- if (!is.null(sd)) sd |> dplyr::filter(grepl(sel$type, permanence_risk_type, ignore.case = TRUE)) else NULL
      tagList(
        h4(sel$type),
        p(type_desc),
        render_study_links(studies)
      )
    }
  })

  output$riskSummaryVisible <- reactive({
    !is.null(selected_item()$level)
  })
  outputOptions(output, "riskSummaryVisible", suspendWhenHidden = FALSE)
}
