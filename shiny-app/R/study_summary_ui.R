# ============================================================================
# study_summary_ui.R - Study Summary UI Component
# ============================================================================
# Purpose: Generate the study summary panel UI used in both Database and
#          Studies tabs to display detailed information about selected studies
#
# Key Functions:
#   - studySummaryUI(): Render study details including title, abstract,
#                       geographic info, and associated risks
#
# Dependencies:
#   - typology data for risk categorization
#   - coords_all for geographic coordinates display
# ============================================================================

studySummaryUI <- function(study, typology = NULL, coords_all = NULL) {
  # Use typology mapping for risks (robust matching)
  risk_types <- unique(unlist(strsplit(as.character(study$permanence_risk_type), "; ?")))
  risk_types <- trimws(risk_types)
  risk_types <- risk_types[nzchar(risk_types)]
  # Normalize spelling: behaviour -> behavior
  normalize_spelling <- function(x) {
    gsub("behaviour", "behavior", tolower(x))
  }
  if (!is.null(typology)) {
    typology_risk_types <- normalize_spelling(typology$risk_type)
    study_risk_types <- normalize_spelling(risk_types)
    match_idx <- which(typology_risk_types %in% study_risk_types)
    risks_info <- typology[match_idx, c("risk_domain", "risk_category", "risk_type")]
    risks_nested <- list()
    for (i in seq_len(nrow(risks_info))) {
      domain <- risks_info$risk_domain[i]
      category <- risks_info$risk_category[i]
      type <- risks_info$risk_type[i]
      if (!domain %in% names(risks_nested)) risks_nested[[domain]] <- list()
      if (!category %in% names(risks_nested[[domain]])) risks_nested[[domain]][[category]] <- c()
      risks_nested[[domain]][[category]] <- unique(c(risks_nested[[domain]][[category]], type))
    }
  } else {
    risks_nested <- list()
  }
  render_risks <- function(risks) {
    if (length(risks) == 0) return(p("No risks identified."))
    tags$ul(
      lapply(names(risks), function(domain) {
        domain_id <- paste0("risklink_domain_", digest::digest(domain))
        tags$li(
          shiny::actionLink(
            inputId = domain_id,
            label = tags$b(domain),
            class = "risk-link-domain",
            `data-domain` = domain
          ),
          tags$ul(
            lapply(names(risks[[domain]]), function(category) {
              category_id <- paste0("risklink_category_", digest::digest(paste(domain, category)))
              tags$li(
                shiny::actionLink(
                  inputId = category_id,
                  label = category,
                  class = "risk-link-category",
                  `data-domain` = domain,
                  `data-category` = category
                ),
                tags$ul(
                  lapply(risks[[domain]][[category]], function(type) {
                    id <- paste0("risklink_", digest::digest(paste(domain, category, type)))
                    tags$li(
                      shiny::actionLink(
                        inputId = id,
                        label = type,
                        class = "risk-link",
                        `data-domain` = domain,
                        `data-category` = category,
                        `data-type` = type
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
  }
  doi <- if ("doi" %in% names(study)) as.character(study$doi) else ""
  doi_link <- if (nzchar(doi)) {
    tags$a(href = paste0("https://doi.org/", doi), target = "_blank", doi)
  } else {
    "Not available"
  }
  # Tag for offset category (biodiversity or carbon) using Pastel1 colors
  offset_tag <- NULL
  if (!is.null(study$offset_category_general) && nzchar(study$offset_category_general)) {
    cat_val <- tolower(study$offset_category_general)
    tag_color <- if (cat_val == "carbon") "#F0C05A" else "#B19CD9"  # Matches offset_colors: gold=carbon, purple=biodiversity
    text_color <- "#333"  # Use dark text for pastel backgrounds
    offset_tag <- tags$span(
      style = paste0("display: inline-block; background: ", tag_color, "; color: ", text_color, "; border-radius: 12px; padding: 0.12em 0.7em; font-size: 0.92em; font-weight: 600; margin-bottom: 0.7em;"),
      toupper(study$offset_category_general)
    )
  }
  tagList(
    tags$h2("Study Summary", style = "font-size: 2em; font-weight: bold; margin-bottom: 1em; margin-right: 0.5em;"),
    div(
      style = "font-size: 1.1em; margin-bottom: 1.5em;",
      tags$h3("Study Information", style = "font-size: 1.4em; font-weight: bold; margin-top: 0.5em;"),
      if (!is.null(offset_tag)) div(offset_tag, style = "margin-bottom: 0.5em;"),
      div(
        tags$b("Title: "), as.character(study$study_title), tags$br(),
        tags$b("DOI: "), doi_link, tags$br(),
        tags$b("Year Published: "), as.character(study$study_publication_year), tags$br(),
        tags$b("Abstract: "), if (!is.null(study$study_abstract) && nzchar(study$study_abstract)) as.character(study$study_abstract) else "Not available"
      )
    ),
    div(
      style = "font-size: 1.1em; margin-bottom: 1.5em;",
      tags$h3("Study Characteristics", style = "font-size: 1.4em; font-weight: bold; margin-top: 0.5em;"),
      div(
        tags$b("Study Type: "), if (!is.null(study$study_evidence_type) && nzchar(study$study_evidence_type)) as.character(study$study_evidence_type) else "Not available", tags$br(),
        tags$b("Ecosystem(s) mentioned: "), if (!is.null(study$ecosystem_broad_type) && nzchar(study$ecosystem_broad_type)) as.character(study$ecosystem_broad_type) else "Not available", tags$br(),
        tags$b("Project type(s) described: "), if (!is.null(study$project_broad_type) && nzchar(study$project_broad_type)) as.character(study$project_broad_type) else "Not available", tags$br(),
        tags$b("Program(s) discussed: "), if (!is.null(study$program_name) && nzchar(study$program_name)) as.character(study$program_name) else "Not available", tags$br(),
        tags$b("Policy(ies) discussed: "), if (!is.null(study$policy_name) && nzchar(study$policy_name)) as.character(study$policy_name) else "Not available"
      )
    ),
    div(
      style = "font-size: 1.1em; margin-bottom: 1.5em;",
      tags$h3("Geographic Context", style = "font-size: 1.4em; font-weight: bold; margin-top: 0.5em;"),
      div(
        style = "display: flex; align-items: flex-start; gap: 18px;",
        div(
          style = "width: 200px; height: 200px; border: 1px solid #ccc; overflow: hidden; flex-shrink: 0;",
          leaflet::leafletOutput("study_geo_map", width = "200px", height = "200px")
        ),
        div(
          tags$b("Country: "), as.character(study$country), tags$br(),
          tags$b("Subnational Region: "), as.character(study$subnational_region)
        )
      )
    ),
    div(
      style = "font-size: 1.1em; margin-bottom: 1.5em;",
      tags$h3("Risks Identified", style = "font-size: 1.4em; font-weight: bold; margin-top: 0.5em;"),
      render_risks(risks_nested)
    ),
    if (!is.null(study$permanence_risk_evidence_and_reasoning) && nzchar(study$permanence_risk_evidence_and_reasoning)) {
      div(
        style = "font-size: 1.1em; margin-bottom: 1.5em;",
        tags$h3("Textual Evidence and Reasoning", style = "font-size: 1.2em; font-weight: bold; margin-top: 0.5em;"),
        tags$blockquote(
          paste0('"', as.character(study$permanence_risk_evidence_and_reasoning), '" '),
          if (!is.null(study$reference) && nzchar(study$reference)) {
            study$reference
          } else {
            NULL
          },
          if (!is.null(study$doi) && nzchar(study$doi)) {
            tags$span(
              " ",
              tags$a(href = paste0("https://doi.org/", study$doi), target = "_blank", "[View Paper]")
            )
          } else {
            NULL
          }
        )
      )
    }
  )
}
