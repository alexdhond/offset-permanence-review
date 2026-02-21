# =============================================================================
# Color Palettes for Offset Permanence Review Analysis
# =============================================================================
# This script defines centralized color palettes used across all analysis
# documents to ensure visual consistency in figures and plots.
# =============================================================================

# Load required package for color manipulation
library(colorspace)

# -----------------------------------------------------------------------------
# Global Risk Category Colors
# -----------------------------------------------------------------------------
# Fixed color palette for the 10 permanence risk categories, organized by domain.
# Structure: Physical = Greens, Non-Physical = Blues, Methodological = Reds
#
# These colors are used in diverging bar plots, pie charts, and other figures
# where risk categories need to be distinguished.

global_risk_colors <- c(
  # Physical (Green shades)
  "Climate and Environmental Disturbances" = "#a1d99b",
  "Direct Anthropogenic Disturbances" = "#31a354",


  # Non-Physical (Blue shades)
  "Compliance, Legal, and Governance Risks" = "#c6dbef",
  "Data, Transparency, and Capacity Issues" = "#9ecae1",
  "Financial Risks" = "#6baed6",
  "Political Risks" = "#3182bd",
  "Socioeconomic and Equity Risks" = "#08519c",

  # Methodological (Red shades)
  "Ecological Design and Implementation Failures" = "#fc9272",
  "Misaligned Metrics, Standards, and Performance Criteria" = "#ef3b2c",
  "Systemic Oversights and Risk Management Gaps" = "#99000d"
)


# -----------------------------------------------------------------------------
# Domain Colors
# -----------------------------------------------------------------------------
# Base colors for the three permanence risk domains.
# Used for domain-level summaries and as the base for generating category palettes.

domain_colors <- c(
  "non-physical" = "#4E79A7",       # Blue
  "physical" = "#59A14F",           # Green
  "methodological" = "#E15759"      # Red
)


# -----------------------------------------------------------------------------
# Domain Order
# -----------------------------------------------------------------------------
# Standard ordering for permanence risk domains (most to least common).

domain_order <- c("non-physical", "physical", "methodological")


# -----------------------------------------------------------------------------
# Helper Function: Generate Domain-Based Palette
# -----------------------------------------------------------------------------
#' Generate a color palette for risk categories within a domain
#'
#' Creates a gradient of colors from white to the specified base color.
#' Useful for creating hierarchical visualizations where categories within
#' a domain need related but distinguishable colors.
#'
#' @param base_color Hex color code for the domain's base color
#' @param n Number of colors to generate (number of categories in the domain)
#'
#' @return Character vector of n hex color codes, from lightest to darkest
#'
#' @examples
#' # Generate 5 shades of blue for non-physical risk categories
#' get_domain_palette("#4E79A7", 5)
#'
#' # Generate palette for all categories in a domain
#' physical_cats <- c("Climate and Environmental Disturbances",
#'                    "Direct Anthropogenic Disturbances")
#' get_domain_palette(domain_colors["physical"], length(physical_cats))

get_domain_palette <- function(base_color, n) {
  shades <- colorRampPalette(c("#FFFFFF", base_color))(n + 1)[-1]
  return(shades)
}


# -----------------------------------------------------------------------------
# Helper Function: Generate Ordered Category Colors
# -----------------------------------------------------------------------------
#' Generate a named color vector for risk categories ordered by luminance
#'
#' Creates colors for each risk category based on their domain, ordered
#' from dark to light within each domain. This ensures consistent visual
#' hierarchy across plots.
#'
#' @param category_domain_df Data frame with columns `permanence_risk_category`
#'   and `permanence_risk_domain`
#'
#' @return Named character vector of hex colors, where names are category names
#'
#' @examples
#' cat_domain <- final_df %>%
#'   distinct(permanence_risk_category, permanence_risk_domain)
#' colors <- get_ordered_category_colors(cat_domain)

get_ordered_category_colors <- function(category_domain_df) {
  # Generate initial colors by domain
  category_colors_df <- category_domain_df %>%
    dplyr::group_by(permanence_risk_domain) %>%
    dplyr::arrange(permanence_risk_category) %>%
    dplyr::mutate(
      color = get_domain_palette(domain_colors[permanence_risk_domain[1]], dplyr::n())
    ) %>%
    dplyr::ungroup()

  # Calculate luminance for ordering
  rgb_matrix <- colorspace::hex2RGB(category_colors_df$color)@coords
  luminance_vals <- 0.299 * rgb_matrix[, 1] +
                    0.587 * rgb_matrix[, 2] +
                    0.114 * rgb_matrix[, 3]

  # Order by domain, then by luminance (dark to light)
  category_order_df <- category_colors_df %>%
    dplyr::mutate(
      luminance = luminance_vals,
      permanence_risk_domain = factor(
        permanence_risk_domain,
        levels = c("physical", "non-physical", "methodological")
      )
    ) %>%
    dplyr::arrange(permanence_risk_domain, dplyr::desc(luminance))

  # Return named vector
  colors <- setNames(
    category_order_df$color,
    category_order_df$permanence_risk_category
  )

  return(colors)
}


# -----------------------------------------------------------------------------
# Offset Category Colors
# -----------------------------------------------------------------------------
# Colors for distinguishing biodiversity vs carbon offset types.
# Uses Pastel1 palette colors for consistency with existing figures.

# Define offset colors - pastel purple and darker gold/yellow
offset_colors <- c("biodiversity" = "#B19CD9", "carbon" = "#F0C05A")


# -----------------------------------------------------------------------------
# Confirmation Message
# -----------------------------------------------------------------------------

message("Color palettes loaded successfully!")
message("  - global_risk_colors: ", length(global_risk_colors), " risk category colors")
message("  - domain_colors: ", length(domain_colors), " domain colors")
message("  - Helper functions: get_domain_palette(), get_ordered_category_colors()")
