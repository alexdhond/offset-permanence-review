# =============================================================================
# Helper Functions for Offset Permanence Review Analysis
# =============================================================================
# This script contains reusable helper functions for data summarization,
# table formatting, and visualization used across multiple analysis documents.
# =============================================================================

#' Count Unique Studies by Variable
#'
#' Summarizes the number of unique studies that mention each value of a
#' specified variable. Filters out NA and empty string values.
#'
#' @param data A data frame containing the dataset
#' @param var The variable to summarize (unquoted column name)
#' @param label Optional custom label for the variable column in output.
#'   If NULL, uses the variable name.
#'
#' @return A tibble with two columns: the variable (or custom label) and
#'   n_studies (count of unique study_id values), sorted by descending count
#'
#' @examples
#' summarize_by_study(final_df, offset_category_general)
#' summarize_by_study(final_df, ecosystem_broad_type, label = "Ecosystem")
summarize_by_study <- function(data, var, label = NULL) {
  var <- rlang::enquo(var)
  label <- label %||% rlang::as_name(var)

  data %>%
    filter(!is.na(!!var), !!var != "") %>%
    group_by(!!var) %>%
    summarise(n_studies = n_distinct(study_id), .groups = "drop") %>%
    arrange(desc(n_studies)) %>%
    rename(!!label := !!var)
}


#' Format a Summary Table using gt
#'
#' Creates a formatted gt table from the output of summarize_by_study().
#' Applies consistent styling with borders, bold headers, and numeric formatting.
#'
#' @param df Data frame from summarize_by_study() with two columns
#' @param var_name Character string for the first column name
#' @param table_title Character string for the table title (will be bolded)
#'
#' @return A gt table object ready for rendering
#'
#' @examples
#' offset_summary <- summarize_by_study(final_df, offset_category_general)
#' make_summary_table(offset_summary, "offset_category_general", "Offset Types")
make_summary_table <- function(df, var_name, table_title) {
  df %>%
    gt::gt() %>%
    gt::tab_header(title = gt::md(paste0("**", table_title, "**"))) %>%
    gt::cols_label(
      !!var_name := "Category",
      n_studies = "Number of Studies"
    ) %>%
    gt::fmt_number(columns = "n_studies", decimals = 0) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(everything())
    ) %>%
    gt::tab_options(
      table.width = "90%",
      table.border.top.style = "solid",
      table.border.top.width = gt::px(1),
      table.border.top.color = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.width = gt::px(1),
      table.border.bottom.color = "black",
      table.border.left.style = "solid",
      table.border.left.width = gt::px(1),
      table.border.left.color = "black",
      table.border.right.style = "solid",
      table.border.right.width = gt::px(1),
      table.border.right.color = "black"
    )
}


#' Generate Safe Combinations from a List
#'
#' Creates all possible combinations of size combo_size from a list.
#' Returns empty list if input list is shorter than combo_size to prevent errors.
#'
#' @param risk_list A character vector of items to combine
#' @param combo_size Integer specifying the size of each combination (default = 2)
#'
#' @return A list of character vectors, each containing one combination.
#'   Returns empty list() if length(risk_list) < combo_size.
#'
#' @examples
#' get_combos(c("Risk A", "Risk B", "Risk C"), combo_size = 2)
#' # Returns: list(c("Risk A", "Risk B"), c("Risk A", "Risk C"), c("Risk B", "Risk C"))
get_combos <- function(risk_list, combo_size = 2) {
  if (length(risk_list) >= combo_size) {
    combn(risk_list, combo_size, simplify = FALSE)
  } else {
    list()
  }
}


#' Count Studies Reporting a Variable
#'
#' Counts the number of distinct studies (study_id) that have non-missing
#' values for a specified variable. Requires 'final_df' to exist in the
#' calling environment.
#'
#' @param var The variable to check (unquoted column name)
#'
#' @return Integer count of unique study_id values with non-NA variable values
#'
#' @examples
#' studies_reporting(ecosystem_broad_type)
#' studies_reporting(program_name)
#'
#' @note This function depends on 'final_df' being available in the environment
studies_reporting <- function(var) {
  final_df %>%
    filter(!is.na({{ var }})) %>%
    distinct(study_id) %>% 
    nrow()
}


#' Create a Standardized Bar Plot with ggplot2
#'
#' Generates a horizontal or vertical bar chart with consistent styling,
#' including value labels, color fills, and clean theming. Used for repeated
#' visualizations in supplementary figures.
#'
#' @param data Data frame containing the plotting variables
#' @param x_var X-axis variable (as rlang expression, e.g., rlang::expr(category))
#' @param y_var Y-axis variable (as rlang expression, e.g., rlang::expr(count))
#' @param fill_var Fill variable for color mapping (as rlang expression)
#' @param x_lab Character string for x-axis label
#' @param y_lab Character string for y-axis label
#' @param fill_lab Character string for fill legend label
#' @param flip Logical: if TRUE (default), creates horizontal bar chart via coord_flip()
#'
#' @return A ggplot2 object
#'
#' @examples
#' make_bar_plot(
#'   year_summary,
#'   rlang::expr(study_publication_year),
#'   rlang::expr(n_studies),
#'   rlang::expr(study_publication_year),
#'   x_lab = "Publication Year",
#'   y_lab = "Number of Studies",
#'   fill_lab = "Year",
#'   flip = FALSE
#' )
make_bar_plot <- function(data, x_var, y_var, fill_var, x_lab, y_lab, fill_lab, flip = TRUE, fill_colors = NULL) {
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!fill_var)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9), color = "black") +
    ggplot2::geom_text(
      ggplot2::aes(label = !!y_var),
      position = ggplot2::position_dodge(width = 0.9),
      hjust = -0.4, size = 4, color = "black"
    )

  if (!is.null(fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = fill_colors)
  } else {
    p <- p + ggplot2::scale_fill_brewer(palette = "Pastel1")
  }

  p <- p +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      fill = fill_lab
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = 10)),
      legend.position = "top",
      legend.text = ggplot2::element_text(),
      legend.title = ggplot2::element_text(),
      plot.margin = ggplot2::margin(1, 2, 1, 1, unit = "lines")
    )
  
  if (flip) {
    p <- p + ggplot2::coord_flip(clip = "off")
  }
  
  return(p)
}
