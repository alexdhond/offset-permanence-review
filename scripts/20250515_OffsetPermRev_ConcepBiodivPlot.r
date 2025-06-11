# ===============================================
# Script:     20250515_OffsetPermRev_ConcepBiodivPlot.r
# Date:       2025-05-23
# Author:     Alex Dhond
# Purpose:    Plot conceptual biodiversity offset trajectories
# ===============================================

#-----------------------------
# Load Required Packages
#-----------------------------

library(ggplot2)   # Plotting
library(patchwork) # Combining plots
library(dplyr)     # Data manipulation
library(scales)    # Scale transformations
library(purrr)     # Functional iteration
library(here)      # Relative paths

#-----------------------------
# Global settings and functions
#-----------------------------

# Define time vector
time <- seq(0, 100, by = 0.1)

# Logistic growth function
s_sigmoid <- function(t, K = 100, r = 0.3, t0 = 15) {
  K / (1 + exp(-r * (t - t0)))
}

# Generate a trajectory dataframe from a named biodiversity function
generate_trajectory <- function(name, biodiversity_func) {
  data.frame(
    time = time,
    biodiversity = biodiversity_func(time),
    scenario = name
  )
}

#-----------------------------
# Define offset scenarios
#-----------------------------

# Baseline scenario names for later reference
baseline_scenarios <- c("Habitat Creation", "Habitat Restoration", "Avoided Loss")

# Avoided loss trajectories: flat, rising, and counter factual decline
avoided_loss_flat <- generate_trajectory("Avoided Loss", function(t) rep(90, length(t))) %>%
  mutate(type = "Actual", subtype = "Flat")

avoided_loss_rising <- generate_trajectory("Avoided Loss", function(t) 90 + (10 / max(t)) * t) %>%
  mutate(type = "Actual", subtype = "Rising")

avoided_loss_counterfactual <- generate_trajectory("Avoided Loss", function(t) pmax(0, 90 - 0.8 * t)) %>%
  mutate(type = "Counterfactual", subtype = "Declining (No Protection)")


# All offset and restoration scenarios
scenario_list <- list(
  
  # Habitat Creation
  generate_trajectory("Habitat Creation", function(t) {
    pmin(s_sigmoid(t, K = 100, r = 0.2, t0 = 30), 100)
  }) %>% mutate(type = "Actual", subtype = "Baseline"),
  
  # Habitat Restoration
  generate_trajectory("Habitat Restoration", function(t) {
    lower <- 50
    upper <- 100
    increment <- s_sigmoid(t, K = upper - lower, r = 0.35, t0 = 25)
    lower + increment
  }) %>% mutate(type = "Actual", subtype = "Baseline"),

  generate_trajectory("Ideal Offset (max = 100)", function(t) pmin(s_sigmoid(t), 100)),
  generate_trajectory("Offset with Limited Gains (max = 70)", function(t) pmin(s_sigmoid(t, K = 70), 70)),
  generate_trajectory("Offset Failure: Catastrophic Collapse (Year 20)", function(t) {
    traj <- pmin(s_sigmoid(t), 100)
    traj[t >= 20] <- 0
    traj
  }),
  generate_trajectory("Offset Shock: Fire with Partial Recovery", function(t) {
    fire_time <- 15; drop_value <- 20
    rise <- pmin(s_sigmoid(t), 100)
    t_rise <- t[t < fire_time]
    values_rise <- rise[t < fire_time]
    values_drop <- drop_value
    t_recovery <- t[t > fire_time]
    recovery_base <- 35 / (1 + exp(-0.05 * (t_recovery - 60)))
    offset <- drop_value - recovery_base[1]
    values_recovery <- recovery_base + offset
    c(values_rise, values_drop, values_recovery)
  }),
  generate_trajectory("Offset Shock: Fire with No Recovery", function(t) {
    fire_time <- 15; drop_value <- 20
    rise <- pmin(s_sigmoid(t), 100)
    t_rise <- t[t < fire_time]
    values_rise <- rise[t < fire_time]
    t_decline <- t[t > fire_time]
    slope <- drop_value / (100 - fire_time)
    values_decline <- pmax(0, drop_value - slope * (t_decline - fire_time))
    c(values_rise, drop_value, values_decline)
  }),
  generate_trajectory("Offset Failure: Transitory Gain (Peak at Year 3.5)", function(t) {
    bell <- 20 * exp(-((t - 3.5)^2) / (2 * 1^2))
    bell[t > 7] <- 0
    bell
  }),
  generate_trajectory("Offset Failure: Early Gain Then Collapse", function(t) {
    rise <- 30 / (1 + exp(-0.4 * (t - 5)))
    decay <- 1 / (1 + exp(0.3 * (t - 20)))
    trajectory <- rise * decay
    trajectory[t > 50] <- 0
    trajectory
  }),
  generate_trajectory("Offset Risk: Amplified Storm Frequency with Recovery", function(t) {
    initial <- 100 / (1 + exp(-0.3 * (t - 10)))
    biodiv <- initial
    storm_times <- c(8, 20, 35, 47, 58, 67, 75, 82, 88)
    drop_depths <- seq(15, 50, length.out = length(storm_times))
    recovery_duration <- 12
    
    for (i in seq_along(storm_times)) {
      drop_idx <- which.min(abs(t - storm_times[i]))
      recovery_end <- which.min(abs(t - (storm_times[i] + recovery_duration)))
      biodiv[drop_idx:length(biodiv)] <- biodiv[drop_idx:length(biodiv)] - drop_depths[i]
      if (recovery_end > drop_idx) {
        recovery_t <- t[drop_idx:recovery_end]
        recovery_curve <- (drop_depths[i] * 0.8) / (1 + exp(-0.5 * (recovery_t - (storm_times[i] + recovery_duration / 2))))
        biodiv[drop_idx:recovery_end] <- biodiv[drop_idx:recovery_end] + recovery_curve
      }
    }
    pmax(biodiv, 0)
  }),
  generate_trajectory("Offset Failure: Invasive Species After Management Ends", function(t) {
    management_end <- 5; final_value <- 25
    rise <- 100 / (1 + exp(-0.3 * (t - 10)))
    decay_multiplier <- rep(1, length(t))
    decay_start <- which(t > management_end)
    
    if (length(decay_start) > 0) {
      decay_curve <- exp(-0.05 * (t[decay_start] - management_end))
      target_drop <- 1 - final_value / 100
      decay_multiplier[decay_start] <- (1 - target_drop) + target_drop * decay_curve
    }
    rise * decay_multiplier
  })
)

# Tag all scenarios with default type/subtype if not already set
scenario_list <- map(scenario_list, function(df) {
  df %>%
    mutate(
      type = if (!"type" %in% names(.)) "Actual" else type,
      subtype = if (!"subtype" %in% names(.)) "Baseline" else subtype
    )
})

# Combine all scenarios into one dataframe
df_all <- bind_rows(scenario_list, avoided_loss_flat, avoided_loss_rising, avoided_loss_counterfactual)

#-----------------------------
# Define Colors
#-----------------------------
offset_colors <- c(
  "Ideal Offset (max = 100)" = "#1b9e77",
  "Offset with Limited Gains (max = 70)" = "#7570b3",
  "Offset Failure: Catastrophic Collapse (Year 20)" = "#d95f02",
  "Offset Shock: Fire with Partial Recovery" = "#66a61e",
  "Offset Shock: Fire with No Recovery" = "#d73027",
  "Offset Failure: Transitory Gain (Peak at Year 3.5)" = "#a6cee3",
  "Offset Failure: Early Gain Then Collapse" = "#984ea3",
  "Offset Risk: Amplified Storm Frequency with Recovery" = "#377eb8",
  "Offset Failure: Invasive Species After Management Ends" = "#a65628"
)

# Fill in missing colors automatically
missing_colors <- setdiff(unique(df_all$scenario), names(offset_colors))
if (length(missing_colors) > 0) {
  auto_colors <- RColorBrewer::brewer.pal(max(3, length(missing_colors)), "Set3")
  names(auto_colors) <- missing_colors
  offset_colors <- c(offset_colors, auto_colors[missing_colors])
}

#-----------------------------
# Plot Function
#-----------------------------
# -----------------------------
# Plot Function (Modified to be more general)
# -----------------------------
plot_offset_trajectories <- function(df, log_scale = TRUE, facet = FALSE, focal = NULL, show_title = TRUE) {
  scenarios <- unique(df$scenario)
  
  # Get style identifiers
  style_ids <- unique(with(df, interaction(scenario, type, subtype)))
  
  # Repeat scenario color for each style_id based on the scenario part
  get_scenario_from_id <- function(id) strsplit(as.character(id), split = "\\.")[[1]][1]
  color_vals <- setNames(
    vapply(style_ids, function(id) offset_colors[get_scenario_from_id(id)], character(1)),
    style_ids
  )
  
  # Style settings
  alpha_vals <- setNames(rep(1, length(style_ids)), style_ids)
  linewidth_vals <- setNames(rep(1.2, length(style_ids)), style_ids)
  
  p <- ggplot(df, aes(
    x = time,
    y = biodiversity,
    group = interaction(scenario, type, subtype),
    color = interaction(scenario, type, subtype),
    alpha = interaction(scenario, type, subtype),
    linewidth = interaction(scenario, type, subtype),
    linetype = subtype
  )) +
    geom_line(show.legend = FALSE) +
    scale_color_manual(values = color_vals) +
    scale_alpha_manual(values = alpha_vals) +
    scale_linewidth_manual(values = linewidth_vals) +
    scale_linetype_manual(values = c(
      "Flat" = "dashed",
      "Rising" = "solid",
      "Declining (No Protection)" = "dotted")) +
    labs(
      title = if (show_title) { # Title for individual plot
        if (!is.null(focal)) {
          focal
        } else if (length(unique(df$scenario)) == 1) {
          unique(df$scenario)
        } else if (log_scale) {
          "Offset Trajectories (Log Time)"
        } else {
          "Offset Trajectories"
        }
      } else NULL,
      x = "Time", # Default X axis title
      y = "Relative Biodiversity" # Default Y axis title
    ) +
    theme_minimal(base_size = 18) + # Increased base_size for all text elements
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5), # Adjust specific elements if needed
      legend.position = if (facet) "none" else "bottom"
      # REMOVED: axis.title.x = element_blank(), axis.title.y = element_blank(), etc.
      # We will control these externally now
    )
  
  p <- p + if (facet) facet_wrap(~scenario, ncol = 2) else NULL
  p <- p + if (log_scale) scale_x_log10(limits = c(1, 100), breaks = c(1, 5, 10, 20, 50, 100)) else scale_x_continuous(limits = c(1, 100))
  
  p
}

#-----------------------------
# Preview plots
#-----------------------------
# View a combined plot (log scale)
print(plot_offset_trajectories(df_all, log_scale = TRUE, show_title = TRUE))

# View a combined plot (linear scale)
print(plot_offset_trajectories(df_all, log_scale = FALSE, show_title = TRUE))

# Preview a single scenario highlight
print(plot_offset_trajectories(df_all, log_scale = TRUE, focal = "Offset Failure: Catastrophic Collapse (Year 20)", show_title = TRUE))


baseline_color <- "#1f78b4"  # Dark blue

# creation 
plot_hc <- plot_offset_trajectories(
  df_all %>% filter(scenario == "Habitat Creation"),
  log_scale = FALSE,
  show_title = TRUE
) +
  scale_color_manual(values = c("Habitat Creation.Actual.Baseline" = baseline_color)) +
  scale_linetype_manual(values = c("Baseline" = "solid"))

# restoration
plot_hr <- plot_offset_trajectories(
  df_all %>% filter(scenario == "Habitat Restoration"),
  log_scale = FALSE,
  show_title = TRUE
) +
  scale_color_manual(values = c("Habitat Restoration.Actual.Baseline" = "#1f78b4")) +
  scale_linetype_manual(values = c("Baseline" = "solid")) +
  ylim(0, 100)

# Get all subtype identifiers for avoided loss
avoided_loss_ids <- df_all %>%
  filter(scenario == "Avoided Loss") %>%
  mutate(id = interaction(scenario, type, subtype)) %>%
  distinct(id) %>%
  pull(id)

# Build color map (all dark blue)
color_map_al <- setNames(rep(baseline_color, length(avoided_loss_ids)), avoided_loss_ids)

# Linetype map
linetype_map_al <- c(
  "Flat" = "dashed",
  "Rising" = "solid",
  "Declining (No Protection)" = "dotted"
)

plot_al <- plot_offset_trajectories(
  df_all %>% filter(scenario == "Avoided Loss"),
  log_scale = FALSE,
  show_title = TRUE
) +
  scale_color_manual(values = color_map_al) +
  scale_linetype_manual(values = linetype_map_al)

print(plot_hc)
print(plot_hr)
print(plot_al)


# -----------------------------
# Preview plots (Modified for specific axis labels and all numbers/ticks)
# -----------------------------
baseline_color <- "#1f78b4"  # Dark blue

# Habitat Creation (plot_hc) - Will have Y-axis title and visible Y-axis numbers/ticks
plot_hc <- plot_offset_trajectories(
  df_all %>% filter(scenario == "Habitat Creation"),
  log_scale = FALSE,
  show_title = TRUE
) +
  scale_color_manual(values = c("Habitat Creation.Actual.Baseline" = baseline_color)) +
  scale_linetype_manual(values = c("Baseline" = "solid")) +
  ylim(0, 100) +
  labs(y = "Relative Biodiversity") + # Add Y-axis title for this specific plot
  theme(
    axis.title.y = element_text(size = 20  , margin = margin(r = 10)), # Make Y title visible
    axis.text.y = element_text(size = 16), # Make Y tick labels visible and set size
    axis.ticks.y = element_line(size = 0.5), # Make Y ticks visible
    # Ensure X-axis elements are hidden
    axis.title.x = element_blank(),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm") # Reduce margins
  )+
  theme_minimal()

# Habitat Restoration (plot_hr) - Will have X-axis title and visible X-axis numbers/ticks
plot_hr <- plot_offset_trajectories(
  df_all %>% filter(scenario == "Habitat Restoration"),
  log_scale = FALSE,
  show_title = TRUE
) +
  scale_color_manual(values = c("Habitat Restoration.Actual.Baseline" = "#1f78b4")) +
  scale_linetype_manual(values = c("Baseline" = "solid")) +
  ylim(0, 100) +
  labs(x = "Time") + # Add X-axis title for this specific plot
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 10)), # Make X title visible
    axis.text.x = element_text(size = 16), # Make X tick labels visible and set size
    axis.ticks.x = element_line(size = 0.5), # Make X ticks visible
    # Ensure Y-axis elements are hidden
    axis.title.y = element_blank(),
    axis.text.y = element_blank(), # Hide Y-axis tick labels for this panel
    axis.ticks.y = element_blank(), # Hide Y-axis ticks for this panel
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm") # Reduce margins
  )+
  theme_minimal()

# Avoided Loss (plot_al) - Will have visible X and Y axis numbers/ticks, but no titles
plot_al <- plot_offset_trajectories(
  df_all %>% filter(scenario == "Avoided Loss"),
  log_scale = FALSE,
  show_title = TRUE
) +
  scale_color_manual(values = c("Avoided Loss.Actual.Flat" = baseline_color,
                                "Avoided Loss.Actual.Rising" = baseline_color,
                                "Avoided Loss.Counterfactual.Declining (No Protection)" = baseline_color)) +
  scale_linetype_manual(values = linetype_map_al) +
  ylim(0, 100) +
  labs(x = NULL, y = NULL) + # Ensure no axis titles for this plot
  theme(
    # Make X and Y axis text (numbers) and ticks visible
    axis.text.x = element_text(size = 16), # X tick labels visible
    axis.ticks.x = element_line(size = 0.5), # X ticks visible
    axis.text.y = element_text(size = 16), # Y tick labels visible
    axis.ticks.y = element_line(size = 0.5), # Y ticks visible
    # Ensure axis titles are hidden (already done by labs(x=NULL, y=NULL))
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm") # Reduce margins
  )+
  theme_minimal()
# -----------------------------
# Combine plots into a three-panel figure
# -----------------------------



# Combine the three baseline plots
# Combine the three baseline plots
combined_baseline_plot <- (plot_hc + plot_hr + plot_al) +
  plot_layout(
    ncol = 3,
    guides = 'collect',
    widths = c(1, 1, 1) # Equal width for each plot
    # Adjust spacing between plots: negative values pull them closer
    # Unit can be "cm", "in", "mm", etc.
    # Play with these values to get the desired closeness
    # For example, to make them closer, try a small positive value like 0.1cm or even negative
    # Example: you want to keep them close, so maybe a very small positive or zero
    # For closer plots, theme(plot.margin) below might be more effective
    # or you might want to adjust `design` for more control over space.
    # For now, we'll primarily use `theme(plot.margin)` on individual plots
  ) &
  theme(
    legend.position = 'bottom',
    # Reduce margins of individual plots to bring them closer
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm") # Top, Right, Bottom, Left margins
  )+
  theme_minimal()
combined_baseline_plot

# Add an overall title and common axis labels to the combined plot
final_combined_plot <- combined_baseline_plot +
  plot_annotation(
    tag_levels = 'A', # Add A, B, C tags to subplots
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5), # Increased main title size
      plot.tag = element_text(face = "bold", size = 20) # Increased A, B, C tag size
    )
  ) +
  # You might want to adjust the size of these common axis titles as well
  theme_minimal(
    axis.title.x = element_text(size = 18, margin = margin(t = 10)), # Increased common X axis title size, added margin
    axis.title.y = element_text(size = 18, margin = margin(r = 10))  # Increased common Y axis title size, added margin
  )
final_combined_plot

# -----------------------------
# Export the combined plot
# -----------------------------
ggsave(
  filename = here("output", "figures", "combined_baseline_scenarios.png"),
  plot = final_combined_plot,
  width = 15, # Adjust width as needed for a three-panel plot
  height = 6, # Adjust height as needed
  dpi = 300
)

#-----------------------------
# Export plots
#-----------------------------

# save the test habitat creation/restoration/averted loss plots
# Save Habitat Creation plot
ggsave(
  filename = here("output", "figures", "habitat_creation.png"),
  plot = plot_hc,
  width = 8, height = 5, dpi = 300
)

# Save Habitat Restoration plot
ggsave(
  filename = here("output", "figures", "habitat_restoration.png"),
  plot = plot_hr,
  width = 8, height = 5, dpi = 300
)

# Save Avoided Loss plot
ggsave(
  filename = here("output", "figures", "avoided_loss.png"),
  plot = plot_al,
  width = 8, height = 5, dpi = 300
)


# # Export individual scenario plots (highlighted)
# walk(unique(df_all$scenario), function(s) {
#   plot <- plot_offset_trajectories(df_all, log_scale = TRUE, focal = s, show_title = FALSE)
#   file_name <- paste0("highlight_", gsub("[^a-zA-Z0-9]", "_", s), ".png")
#   ggsave(here("output", "figures", file_name), plot, width = 8, height = 5, dpi = 300)
# })
# 
# # Export combined plots
# ggsave(here("output", "figures", "all_scenarios_log.png"),
#        plot_offset_trajectories(df_all, log_scale = TRUE, show_title = FALSE),
#        width = 8, height = 5, dpi = 300)
# 
# ggsave(here("output", "figures", "all_scenarios_linear.png"),
#        plot_offset_trajectories(df_all, log_scale = FALSE, show_title = FALSE),
#        width = 8, height = 5, dpi = 300)