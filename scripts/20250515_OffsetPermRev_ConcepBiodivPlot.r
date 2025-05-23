# =============================
# Biodiversity Offset Trajectory Plot Script
# =============================

# Load packages
library(ggplot2)   # plotting
library(dplyr)     # data manipulation
library(scales)    # scaling axes
library(purrr)     # iteration over scenarios
library(here)      # relative file paths

#-----------------------------
# Global settings
#-----------------------------

# Time vector
time <- seq(0, 100, by = 0.1)

# Logistic growth function
s_sigmoid <- function(t, K = 100, r = 0.3, t0 = 15) {
  K / (1 + exp(-r * (t - t0)))
}

#-----------------------------
# Generate scenario trajectories
#-----------------------------
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

#-----------------------------
# Custom scenarios with multiple types
#-----------------------------

# Avoided Loss: actual vs. counterfactual
avoided_loss_actual <- generate_trajectory("Avoided Loss", function(t) {
  90 + (10 / max(t)) * t  # linear increase from 90 to 100 over 100 time units
}) %>%
  mutate(type = "Actual")

avoided_loss_counterfactual <- generate_trajectory("Avoided Loss", function(t) {
  pmax(0, 90 - 0.8 * t)
}) %>%
  mutate(type = "Counterfactual")

scenario_list <- list(
  
  # Habitat creation: starts at 0, rises to a cap
  generate_trajectory("Habitat Creation", function(t) {
    pmin(s_sigmoid(t, K = 100, r = 0.2, t0 = 20), 100)
  }),
  
  # Habitat restoration: starts above 0 (e.g., 30), and rises toward a higher asymptote
  generate_trajectory("Habitat Restoration", function(t) {
    base <- 30
    increment <- s_sigmoid(t, K = 70, r = 0.2, t0 = 20)
    pmin(base + increment, 100)
  }),

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

# Tag all with type actual
scenario_list <- map(scenario_list, ~ mutate(.x, type = "Actual"))

# Combine all scenarios into one dataframe
df_all <- bind_rows(scenario_list, avoided_loss_actual, avoided_loss_counterfactual)

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
plot_offset_trajectories <- function(df, log_scale = TRUE, facet = FALSE, focal = NULL, show_title = TRUE) {
  scenarios <- unique(df$scenario)
  color_vals <- if (!is.null(focal)) setNames(ifelse(scenarios == focal, offset_colors[focal], "gray70"), scenarios) else offset_colors
  alpha_vals <- if (!is.null(focal)) setNames(ifelse(scenarios == focal, 1, 0.3), scenarios) else setNames(rep(1, length(scenarios)), scenarios)
  linewidth_vals <- if (!is.null(focal)) setNames(ifelse(scenarios == focal, 1.6, 0.5), scenarios) else setNames(rep(1.2, length(scenarios)), scenarios)
  
  p <- ggplot(df, aes(x = time, y = biodiversity, color = scenario, alpha = scenario, linewidth = scenario, linetype = type)) +
    geom_line(show.legend = FALSE) +
    scale_color_manual(values = color_vals) +
    scale_alpha_manual(values = alpha_vals) +
    scale_linewidth_manual(values = linewidth_vals) +
    scale_linetype_manual(values = c("Actual" = "solid", "Counterfactual" = "dotted"), guide = "none") +
    labs(
      title = if (show_title) {
        if (!is.null(focal)) focal else if (log_scale) "Offset Trajectories (Log Time)" else "Offset Trajectories"
      } else NULL,
      x = if (log_scale) "Time (log scale)" else "Time",
      y = "Relative Biodiversity"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position = if (facet) "none" else "bottom"
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

#-----------------------------
# Export plots
#-----------------------------

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