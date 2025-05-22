# ===============================================
# Script:     20250112_OffsetPermRev_MetagearScreening.r
# Date:       2025-01-12
# Author:     Alex Dhond
# Purpose:    Launch metagear abstract screening GUI for AKD
# ===============================================

# ---------------------------
# 1. Load packages
# ---------------------------
library(metagear) # Screening tool for systematic reviews
library(here)  # for consistent project-based paths

# ---------------------------
# 2. Define input file path
# ---------------------------

# take input from the previous output folder
input_file <- here("output", "effort_AKD.csv")

# ---------------------------
# 3. Launch metagear GUI
# ---------------------------
abstract_screener(
  file        = input_file,
  aReviewer   = "AKD",
  highlightColor = "powderblue",
  highlightKeywords = c(
    "biodiversity", "carbon", "biodiversity offset",
    "wetland mitigation", "wetland", "carbon offset",
    "habitat", "offset"
  )
)
