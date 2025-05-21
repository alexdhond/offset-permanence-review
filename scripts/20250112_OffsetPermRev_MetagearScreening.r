# 2025-01-12 Metagear Screening Initialization
# Alex Dhond

# This script is used to initialize Metagear
# screening GUI for the reviewer (Alex) to begin the screening process.

# Load necessary libraries
library(metagear)

# Initialize Metagear screening GUI
abstract_screener("effort_AKD.csv", aReviewer = "AKD", highlightColor = "powderblue", highlightKeywords = c("biodiversity", "carbon", "biodiversity offset", "wetland mitigation", "wetland", "carbon offset", "habitat", "offset"))
