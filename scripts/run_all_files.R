# Load the here package
library(here)

# Define the path to the scripts folder
script_dir <- here("scripts")

# List all .R files in that folder
all_scripts <- list.files(path = script_dir, pattern = "\\.R$", full.names = TRUE)

# Filter for scripts numbered 01 to 18
target_scripts <- grep("^.*/(0[1-9]|1[0-8])_.*\\.R$", all_scripts, value = TRUE)

# Sort the scripts
target_scripts <- sort(target_scripts)

# Run each script
for (script in target_scripts) {
  cat("Running:", basename(script), "\n")
  source(script)
}
