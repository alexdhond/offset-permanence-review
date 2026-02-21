# Load the here package
library(here)

# Define the path to the data prep scripts folder
script_dir <- here("code", "02_data_prep")

# List all .R files in that folder
all_scripts <- list.files(path = script_dir, pattern = "\\.R$", full.names = TRUE)

# Filter for scripts numbered 01 to 18 (exclude 00_run_all.R and any temp files)
target_scripts <- all_scripts[grepl("^(0[1-9]|1[0-8])_", basename(all_scripts))]

# Sort the scripts
target_scripts <- sort(target_scripts)

# Run each script
for (script in target_scripts) {
  cat("Running:", basename(script), "\n")
  source(script)
}
