# Offset Permanence Review

Code and data for: *Permanence Risks to Biodiversity and Nature-based Carbon Offsets*.

## Repository Structure

```
offset-permanence-review/
├── code/
│   ├── 01_screening/          # Literature screening (metagear)
│   ├── 02_data_prep/          # Data pipeline (scripts 01–17)
│   │   └── 00_run_all.R       # Runs all pipeline scripts in sequence
│   ├── 03_analysis/           # Quarto analysis documents
│   └── helpers/               # Shared utilities (data loading, palettes)
├── data/
│   ├── raw/                   # Source database (.xlsx)
│   ├── lookups/               # Reference tables
│   ├── cleaned/               # Intermediate cleaned data
│   └── final/                 # Analysis-ready dataset
├── shiny-app/                 # Interactive web application
├── output/
│   ├── figures/main/          # Manuscript figures
│   └── figures/supplementary/ # Supplementary figures
└── renv.lock
```

## Brief Analysis Overview

1. We screened the literature and binned studies into yes, maybe, or no (`code/01_screening/`).
2. We extracted data from the included studies and recorded it in a single Excel database (`data/raw/`); the raw database is not provided as it is messy.
3. To clean up the raw database into an analysis-ready dataset, we created a pipeline of R scripts (`code/02_data_prep/`) that builds reference lookup tables, cleans each data domain (country, species, ecosystem, project type, delivery mechanism, program, policy, and permanence risk), and joins them into one long-format dataset of 137 studies, which is provided in (`data/final/`).
4. We analysed this dataset and produced all manuscript and supplementary figures using Quarto documents (`code/03_analysis/`).

## Interactive Database

An interactive database explorer is available at [alexdhond.shinyapps.io/offset-permanence-database](https://alexdhond.shinyapps.io/offset-permanence-database/).

## Contact

For any questions or concerns about the code or files, please contact: 

Alexander Dhond — [alexander.dhond@biology.ox.ac.uk]

## License

[MIT License](LICENSE)
