# Offset Permanence Review

Code and data for: *Permanence Risks to Biodiversity and Nature-based Carbon Offsets*.

This repository contains the data pipeline, analysis scripts, and interactive Shiny app for a systematic review of permanence risks across 137 peer-reviewed studies of biodiversity and carbon offset programs.

## Repository Structure

```
offset-permanence-review/
├── code/
│   ├── 01_screening/          # Literature screening (metagear)
│   ├── 02_data_prep/          # Data pipeline (scripts 01–18)
│   │   └── 00_run_all.R       # Runs scripts 01–17 in sequence
│   ├── 03_analysis/           # Quarto analysis documents
│   │   ├── permanence_risk_analysis.qmd
│   │   ├── publication_figures.qmd
│   │   └── supp_info_figures.qmd
│   └── helpers/               # Shared utilities (data loading, palettes, etc.)
├── data/
│   ├── raw/                   # Source database (.xlsx)
│   ├── lookups/               # Reference tables
│   ├── cleaned/               # Intermediate cleaned data
│   └── final/                 # Processed datasets
├── shiny-app/                 # Interactive web application
├── output/
│   ├── figures/main/          # Manuscript figures (600 dpi)
│   └── figures/supplementary/ # Supplementary figures (300 dpi)
├── _quarto.yml
├── references.bib
└── renv.lock
```

## Reproducing the Analysis

**Requirements:** R (>= 4.5.0) and [Quarto](https://quarto.org/).

1. Clone the repository and open `offset-permanence-review.Rproj` in RStudio.

2. Restore package dependencies:
   ```r
   renv::restore()
   ```

3. Run the data pipeline:
   ```r
   source("code/02_data_prep/00_run_all.R")
   ```
   This processes the raw database (`data/raw/offset_perm_rev_database.xlsx`) through 17 scripts that build lookup tables, clean each data domain, and join everything into a single long-format dataset: `data/final/offset_perm_rev_long_cleaned.csv` (~682K rows, 137 studies, 39 columns).

4. Render the analysis documents:
   ```r
   quarto::quarto_render("code/03_analysis/permanence_risk_analysis.qmd")
   quarto::quarto_render("code/03_analysis/publication_figures.qmd")
   quarto::quarto_render("code/03_analysis/supp_info_figures.qmd")
   ```

Optionally, create a condensed one-row-per-study summary:
```r
source("code/02_data_prep/18_create_condensed_database.R")
```

## Figure Outputs

| Document | Output | Format |
|----------|--------|--------|
| `permanence_risk_analysis.qmd` | `output/figures/main/` | PNG (600 dpi) + SVG |
| `publication_figures.qmd` | `output/figures/main/` | PNG (600 dpi) + SVG |
| `supp_info_figures.qmd` | `output/figures/supplementary/` | PNG (300 dpi) |

## Shiny App

An interactive database explorer is deployed at [alexdhond.shinyapps.io/offset-permanence-database](https://alexdhond.shinyapps.io/offset-permanence-database/). To update it with the latest pipeline output, run `source("shiny-app/refresh_data.R")`. The app has its own `renv` environment.

## Troubleshooting

- **Figures not rendering?** Check that `code/helpers/load_data.R` ran successfully and `final_df` loaded.
- **Slow world map?** Delete `cache/world_map_cached.rds` and re-render — it regenerates automatically.
- **Cross-references broken?** Chunk labels must use `#| label: fig-name` (not `fig.name` or `fig_name`).

## Author

Alexander Dhond — Department of Biology, University of Oxford

## License

[MIT License](LICENSE)
