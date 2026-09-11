# Deconstructing the Livestock Manure Digester and Biogas Controversy

This repository is the public data/code companion to reproduce Figures 2 and 3 and Supplemental Figures S1–S3 from:

Wainer, A., Love, D.C., Kim, B.F., Harding, J., Lyu, Q., Williams, D.A.L., Heaney, C.D., Hobbs, B.F. and Nachman, K.E., 2025. Deconstructing the Livestock Manure Digester and Biogas Controversy. *Current Environmental Health Reports*, 12(1), p.43.

DOI: https://doi.org/10.1007/s40572-025-00512-8

## Repository Structure

    biogas/
        agstar-livestock-ad-database_june2024.csv
        agstar-livestock-ad-database_shutdown_june2024.csv
        epa_2022_ag_emissions.csv
        figure2.R
        figure3.R
        figureS1.R
        figureS2.R
        figureS3.R

## Data Sources

**EPA AgSTAR Livestock Anaerobic Digester Database**
`agstar-livestock-ad-database_june2024.csv` and `agstar-livestock-ad-database_shutdown_june2024.csv` are derived from the EPA AgSTAR Livestock Anaerobic Digester Database (accessed 26 Feb 2025):
https://www.epa.gov/agstar/livestock-anaerobic-digester-database

**EPA Inventory of U.S. Greenhouse Gas Emissions and Sinks (2024)**
`epa_2022_ag_emissions.csv` is transcribed from Table 5-2 (Chapter 5, Agriculture) of the EPA's 2024 GHG inventory:
https://www.epa.gov/system/files/documents/2024-04/us-ghg-inventory-2024-chapter-5-agriculture.pdf

Full inventory report: https://www.epa.gov/system/files/documents/2024-04/us-ghg-inventory-2024-main-text_04-18-2024.pdf

## Scripts

### figure2.R

Reproduces Figure 2: "Number of anaerobic manure digesters at livestock operations in the United States, by animal type."

- Original source of code: https://asmith.ucdavis.edu/news/cow-power-rising
- Reads `agstar-livestock-ad-database_june2024.csv` directly from this repository's raw GitHub URL.
- Recodes `Animal.Farm.Type.s.` into five categories: Poultry, Beef cattle, Mixed, Dairy cows, Swine.
- Filters to digesters with `Year.Operational` between 2000 and 2024.
- Computes the cumulative number of operating digesters per animal type per year (`csum_n`).
- Plots a stacked area chart of cumulative digester counts over time, filled by animal type, using the "PuRd" color palette.
- Underlying numerical data correspond to Supplemental Excel Table S2 in the manuscript.

Packages used: dplyr, ggplot2, forcats, tidyr, patchwork

### figure3.R

Reproduces Figure 3: "Agriculture-related greenhouse gas emissions in the United States in 2022, by source and gas."

- Data source: Table 5-2 of the EPA 2024 GHG Inventory, Chapter 5 (Agriculture).
- Reads `epa_2022_ag_emissions.csv` directly from this repository's raw GitHub URL.
- Orders emissions gas (N2O, CO2, CH4) and source category by descending total emissions.
- Plots a stacked bar chart of emissions (MMT CO2 eq) by source, filled by gas, using the "PuRd" color palette.
- Underlying numerical data correspond to Supplemental Excel Table S3 in the manuscript.

Packages used: ggplot2, dplyr, forcats

### figureS1.R

Reproduces Supplemental Figure S1, showing newly operational, under-construction, and shut-down digesters by year.

- Original source of code: https://asmith.ucdavis.edu/news/cow-power-rising
- Reads both `agstar-livestock-ad-database_june2024.csv` and `agstar-livestock-ad-database_shutdown_june2024.csv` directly from raw GitHub URLs.
- Aggregates counts of operational vs. shut-down digesters by year and status; shut-down counts are made negative so they plot below the x-axis.
- Recodes status into: "Newly operational", "Under construction", "Shut down".
- Plots a stacked bar chart with a dashed horizontal reference line at y = 0, using the "OrRd" color palette.

Packages used: dplyr, ggplot2, forcats, tidyr, patchwork

### figureS2.R

Reproduces Supplemental Figure S2, a three-panel breakdown of dairy digesters in key states (California, New York, Pennsylvania, Wisconsin).

- Original source of code: https://asmith.ucdavis.edu/news/cow-power-rising
- Reads `agstar-livestock-ad-database_june2024.csv` directly from the raw GitHub URL.
- Defines `key_states <- c("CA", "NY", "PA", "WI")`.
- Builds three sub-plots combined with patchwork:
  - Panel A: histogram of dairy digesters by year operational (pre-2000 records bucketed together), colored by state.
  - Panel B: histogram of dairy digesters by biogas end use (recoded into CNG, Cogeneration, Electricity, Mixed, Flared Full-time, Boiler/Furnace fuel, and Pipeline-to-Electricity merged into Electricity), colored by state.
  - Panel C: histogram of dairy digesters by digester type (recoded into Plug flow, Complete mix, Covered lagoon, Other), colored by state.
- Final layout: `p1 / (p2 + p3) + plot_layout(guides = "collect", heights = c(2, 1))`.
- Uses the "GnBu" color palette throughout.

Packages used: ggplot2, dplyr, forcats, patchwork

### figureS3.R

Prepares the underlying summary data for Supplemental Figure S3 (a U.S. map of digesters by animal type).

- Reads `agstar-livestock-ad-database_june2024.csv` directly from the raw GitHub URL.
- Uses the same animal-type recoding as `figure2.R`.
- Filters to digesters with `Year.Operational` before 2025 or missing.
- Selects City, County, State, `Animal.Farm.Type.s.`, and `Year.Operational`.
- Groups by animal type and summarizes counts.
- Note: this script only generates the summary counts used for the map. The map itself was produced externally in ArcGIS and is **not** reproducible from this script alone (see the `#plot made in ArcGIS.` comment in the script).

Packages used: dplyr, ggplot2, forcats, tidyr, patchwork

## Requirements

- R (≥ 4.0 recommended)
- R packages: `dplyr`, `ggplot2`, `forcats`, `tidyr`, `patchwork`

Install with:

    install.packages(c("dplyr", "ggplot2", "forcats", "tidyr", "patchwork"))

## Usage

Each script reads its required CSV input(s) directly from this repository's raw GitHub URLs (e.g., `https://raw.githubusercontent.com/dave-love/biogas/main/agstar-livestock-ad-database_june2024.csv`), so no manual download of the data files is necessary. Simply clone or download this repository and run any script in R:

    source("figure2.R")
    source("figure3.R")
    source("figureS1.R")
    source("figureS2.R")
    source("figureS3.R")

Note that `figureS3.R` only produces the tabular summary used to build the map; recreating the final map graphic requires importing that summary into ArcGIS or a similar GIS tool.

## Acknowledgements

The code in `figure2.R`, `figureS1.R`, and `figureS2.R` is adapted from sample R code originally written by Aaron Smith, University of California, Davis, for his "Cow Power Rising" analysis (https://asmith.ucdavis.edu/news/cow-power-rising). We thank him for sharing this code, as acknowledged in the manuscript.


## Contact for more information

Dave Love, PhD, MSPH  
Research Professor  
Johns Hopkins Center for a Livable Future  
Department of Environmental Health and Engineering  
Johns Hopkins Bloomberg School of Public Health  
dlove8@jhu.edu
