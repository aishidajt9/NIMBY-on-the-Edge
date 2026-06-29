# NIMBY on the Edge: A One-dimensional Model of NIMBY Facility Siting

Replication code and data for:

> Ishida, A. & Maeda, Y. (forthcoming). NIMBY on the Edge: A One-dimensional Model of NIMBY Facility Siting. *Journal of Mathematical Sociology*.

## Files

### R code (`R/`)

| File | Description | Figures |
|------|-------------|---------|
| `fig_sec2_model.R` | Numerical examples for Section 2 (utility functions, distributions, social welfare) | Figures 1--5 |
| `fig_sec3_siting_feasibility.R` | Numerical analysis for Section 3 (composite objective function, optimal location trajectories) | Figures 6--9 |
| `fig_sec4_empirical.R` | Empirical analysis for Section 4 (MSW facility locations in Japan) | Figures 10--13, Table 1 |

### Data (`R/`)

| File | Description |
|------|-------------|
| `DATA.rds` | Processed facility-level data for empirical analysis (N = 1253) |

`DATA.rds` contains the following variables:

- `ID`: Facility ID
- `Year`: Year of operation start (2000--2012)
- `Municipality`: Municipal code (5-digit JIS code, stored as numeric)
- `Distance`: Distance to municipal boundary (meters)
- `Cross`: Inter-municipal facility flag
- `RelDens`: Relative density (facility block density / municipal average density)
- `N`: Number of census blocks in the municipality
- `geometry`: Facility location (sf point, WGS84)

## Requirements

- R >= 4.1.0
- tidyverse >= 2.0.0
- statmod >= 1.5.0
- patchwork >= 1.1.0
- sf >= 1.0.0 (for empirical figures)
- ggspatial >= 1.1.0 (for map figures)

## Usage

Run from the repository root:

```r
# Section 2 figures
source("R/fig_sec2_model.R")
generate_all_figures()

# Section 3 figures
source("R/fig_sec3_siting_feasibility.R")
generate_section3_figures()

# Section 4 figures (Figures 12-13 and Table 1)
source("R/fig_sec4_empirical.R")
generate_section4_figures()
```

Figures 10--11 (maps of Tokyo and Osaka) require census shapefiles that must be downloaded separately from [e-Stat](https://www.e-stat.go.jp/) and [MLIT](https://nlftp.mlit.go.jp/). See comments in `fig_sec4_empirical.R` for details.

## Data sources

- Municipal Waste Management Facility Data: https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P15.html
- Municipal Waste Management Facility Survey 2012: https://www.env.go.jp/recycle/waste_tech/ippan/h24/index.html
- Municipality Boundary Data: https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-2015.html
- National Census data: https://www.e-stat.go.jp/

## License

MIT
