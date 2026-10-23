# Peatland fire selectivity: 2023 patterns

Code and data for:

Goldman, JA; Pelletier, N; Thompson, DK; Pontone N; Wilkinson, SL, Guindon, L; Millard, K. Peatlands burn preferentially to uplands during severe summertime drought: empirical evidence from 2023 Canadian wildfires


This repository calculates whether fires in the 2023 Canadian fire season selected for or avoided peatland classes, compared with uplands. Selectivity is measured with Jacobs' index for each daily fire-progression polygon. The analysis then models selectivity against peatland class, canopy structure, fire weather (BUI, ISI), season and region.

## Repository structure

| Folder              | Contents                                                                                                                                                                                                |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `analysis/`       | Mixed models and contrasts (`statistical_analysis.R`), summary statistics (`summary_stats.R`), and selectivity over the season (`time.R`)                                                         |
| `plots/`          | Scripts for the figures, plus the rendered figures (`main/`, `supplementary/`, `map/`)                                                                                                            |
| `data/`           | The selectivity dataset used in the analysis (`Dataset_selectivity_jg_0617.csv`),  and upland species totals (`upland_lc/`)                                                                        |
| `results/tables/` | Tables  and supplementary tables                                                                                                                                                                      |
|                     |                                                                                                                                                                                                         |

## Reproducing the analysis

The statistical analysis and most figures run from the included data:

1. Open R in the repository root (paths use the `here` package).
2. Run the scripts in `analysis/`, then the plotting scripts in `plots/`.

The scripts in `plots/map/` also need large spatial inputs that are not included here: the peatland map raster, NBAC fire-progression polygons, dNBR rasters. Their file paths are set at the top of each script and must be changed to match your copies. The final selectivity dataset produced by this pipeline is provided in `data/`.

## R packages

dplyr, emmeans, foreach, doParallel, ggplot2, gt, here, lme4, maptiles, patchwork, raster, readr, rnaturalearth, sf, svglite, terra, tidyterra, tidyverse, treemapify, viridisLite

## License

[To be added]
