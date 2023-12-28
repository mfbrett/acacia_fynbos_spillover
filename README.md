# Overview

This repository contains all the R script and raw data required to reproduce the analyses and figures included in the following manuscript:

## Spillover effects from invasive Acacia alter the plant-pollinator networks and seed production of native plants. 

*Manuscript authors:* Maisie F. Brett | Paula Strauss | Kurt van Wyk |  Ian P. Vaughan | Jane Memmott

*Author of R script:* Maisie F. Brett

[Download][1] the data and script files, then run the scripts using [R][2] or [R Studio][3].

[1]: https://github.com/mfbrett/acacia_spillover/blob/main/Master.zip
[2]: https://www.r-project.org/
[3]: https://www.rstudio.com/products/rstudio/download/

# Scripts

All the R scripts for data analysis and figures are located in the `scripts/` folder of this repository.

## a) Data analysis

*Acacia flower counts*: to reproduce the regression lines used to predict acacia flower densities at sites, use `acaciaregression_.r`

*For analyses regarding the visitation to and seed set of 3 focal plant species*: use `focals_analysis_.R`

*For all other analyses*: including visitation networks, use `main_analysis_.r`

## b) Main text Figures

*Figure 3: Visitation network analyses*: The code to produce the panels in Figure 3 can be found in `main_analysis_.r`

*Figure 4: Focal species analyses*: The code to produce the panels in Figure 4 can be found in `focals_.r`

## c) Supplimentary material figures

*Figure S1: Site Map*: The script concerning map plotting is `sitemap_.r`

*Figure S3: Visitation networks*: Plant-insect visitation networks: `webillustration_.r`
