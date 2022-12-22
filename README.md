## Overview
This repository allows to replicate the sensitivity analyses conducted in "Anicker et al. (2022) - Do Biospheric Values Really Moderate the Impact of Informational Interventions on Pro-Environmental Behavior Intentions?" The analyses were conducted using R 4.2.2 (64 bit, Windows). 

## Structure of this repository
* `data`: This folder contains the datasets needed for the sensitivity analyses.
* `R`: This folders contains all R scripts needed to replicate the results.
* `results`: This folder contains the results of the simulations that can be reproduced by running the R script

## How to replicate the results
If you use RStudio, open the `R` project 
`anicker_et_al.Rproj` first, then all R scripts loaded into this project should run as they are. If you do not use RStudio, you have to set your working directory at the beginning of each R script (`setwd(…)`) to the directory where the folders “data” and “results” are located.

Run the R scripts (stored in folder `R`) `study1_sensitivity_analyses.R` to obtain the results for the sensitivity analyses in Study 1 and `study2_sensitivity_analyses.R` to obtain the results for the sensitivity analyses in Study 2.
