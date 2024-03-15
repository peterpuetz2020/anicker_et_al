## Overview
This repository allows to replicate the results obtained in "Anicker et al. (2024) - Do Biospheric Values Really Moderate the Impact of Informational Interventions on Pro-Environmental Behavior Intentions?" The analyses were conducted using R 4.2.2 (64 bit, Windows). 

## Structure of this repository
* `data`: This folder contains the datasets needed for the analyses and the codebooks explaining the respective variables in the datasets.
* `R`: This folders contains all R scripts needed to replicate the results.
* `results`: This folder contains the figure and the results of the simulations that can be reproduced by running the respective R scripts.
* `supplement`: This folder contains the supplementary material as explained in the manuscript.

## How to replicate the results
If you use RStudio, open the `R` project 
`anicker_et_al.Rproj` in the main directory first, then all R scripts loaded into this project should run as they are. If you do not use RStudio, you have to set your working directory at the beginning of each R script (`setwd(…)`) to the directory where the folders “data” and “results” are located.

You can run the script
* `study1_tables_and_figure.R` to obtain the figure and tables in Study 1 
* `study1_sensitivity_analyses.R` to obtain the results for the sensitivity analyses in Study 1 
* `study2_tables.R` to obtain the tables in Study 2 
* `study2_sensitivity_analyses.R` to obtain the results for the sensitivity analyses in Study 2

The script `functions.R` is called by other scripts (make sure that the conditions on the working directory as explained above hold) and need not be called separately.