# Contents
This repository contains full documentation for the paper:

> Wolfe, C.A. (In Review). Beyond chronological age: Modeling human growth trajectories using dental development stages. American Journal of Biological Anthropology.

To run the code for this paper locally, please ensure that [R](https://www.r-project.org/) and optionally, [RStudio](https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads), are installed. Note, for full recreation with the documents here, users must also utilize the [Stan Programming Language](https://mc-stan.org/) and optionally, the [cmdstanr](https://mc-stan.org/cmdstanr/index.html) interface. 

### How to Cite
If using the code in this repository for one's own work, please cite this compendium as:

> Wolfe, C.A.

## The Data
All data are from the [Subadult Virtual Anthropology Database](https://zenodo.org/communities/svad/records?q=&l=list&p=1&s=10&sort=newest). Specifically, this project uses portions of the data from the [United States](https://zenodo.org/records/5193208), [South Africa](https://zenodo.org/records/3950301), and [Colombia](https://zenodo.org/records/7668554). Note, Figure 1 does utilize data from the Harpenden Growth dataset (Tanner 1981; Johnson 2024).

## The Files
All code to recreate the results, figures, and tables can be found in the `code` folder. For ease, each individual part of the analysis is encapuslated in a single .R file. 

1. [model_fit.R](https://github.com/ChristopherAWolfe/Wolfe_MonotonicGrowth_AJBA/blob/main/code/model_fit.R) imports all SVAD data and fits the requisite growth models.
2. [Figure1.R](https://github.com/ChristopherAWolfe/Wolfe_MonotonicGrowth_AJBA/blob/main/code/Figure1.R) recreates Figure 1. Note, the Harpenden dataset is required for full reproducibility.
3. [Figure2.R](https://github.com/ChristopherAWolfe/Wolfe_MonotonicGrowth_AJBA/blob/main/code/Figure2.R) recreates Figure 2.
4. [Figure3.R](https://github.com/ChristopherAWolfe/Wolfe_MonotonicGrowth_AJBA/blob/main/code/Figure3.R) recreates Figure 3.
5. [Figure4.R](https://github.com/ChristopherAWolfe/Wolfe_MonotonicGrowth_AJBA/blob/main/code/Figure4.R) recreates Figure 4.
6. [Figure5.R](https://github.com/ChristopherAWolfe/Wolfe_MonotonicGrowth_AJBA/blob/main/code/Figure5.R) recreates Figure 5.
7. [Figure6.R](https://github.com/ChristopherAWolfe/Wolfe_MonotonicGrowth_AJBA/blob/main/code/Figure6.R) recreates Figure 6. Note, the [errors_in_variables_growth.stan] model is required for this step.
8. [tables.R](https://github.com/ChristopherAWolfe/Wolfe_MonotonicGrowth_AJBA/blob/main/code/tables.R) recreates Tables 2-5 from the text.

Note, the [Supporting Information](https://github.com/ChristopherAWolfe/Wolfe_MonotonicGrowth_AJBA/blob/main/SI_Submission1.pdf) file is also included for full model transparency and description.

## Timeline

All associated code and information is up to date as of the most recent date below. It is likely some files may change or be modified as the paper goes through the review process. 

-  First Submission to AJBA: June 2026
