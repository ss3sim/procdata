### This simulation explores the interaction of process error and data
### weighting for the likelihood components. Started 9/2015. -Cole

### ------------------------------------------------------------
## Step 0: Prepare the R workspace and generate case files
## devtools::install_github("ss3sim/ss3sim")
## devtools::install_github('r4ss/r4ss')
cores <- 18   # parallel cores
Nsim <- 300                             # simulation replicates
Nbias <- 10                             # bias adjustment runs
## scalars used to control ESS in the data case files, 1 means true ESS
ESS.scalar.vec <- c(.1, 1, 10)
## Scalar multiplier for the selectivity trend
selex.scalar.vec <- c(0,20)
## The ESS.scalar.vec controls the OM dynamically. The EM is controlled via
## models. cod is base case (no process error); codtv is the EM with
## process error estimated at sigma .48 and codtvx is identical except
## sigma is .96.
species <- c('cod', 'codtv', 'codtvx')
## F is fishing pattern; S is selectivity of the OM; E is estimation (M and
## h)
case_files <- list(F="F", D=c("index","lcomp","agecomp"), S='S', E='E')
source("startup.R")
### ------------------------------------------------------------

### ------------------------------------------------------------
### Step 1: Run the simulation and process the results.
## First do deterministic runs to ensure everything is working right.  See
## file plots/deterministic/make_plots_deterministic.R to make the plots if
## this is rerun
## source('run_deterministic.R')
## Now run the real simulations
## source('run_scenarios.R')
### ------------------------------------------------------------

### ------------------------------------------------------------
## Step 2: Make plots and figures
source('load_results.R')                # load and process results
source('make_plots.R')                  # not in paper
source('make_figures.R')                # in paper
source('make_tables.R')                 # written to results folder


