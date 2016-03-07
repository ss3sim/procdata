### This simulation explores the interaction of process error and data
### weighting for the likelihood components. Started 9/2015. -Cole

### ------------------------------------------------------------
## Step 0: Prepare the R workspace and generate case files
cores <- 8   # parallel cores
## devtools::install_github("ss3sim/ss3sim")
## devtools::install_github('r4ss/r4ss')
Nsim <- 200
## scalars used to control ESS in the data case files, 1 means true ESS
ESS.scalar.vec <- c(.1, 1, 10)
selex.scalar.vec <- c(0,20)
species <- c('cod', 'codtv', 'codtvx')
case_files <- list(F="F", D=c("index","lcomp","agecomp"), S='S', E='E')
source("startup.R")
### ------------------------------------------------------------

### ------------------------------------------------------------
## Step 1: Run the simulation and process the results
scenarios.cod <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='cod')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.cod,
           parallel=TRUE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths['cod'],
           em_dir=em.paths['cod'], case_files=case_files)
scenarios.codtv <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='codtv')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.codtv,
           parallel=TRUE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths['codtv'],
           em_dir=em.paths['codtv'], case_files=case_files)
scenarios.codtvx <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='codtvx')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.codtvx,
           parallel=TRUE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths['codtvx'],
           em_dir=em.paths['codtvx'], case_files=case_files)
## Read in results for both species and save them to file
get_results_all(user=c(scenarios.codtvx, scenarios.codtv, scenarios.cod),
                parallel=TRUE)
xx <- calculate_re(read.csv("ss3sim_scalar.csv"))
saveRDS(xx, file='results/scalars.RData')
yy <- calculate_re(read.csv("ss3sim_ts.csv"))
saveRDS(yy, file='results/ts.RData')

### ------------------------------------------------------------

### ------------------------------------------------------------
## Step 2: Make plots and figures
source('make_plots.R')



## see file plots/deterministic/make_plots_deterministic.R to make the
## plots if this is rerun...
source('run_deterministic.R')
