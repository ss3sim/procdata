### This simulation explores the interaction of process error and data
### weighting for the likelihood components. Started 9/2015. -Cole

### ------------------------------------------------------------
## Step 0: Prepare the R workspace and generate case files
## devtools::install_github("ss3sim/ss3sim")
## devtools::install_github('r4ss/r4ss')
cores <- 18   # parallel cores
Nsim <- 100                             # simulation replicates
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
scenarios.cod <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='cod')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.cod,
           bias_adjust=TRUE, bias_nsim=Nbias, hess_always=TRUE,
           parallel=TRUE, parallel_iterations=FALSE,
           case_folder=case_folder, om_dir=om.paths['cod'],
           em_dir=em.paths['cod'], case_files=case_files)
scenarios.codtv <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='codtv')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.codtv,
           bias_adjust=TRUE, bias_nsim=Nbias, hess_always=TRUE,
           parallel=TRUE, parallel_iterations=FALSE,
           case_folder=case_folder, om_dir=om.paths['codtv'],
           em_dir=em.paths['codtv'], case_files=case_files)
scenarios.codtvx <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='codtvx')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.codtvx,
           bias_adjust=TRUE, bias_nsim=Nbias, hess_always=TRUE,
           parallel=TRUE, parallel_iterations=FALSE,
           case_folder=case_folder, om_dir=om.paths['codtvx'],
           em_dir=em.paths['codtvx'], case_files=case_files)
## Read in results for both species and save them to file
get_results_all(user=c(scenarios.codtvx, scenarios.codtv, scenarios.cod),
                parallel=TRUE)
xx <- calculate_re(read.csv("ss3sim_scalar.csv"))
saveRDS(xx, file='results/scalars.RData')
yy <- calculate_re(read.csv("ss3sim_ts.csv"))
saveRDS(yy, file='results/ts.RData')
## Save an OM r4ss output list which is used in some plots
replist <- SS_output(dir='D1-E0-F1-S1-cod/1/om/', covar=FALSE, ncols=250)
saveRDS(out, file='results/replist.RDS')
### ------------------------------------------------------------

### ------------------------------------------------------------
## Step 2: Make plots and figures
source('load_results.R')
source('make_plots.R')
source('make_figures.R')

## Tables
table.long <-
  ddply(xx, .(om.process, em.process, estimated, weighted),
        summarize, count=length(hessian), converged.pct=sum(hessian),
        MARE.ssb=round(median(abs(SSB_MSY_re)),2))
table.MSY.mare <- dcast(table.long, om.process+em.process+estimated~weighted, value.var='MARE.ssb')
write.table(table.MSY.mare,'results/table.MSY.mare.csv', sep=',', row.names=FALSE)
table.converged <- dcast(table.long, om.process+em.process+estimated~weighted, value.var='converged.pct')
write.table(table.converged,'results/table.converged.csv', sep=',', row.names=FALSE)
