### This simulation explores the interaction of process error and data
### weighting for the likelihood components. Started 9/2015. -Cole

### ------------------------------------------------------------
## Step 0: Prepare the R workspace and generate case files
cores <- 4   # parallel cores
## devtools::install("c:/Users/Cole/ss3sim")
## devtools::install_github('r4ss/r4ss')
Nsim <- 1

## scalars used to control ESS in the data case files, 1 means true ESS
ESS.scalar.vec <- sort(unique(c(1,exp(seq(log(.1),log(10), len=10)))))
selex.scalar.vec <- c(0,20)
species <- c('cod', 'codtv')
case_files <- list(F="F", D=c("index","lcomp","agecomp"), S='S', E='E')
source("startup.R")
### ------------------------------------------------------------

### ------------------------------------------------------------
## Run the simulation for the two species
scenarios.cod <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='cod')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.cod,
           parallel=TRUE, parallel_iterations=FALSE,
           case_folder=case_folder, om_dir=om.paths['cod'],
           em_dir=em.paths['cod'], case_files=case_files)
scenarios.codtv <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='codtv')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.codtv,
           parallel=TRUE, parallel_iterations=FALSE,
           case_folder=case_folder, om_dir=om.paths['codtv'],
           em_dir=em.paths['codtv'], case_files=case_files)
## Read in results for both species
get_results_all(user=c(scenarios.codtv, scenarios.cod), parallel=TRUE)



