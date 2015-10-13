## This file loads libraries, and preps the workspace

### ------------------------------------------------------------
## Startup the working environment
## ## Update the development tools, if you haven't recently
## update.packages(c('r4ss','knitr', 'devtools', 'roxygen2'))
## Load the neccessary libraries
case_folder <- 'cases'
## Used to get models from the ss3models package, but manually moved them
## into this repo for better long-term reproducibility. The F case files
## are also there.
em.paths <- list('cod'='models/cod/em', 'codtv'='models/codtv/em')
om.paths <- list('cod'='models/cod/om', 'codtv'='models/codtv/om')

## devtools::install_github("ss3sim/ss3sim")
## install("../ss3sim")
## install("../ss3models")
library(ss3sim)
library(reshape2)
library(r4ss)
library(ggplot2)
library(devtools)
library(plyr)
library(dplyr)
library(doParallel)
registerDoParallel(cores = cores)
library("foreach")
ggwidth <- 9
ggheight <- 5
message(paste(getDoParWorkers(), "cores have been registered for",
    "parallel processing."))
message("Writing case files...")

### ------------------------------------------------------------
write_cases_data <- function(case, spp, Nsamp, ESS.scalar, dir=case_folder){
    ## Years need to be different for different species
    if(spp=='yellow-long'){
        ## just add 75 more years due to the extra burn in
        index.years1 <- 'years;list(75+seq(76,100, by=2))'
        index.years2 <- 'years;list(75+c(seq(94,100,by=2)))'
        comp.years1 <- 'years;list(75+c(36,46,seq(51,66,by=5),71:100), 75+seq(76,100, by=2))'
        comp.years2 <- 'years;list(75+c(seq(86,90,by=10), 91:100), 75+seq(94,100,by=2))'
    } else {
        index.years1 <- 'years;list(seq(76,100, by=2))'
        index.years2 <- 'years;list(c(seq(94,100,by=2)))'
        comp.years1 <- 'years;list(c(36,46,seq(51,66,by=5),71:100), seq(76,100, by=2))'
        comp.years2 <- 'years;list(c(seq(86,90,by=10), 91:100), seq(94,100,by=2))'
    }
    Nsamp.string <- paste0('Nsamp;list(', paste0(Nsamp, collapse=','), ')')
    ESS.string <- paste0('ESS;list(', paste0(Nsamp*ESS.scalar, collapse=','), ')')
    ## data rich w/ age comps
    index <- c('fleets;c(2)', index.years1 , 'sds_obs;list(.2)')
    lcomp <- c('fleets;c(1,2)', comp.years1, Nsamp.string, ESS.string,  'cpar;c(NA,NA)')
    agecomp <- lcomp
    writeLines(index, con=paste0(dir,"/", "index", case,"-", spp, ".txt"))
    writeLines(lcomp, con=paste0(dir,"/", "lcomp", case,"-", spp, ".txt"))
    writeLines(agecomp, con=paste0(dir,"/", "agecomp", case,"-", spp, ".txt"))
}

write_cases_selex <- function(spp, case, scalar, dir=case_folder){
    ## The true parameter value for P1 for fishery is 50.8
    ## set.seed(1)
    ## sigmaS <- CV*50.8
    ## if(CV==0) {
    ##     devs <- rep(0,100)
    ## } else {
    ##     devs <- round(rnorm(100, 0, sigmaS),2)
    ## }
    ## if(any(devs < -50.8)) stop("negative selectivity parameter!")
    devs <- scalar*c(rep(0, 25), seq(0, -.5, len=15), seq(-.5, 1, len=60))
    selex.string <- c('function_type; change_tv',
      'param; SizeSel_1P_1_Fishery',
      paste0('dev;c(', paste0(devs, collapse=','), ')'))
    writeLines(selex.string, con=paste0(dir,"/", "S", case,"-", spp, ".txt"))
}
### ------------------------------------------------------------

write_cases_estimation <- function(spp, case, dir=case_folder){
    ## Write a change_e case file for which params to estimate
    pars <- c("L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1",
              "VonBert_K_Fem_GP_1", "CV_young_Fem_GP_1",
              "CV_old_Fem_GP_1", "SR_BH_steep")
    if(case==0){ ## neither h nor M estimated
        phases <- c(rep(-1, 5),-1)
        M.est <- 'natM_val;c(NA,-1)'
    } else if(case==1) { ## just h estimated
        M.est <- 'natM_val;c(NA,-1)'
        phases <- c(rep(-1, 5), 5)
    } else if(case==2) { ## just M estimated
        M.est <- 'natM_val;c(NA,5)'
        phases <- c(rep(-1, 5), -1)
    } else {
        stop('case not defined')
    }
    est <-
        c(M.est, 'natM_type;1parm',
          'natM_n_breakpoints; NULL',
          'natM_lorenzen;NULL',
          paste0('par_name;c(\'', paste0(pars, sep='\'', collapse=",\'"), ')'),
          paste0('par_int;c(', paste0(rep(NA, len=length(pars)), collapse=","), ')'),
          paste0('par_phase;c(', paste0(phases, collapse=','), ')'),
          'forecast_num;1')
    writeLines(est, con=paste0(dir,"/", "E", case,"-", spp, ".txt"))
}

D.cases <- seq_along(ESS.scalar.vec)
D.df <- data.frame(pct.ess=ESS.scalar.vec, D=paste0('D', D.cases))
S.cases <- seq_along(selex.scalar.vec)
S.df <- data.frame(om_tv=factor(c("No Process Error", "Process Error")),
                   S=paste0('S', S.cases))
E.cases <- c(0,1,2)
E.df <-
    data.frame(estimated=factor(c("M & h fixed", "h estimated",
               "M estimated"), levels=c("M & h fixed", "h estimated",
                               "M estimated")), E=paste0('E', E.cases))

## Create case files dynamically for reproducibility
unlink('cases', TRUE)
dir.create('cases')
for(spp in species) {
    ## Get the F cases from the package since based on Fmsy
    file.copy(from=paste0("models/F1-", spp,'.txt'), to=case_folder)
    ## write the data and binning cases
    trash <- sapply(D.cases, function(i)
        write_cases_data(case=i, spp=spp, Nsamp=c(100,500),
                         ESS.scalar=ESS.scalar.vec[i]))
    trash <- sapply(S.cases, function(i)
        write_cases_selex(spp=spp, case=i, scalar=selex.scalar.vec[i]))
    trash <- sapply(E.cases, function(i)  write_cases_estimation(spp, case=i))
}

## End of writing cases
### ------------------------------------------------------------
message("Done loading workspace and preparing for simulation")
