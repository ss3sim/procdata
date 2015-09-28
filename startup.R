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
em.paths <- list('cod'='models/cod/em', 'flatfish'='models/flatfish/em',
                 'yellow-long'='models/yellow-long/em')
om.paths <- list('cod'='models/cod/om', 'flatfish'='models/flatfish/om',
                 'yellow-long'='models/yellow-long/om')
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

## Create case files dynamically for reproducibility
unlink('cases', TRUE)
dir.create('cases')
for(spp in species) {
    ## Get the F cases from the package since based on Fmsy
    file.copy(from=paste0("models/F1-", spp,'.txt'), to=case_folder)
    ## write the data and binning cases
    trash <- sapply(seq_along(ESS.scalar.vec), function(i)
        write_cases_data(case=i, spp=spp, Nsamp=c(100,500),
                         ESS.scalar=ESS.scalar.vec[i]))
    trash <- sapply(seq_along(selex.scalar.vec), function(i)
        write_cases_selex(spp=spp, case=i, scalar=selex.scalar.vec[i]))
}

## End of writing cases
### ------------------------------------------------------------
message("Done loading workspace and preparing for simulation")
