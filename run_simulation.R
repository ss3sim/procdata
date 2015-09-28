### This simulation explores the interaction of process error and data
### weighting for the likelihood components. Started 9/2015. -Cole

### ------------------------------------------------------------
## Step 0: Prepare the R workspace and generate case files
cores <- 4   # parallel cores
## devtools::install("c:/Users/Cole/ss3sim")
## devtools::install_github('r4ss/r4ss')
## sample sizes
Nsim <- 1
## scalars used to control ESS in the data case files, 1 means true ESS
ESS.scalar.vec <- sort(c(1,exp(seq(log(.1),log(10), len=1))))[2]
D.cases <- seq_along(ESS.scalar.vec)
selex.scalar.vec <- c(0,1,5,10,20)
S.cases <- seq_along(selex.scalar.vec)
species <- c('cod','flatfish','yellow-long')[1]
source("startup.R")

case_files <- list(F="F", D=c("index","lcomp","agecomp"), S='S')
scenarios <- expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases),
                              species=spp)
unlink(scenarios, TRUE)
spp <- 'cod'
run_ss3sim(iterations=1:Nsim, scenarios=scenarios,
           parallel=TRUE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths[spp],
           em_dir=em.paths[spp], case_files=case_files,
           ## bias_adjust=TRUE, bias_nsim=10,
           ## admb_options= " -maxfn 1 -phase 50"
           )

## Read in results
get_results_all(user=scenarios, parallel=F)
xx <- calculate_re(read.csv("ss3sim_scalar.csv"))
## create meta dataframe to merge into results
D.df <- data.frame(pct.ess=ESS.scalar.vec, D=paste0('D', D.cases))
xx <- merge(xx, D.df, by="D")
S.df <- data.frame(selex.scalar=selex.scalar.vec, S=paste0('S', S.cases))
xx <- merge(xx, S.df, by="S")
ggplot(xx, aes(pct.ess, SSB_MSY_re, group=replicate))+
    geom_line() + facet_grid(.~selex.scalar)

yy <- read.csv("ss3sim_ts.csv")
subset(yy, year==99)
ggplot(yy, aes(year, SpawnBio_om, group=S, color=S))+geom_line() + ylim(0,5e9)
### ------------------------------------------------------------


## ## test the new smapling functions
## library(ss3sim)
## datlist <- r4ss::SS_readdat('ss3.dat', verbose = FALSE)
## ## should work
## sample_agecomp(datlist, outfile='ss3new.dat', fleets=c(1,2),
##                Nsamp=list(100,200), years=list(c(95,96), c(91,92,93)),
##                cpar=c(NA,NA), ESS=NULL)
## sample_agecomp(datlist, outfile='ss3new.dat', fleets=c(1,2),
##                Nsamp=list(100,200), years=list(c(95,96), c(91,92,93)),
##                cpar=c(1,1), ESS=NULL)
## sample_agecomp(datlist, outfile='ss3new.dat', fleets=c(1,2),
##                Nsamp=list(100,200), years=list(c(95,96), c(91,92,93)),
##                cpar=c(2,1), ESS=list(101,201))
## sample_agecomp(datlist, outfile='ss3new.dat', fleets=c(1,2),
##                Nsamp=list(100,200), years=list(c(95,96), c(91,92,93)),
##                cpar=c(2,1), ESS=list(101,c(1,2,201)))
