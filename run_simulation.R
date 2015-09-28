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
D.df <- data.frame(pct.ess=ESS.scalar.vec, D=paste0('D', D.cases))
selex.scalar.vec <- c(0,1,5,10,20)[1]
S.cases <- seq_along(selex.scalar.vec)
S.df <- data.frame(selex.scalar=factor(selex.scalar.vec), S=paste0('S', S.cases))
E.cases <- c(0,1)
E.df <- data.frame(estimated=factor(c("M estimated",
                   "steepness estimated")), E=paste0('E', E.cases))
species <- c('cod','flatfish','yellow-long')[1]
source("startup.R")

case_files <- list(F="F", D=c("index","lcomp","agecomp"), S='S', E='E')
scenarios <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species=spp)
unlink(scenarios, TRUE)
spp <- 'cod'
get_caseargs(case_folder, scenarios[2], case_files=case_files)
run_ss3sim(iterations=1:Nsim, scenarios=scenarios,
           parallel=FALSE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths[spp],
           em_dir=em.paths[spp], case_files=case_files,
           ## bias_adjust=TRUE, bias_nsim=10,
           ## admb_options= " -maxfn 1 -phase 50"
           )

## Read in results
get_results_all(user=scenarios, parallel=TRUE)
xx <- calculate_re(read.csv("ss3sim_scalar.csv"))
## create meta dataframe to merge into results
xx <- merge(xx, D.df, by="D")
xx <- merge(xx, S.df, by="S")
ggplot(xx, aes(pct.ess, SSB_MSY_re, group=replicate))+
    geom_line() + facet_grid(.~selex.scalar)

## Look at differences in OM biomass trajectories
yy <- read.csv("ss3sim_ts.csv")
yy <- merge(yy, D.df, by="D")
yy <- merge(yy, S.df, by="S")
subset(yy, year==99)
yy <- ddply(yy, .(year), mutate,
             SSB_re=(SpawnBio_om-SpawnBio_om[which(selex.scalar==0)])/
                 SpawnBio_om[which(selex.scalar==0)])
ggplot(yy, aes(year, SpawnBio_om, group=selex.scalar, color=selex.scalar))+geom_line() + ylim(0,5e9)
ggplot(yy, aes(year, SSB_re, group=selex.scalar, color=selex.scalar))+geom_line() + ylim(-1,1)
### ------------------------------------------------------------

library(ss3models)
par.df <- subset(get_parvalues('models', write_csv = FALSE), model=='cod')
subset(par.df, PHASE.em>0, select=Label)
par.df <- par.df[grep('_gp_1|sr_bh_steep', xx$Label),]
par.df <- par.df[par.df$model=='cod',]



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
