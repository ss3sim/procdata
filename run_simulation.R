### This simulation explores the interaction of process error and data
### weighting for the likelihood components. Started 9/2015. -Cole

### ------------------------------------------------------------
## Step 0: Prepare the R workspace and generate case files
cores <- 4   # parallel cores
## devtools::install("c:/Users/Cole/ss3sim")
## devtools::install_github('r4ss/r4ss')
## sample sizes
Nsim <- 20

## scalars used to control ESS in the data case files, 1 means true ESS
ESS.scalar.vec <- sort(unique(c(1,exp(seq(log(.1),log(10), len=1)))))
ESS.scalar.vec <- 1
D.cases <- seq_along(ESS.scalar.vec)
D.df <- data.frame(pct.ess=ESS.scalar.vec, D=paste0('D', D.cases))
selex.scalar.vec <- c(0,20)
S.cases <- seq_along(selex.scalar.vec)
S.df <- data.frame(selex.scalar=factor(paste0('selex.scalar=',selex.scalar.vec),
                   levels=paste0('selex.scalar=',selex.scalar.vec)),
                   S=paste0('S', S.cases))
E.cases <- c(0,1,2)[1]
E.df <-
    data.frame(estimated=factor(c("M & h fixed", "h estimated",
               "M estimated"), levels=c("M & h fixed", "h estimated",
                               "M estimated")), E=paste0('E', E.cases))
species <- c('cod', 'codtv')
case_files <- list(F="F", D=c("index","lcomp","agecomp"), S='S', E='E')
source("startup.R")

## Run the simulation
scenarios <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species=spp)
unlink(scenarios, TRUE)
spp <- 'codtv'
get_caseargs(case_folder, scenarios[2], case_files=case_files)
run_ss3sim(iterations=1:Nsim, scenarios=scenarios,
           parallel=TRUE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths[spp],
           em_dir=em.paths[spp], case_files=case_files,
           ## bias_adjust=TRUE, bias_nsim=10,
           ## admb_options= " -maxfn 1 -phase 50"
           )
## Read in results
get_results_all(user=scenarios, parallel=TRUE)



## Test new TV estimation model (codtv)
for(spp in species[2]){
    scenarios <-
 expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases), species=spp)
    run_ss3sim(iterations=1:1, scenarios=scenarios, parallel=FALSE,
               case_folder=case_folder, om_dir=om.paths[spp],
               em_dir=em.paths[spp], case_files=case_files)
}
get_results_all(parallel=TRUE)

xx <- calculate_re(read.csv('ss3sim_scalar.csv'))
xx <- merge(xx, S.df, by="S")
## Plot estimated random walk devs
xx.devs <- subset(xx, species=='codtv')
xx.devs <- xx.devs[,grep('selex.scalar|DEVr.*_em', x=names(xx.devs))]
names(xx.devs) <- gsub('SizeSel_1P_1_Fishery_DEVrwalk_|_em', '', x=names(xx.devs))
xx.devs.long <- melt(xx.devs)
xx.devs.long$variable <- as.numeric(as.character(xx.devs.long$variable))
em.devs <- dcast(xx.devs.long, formula=variable~selex.scalar)
om.devs <- data.frame(
    value=as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S2-cod', case_files=case_files)$tv_params))[27:100],
    year=27:100)
devs <- data.frame(om=om.devs[,1], em_0=cumsum(em.devs[,2]),
                   em_20=cumsum(em.devs[,3]),
                   year=27:100)

png('plots/deviations.png', width=7, height=5, units='in', res=500)
plot(om~year, data=devs, type='l', lty=2, col='blue', ylab='Deviation')
lines(em_0~year, data=devs, col='red')
lines(em_20~year, data=devs, col='blue')
lines(x=c(27,100), y=c(0,0), col='red', lty=2)
legend('topleft', legend=c('om_0', 'em_0', 'om_20', 'em_20'), lty=c(2,1),
       col=rev(c('blue', 'blue', 'red', 'red')))
dev.off()


### OLD TESTS
## library(ss3models)
## par.df0 <- subset(get_parvalues('D1-E0-F1-S1-cod', write_csv = FALSE))
## par.df1 <- subset(get_parvalues('D1-E1-F1-S1-cod', write_csv = FALSE))
## subset(par.df0, PHASE.em>0, select=c(Label, model))
## subset(par.df1, PHASE.em>0, select=c(Label, model))
## par.df <- par.df[grep('_gp_1|sr_bh_steep', xx$Label),]
## par.df <- par.df[par.df$model=='cod',]

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
