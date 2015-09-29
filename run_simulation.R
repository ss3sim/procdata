### This simulation explores the interaction of process error and data
### weighting for the likelihood components. Started 9/2015. -Cole

### ------------------------------------------------------------
## Step 0: Prepare the R workspace and generate case files
cores <- 10   # parallel cores
## devtools::install("c:/Users/Cole/ss3sim")
## devtools::install_github('r4ss/r4ss')
## sample sizes
Nsim <- 20
## scalars used to control ESS in the data case files, 1 means true ESS
ESS.scalar.vec <- sort(unique(c(1,exp(seq(log(.1),log(10), len=10)))))
D.cases <- seq_along(ESS.scalar.vec)
D.df <- data.frame(pct.ess=ESS.scalar.vec, D=paste0('D', D.cases))
selex.scalar.vec <- c(0,10,20)
S.cases <- seq_along(selex.scalar.vec)
S.df <- data.frame(selex.scalar=factor(paste0('selex.scalar=',selex.scalar.vec),
                   levels=paste0('selex.scalar=',selex.scalar.vec)),
                   S=paste0('S', S.cases))
E.cases <- c(0,1,2)
E.df <-
    data.frame(estimated=factor(c("M & h fixed", "h estimated",
               "M estimated"), levels=c("M & h fixed", "h estimated",
                               "M estimated")), E=paste0('E', E.cases))

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
           parallel=TRUE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths[spp],
           em_dir=em.paths[spp], case_files=case_files,
           ## bias_adjust=TRUE, bias_nsim=10,
           ## admb_options= " -maxfn 1 -phase 50"
           )
## Read in results
get_results_all(user=scenarios, parallel=TRUE)

xx <- calculate_re(read.csv("ss3sim_scalar.csv"))
yy <- read.csv("ss3sim_ts.csv")
## create meta dataframe to merge into results
xx <- merge(xx, yy[yy$year==100, c('ID', 'year', 'SpawnBio_om',
                   'SpawnBio_em')], by='ID')
xx <- within(xx, {
             depletion_om = SpawnBio_om/SSB_Unfished_om
             depletion_em = SpawnBio_em/SSB_Unfished_em
         depletion_re=(depletion_om-depletion_em)/depletion_om})
xx <- merge(xx, D.df, by="D")
xx <- merge(xx, S.df, by="S")
xx <- merge(xx, E.df, by="E")

## Look at trend in process error in selex
S1.devs <- as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S1-cod', case_files=case_files)$tv_params))
S2.devs <- as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S2-cod', case_files=case_files)$tv_params))
S3.devs <- as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S3-cod', case_files=case_files)$tv_params))
devs.df <- data.frame(cbind(years=1:100, S1=S1.devs, S2=S2.devs, S3=S3.devs))
devs.df.long <- melt(devs.df, 'years', variable.name='S')
devs.df.long <- merge(devs.df.long, S.df, by='S')
g <- ggplot(devs.df.long, aes(years, value+50.8, group=selex.scalar, color=selex.scalar))+
    geom_line() + ylab('Sel_P1') + theme_bw()
ggsave('plots/process_errors.png', g, width=ggwidth, height=ggheight)

## Look at differences in OM biomass trajectories for an arbitrary iteration
yy <- merge(yy, D.df, by="D")
yy <- merge(yy, S.df, by="S")
yy <- merge(yy, E.df, by="E")
yy1 <- subset(yy, E=='E0' & D=='D1')
yy1 <- ddply(yy1, .(year, replicate), mutate,
             SSB_re=(SpawnBio_om-SpawnBio_om[which(S=='S1')])/
                 SpawnBio_om[which(S=='S1')])
g <- ggplot(subset(yy1,  replicate==1), aes(year, SpawnBio_om, group=selex.scalar, color=selex.scalar))+
    geom_line() + ylim(0,5e9) + theme_bw()
ggsave('plots/SSB_process_error1.png', g, width=ggwidth, height=ggheight)
g <- ggplot(yy1, aes(year, SSB_re, group=replicate, color=selex.scalar))+
    geom_point() + ylim(-1,1) + theme_bw()
ggsave('plots/SSB_process_error_re.png', g, width=ggwidth, height=ggheight)

myylim <- ylim(-1,1)
g <- ggplot(xx, aes(pct.ess, NatM_p_1_Fem_GP_1_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~selex.scalar) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim + theme_bw()
ggsave('plots/M_re.png', g, width=ggwidth, height=ggheight)
g <- ggplot(xx, aes(pct.ess, SR_BH_steep_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~selex.scalar) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim+ theme_bw()
ggsave('plots/steepness_re.png', g, width=ggwidth, height=ggheight)
g <- ggplot(xx, aes(pct.ess, SSB_MSY_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~selex.scalar) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim+ theme_bw()
ggsave('plots/SSB_MSY_re.png', g, width=ggwidth, height=ggheight)
g <- ggplot(xx, aes(pct.ess, depletion_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~selex.scalar) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim+ theme_bw()
ggsave('plots/depletion_re.png', g, width=ggwidth, height=ggheight)
g <- ggplot(xx, aes(pct.ess, F_MSY_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~selex.scalar) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim+ theme_bw()
ggsave('plots/Fmsy_re.png', g, width=ggwidth, height=ggheight)
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
