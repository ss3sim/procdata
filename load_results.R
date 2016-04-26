### This file loads in the saved results and prepares them for making
### plots, figures and tables.

message('\nLoading results into workspace and preparing for plots/figures/tables')
xx <- readRDS('results/scalars.RData')
##xx <- calculate_re(read.csv("ss3sim_scalar.csv"))
##yy <- read.csv("ss3sim_ts.csv")
yy <- readRDS('results/ts.RData')
## create meta dataframe to merge into results
xx <- merge(xx, yy[yy$year==100, c('ID', 'year', 'SpawnBio_om', 'SpawnBio_em')], by='ID')
xx <- within(xx, {
             depletion_om = SpawnBio_om/SSB_Unfished_om
             depletion_em = SpawnBio_em/SSB_Unfished_em
             depletion_re=(depletion_om-depletion_em)/depletion_om
         terminalSSB_re=(SpawnBio_om-SpawnBio_em)/SpawnBio_om})
xx <- merge(xx, D.df, by="D")
xx <- merge(xx, S.df, by="S")
xx <- merge(xx, E.df, by="E")
xx <- merge(xx, species.df, by="species")
yy <- merge(yy, D.df, by="D")
yy <- merge(yy, S.df, by="S")
yy <- merge(yy, E.df, by="E")
yy <- merge(yy, species.df, by="species")
xx$selex.em <- xx$SizeSel_1P_1_Fishery_em
years.ages <- get_caseargs(case_folder, 'D1-E0-F1-S1-cod', case_files=case_files)$agecomp$years[[1]]
xx$M_re <- xx$NatM_p_1_Fem_GP_1_re
xx$steepness_re <- xx$SR_BH_steep_re
xx$Fmsy_re <- xx$F_MSY_re

## Filter out those that didn't converge
xx$converged <- with(xx, ifelse(!hessian | params_on_bound_em>0, FALSE, TRUE))


replist <- readRDS('results/replist.RDS')
selex <- replist$sizeselex

### Do some data massaging for the plots and figures specifically
## Data for figure 1
S1.devs <- 50.8+as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S1-cod', case_files=case_files)$tv_params))
S2.devs <- 50.8+as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S2-cod', case_files=case_files)$tv_params))
sd(diff(S2.devs[27:100]))
devs.df <- data.frame(cbind(years=1:100, S1=S1.devs, S2=S2.devs))


## These are used in figure 1 to show biomass trajectories for a single
## scenario and the F trajectory.
om.ssb <- subset(yy, E=='E0' & D=='D1' & species == 'cod',
              select=c('SpawnBio_om', 'year',
                'om.process', 'replicate'))
## reshape gymnastics to get median OM biomass trajectories
om.ssb.wide <- dcast(om.ssb, formula=replicate+year~om.process, value.var='SpawnBio_om' )
names(om.ssb.wide)[3:4] <- c('s0', 's5')
om.ssb.wide2 <- ddply(om.ssb.wide, .(year), summarize, med.s0=median(s0), med.s5=median(s5))
F1 <- data.frame(year=1:100, F=as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S1-cod', case_files=case_files)$F$fvals)))



## Random walk patterns for figure 3
om.devs.vec1 <- as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S1-cod', case_files=case_files)$tv_params))[27:100]
om.devs1 <- data.frame(expand.grid(year=27:100, estimated=E.df$estimated,
                                     om.process=S.df$om.process[1], weighted=1.1))
om.devs1$randwalk <- om.devs.vec1
om.devs.vec2 <- as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S2-cod', case_files=case_files)$tv_params))[27:100]
om.devs2 <- data.frame(expand.grid(year=27:100, estimated=E.df$estimated,
                                     om.process=S.df$om.process[2], weighted=1.1))
om.devs2$randwalk <- om.devs.vec2
om.devs <- rbind(om.devs1, om.devs2)
om.devs$replicate <- 1
om.devs$randwalk <- om.devs$randwalk+50.8
## Setup the data in long format with random wlak by year as variables
temp <- c('selex.em','em.process','replicate', 'estimated','weighted', 'F', 'om.process')
devs <- xx[,c(temp,names(xx)[grep('DEVr.*_em', x=names(xx))])]
names(devs) <- gsub('SizeSel_1P_1_Fishery_DEVrwalk_|_em', '', x=names(devs))
devs.long <- melt(devs, id.vars=temp, variable.name='year', value.name='dev')
devs.long$year <- as.numeric(as.character(devs.long$year))
devs.long <- ddply(devs.long, .variables=temp, mutate,
                      randwalk=cumsum(dev)+ selex.em)
devs.long <-
  droplevels(subset(devs.long,  em.process != 'EM sigma=0' &
                        estimated=='Fixed'))
devs.long.medians <-
  ddply(devs.long, .(em.process, weighted, om.process, year),
        summarize, randwalk.median=median(randwalk))
