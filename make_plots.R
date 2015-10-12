
## Make overall plots of performance
xx <- readRDS('results/scalars.RData')
##xx <- calculate_re(read.csv("ss3sim_scalar.csv"))
##yy <- read.csv("ss3sim_ts.csv")
yy <- readRDS('results/ts.RData')
## create meta dataframe to merge into results
xx <- merge(xx, yy[yy$year==100, c('ID', 'year', 'SpawnBio_om',
                   'SpawnBio_em')], by='ID')
xx <- within(xx, {
             depletion_om = SpawnBio_om/SSB_Unfished_om
             depletion_em = SpawnBio_em/SSB_Unfished_em
             depletion_re=(depletion_om-depletion_em)/depletion_om
         terminalSSB_re=(SpawnBio_om-SpawnBio_em)/SpawnBio_om})
xx <- merge(xx, D.df, by="D")
xx <- merge(xx, S.df, by="S")
xx <- merge(xx, E.df, by="E")

myylim <- ylim(-1,1)
g <- ggplot(xx, aes(pct.ess, NatM_p_1_Fem_GP_1_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~om_tv+species) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim + theme_bw()
ggsave('plots/M_re.png', g, width=ggwidth, height=ggheight)
g <- ggplot(xx, aes(pct.ess, SR_BH_steep_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~om_tv+species) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim+ theme_bw()
ggsave('plots/steepness_re.png', g, width=ggwidth, height=ggheight)
g <- ggplot(xx, aes(pct.ess, SSB_MSY_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~om_tv+species) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim+ theme_bw()
ggsave('plots/SSB_MSY_re.png', g, width=ggwidth, height=ggheight)
g <- ggplot(xx, aes(pct.ess, depletion_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~om_tv+species) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim+ theme_bw()
ggsave('plots/depletion_re.png', g, width=ggwidth, height=ggheight)
g <- ggplot(xx, aes(pct.ess, terminalSSB_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~om_tv+species) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim+ theme_bw()
ggsave('plots/terminalSSB_re.png', g, width=ggwidth, height=ggheight)
g <- ggplot(xx, aes(pct.ess, F_MSY_re, group=replicate))+
    geom_line(alpha=.5) + facet_grid(estimated~om_tv+species) +
    geom_vline(xintercept=1, col='blue') +
    geom_hline(yintercept=0, col='red') + myylim+ theme_bw()
ggsave('plots/Fmsy_re.png', g, width=ggwidth, height=ggheight)


### ------------------------------------------------------------
### Look at the estimated deviations from codtv models
## get the OM true values
om.devs.vec1 <- as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S1-cod', case_files=case_files)$tv_params))[27:100]
om.devs1 <- data.frame(expand.grid(year=27:100, estimated=E.df$estimated,
                                     om_tv=S.df$om_tv[1], pct.ess=1.1))
om.devs1$randwalk <- om.devs.vec1
om.devs.vec2 <- as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S2-cod', case_files=case_files)$tv_params))[27:100]
om.devs2 <- data.frame(expand.grid(year=27:100, estimated=E.df$estimated,
                                     om_tv=S.df$om_tv[2], pct.ess=1.1))
om.devs2$randwalk <- om.devs.vec2
om.devs <- rbind(om.devs1, om.devs2)
## Now get the estimated devs for a single replicate, then do cumsum to get
## the random walk
replicate.temp <- 1
xx.devs <- droplevels(subset(xx, replicate==replicate.temp & species=='codtv'))
temp <- c('species','replicate', 'estimated','pct.ess', 'F', 'om_tv')
xx.devs <- xx.devs[,c(temp,names(xx.devs)[grep('DEVr.*_em', x=names(xx.devs))])]
names(xx.devs) <- gsub('SizeSel_1P_1_Fishery_DEVrwalk_|_em', '', x=names(xx.devs))
xx.devs.long <- melt(xx.devs, id.vars=temp,
                     variable.name='year', value.name='dev')
xx.devs.long$year <- as.numeric(as.character(xx.devs.long$year))
xx.devs.long <- ddply(xx.devs.long, .variables=temp, mutate,
                      randwalk=cumsum(dev))

## Now combine and plot
years.ages <- get_caseargs(case_folder, 'D1-E0-F1-S1-cod', case_files=case_files)$agecomp$years[[1]]
g <- ggplot(xx.devs.long, aes(year, randwalk, group=pct.ess, color=pct.ess, linetype=pct.ess==1)) +
  geom_line() + facet_grid(estimated~om_tv) + theme_bw()+
      geom_line(data=om.devs, aes(year, randwalk), color='red', lwd=.5) +
          geom_vline(xintercept=years.ages, lwd=.2, col=gray(.5)) +
              ggtitle(paste("Random walk for replicate", replicate.temp))
ggsave(paste0('plots/deviations_for_replicate_', replicate.temp, '.png'), g, width=9, height=5)

## WARNING! this uses the local variable for replicate.temp from above
for(year.temp in c(30, 40, 50, 60, 70, 80, 90, 99)){
    temp <- ddply(subset(xx.devs.long, om_tv=='Process Error' & year==year.temp),
                  .(pct.ess, om_tv), mutate,
                  dev.re=(dev-dev[which(estimated=='M & h fixed')])/dev[which(estimated=='M & h fixed')])
    g <- ggplot(temp, aes(pct.ess, dev.re, group=estimated, color=estimated))+ theme_bw()+
        geom_line() + ggtitle(paste("Normalized Deviation for replicate",
                                    replicate.temp, "in year", year.temp))
    ggsave(paste0('plots/deviations_for_year_', year.temp, '.png'), g, width=9, height=5)
}





### OLD way of doing this
## em.devs <- dcast(xx.devs.long, formula=year~scenario+replicate, value.var='dev')
## om.devs <- data.frame(
##     value=as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S2-cod', case_files=case_files)$tv_params))[27:100],
##     year=27:100)
## devs <- data.frame(om=om.devs[,1], em_0=cumsum(em.devs[,2]),
##                    em_20=cumsum(em.devs[,3]),
##                    year=27:100)
## png('plots/deviations.png', width=7, height=5, units='in', res=500)
## plot(om~year, data=devs, type='l', lty=2, col='blue', ylab='Deviation')
## lines(em_0~year, data=devs, col='red')
## lines(em_20~year, data=devs, col='blue')
## lines(x=c(27,100), y=c(0,0), col='red', lty=2)
## legend('topleft', legend=c('om_0', 'em_0', 'om_20', 'em_20'), lty=c(2,1),
##        col=rev(c('blue', 'blue', 'red', 'red')))
## dev.off()



## ## Look at trend in process error in selex
## S1.devs <- as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S1-cod', case_files=case_files)$tv_params))
## S2.devs <- as.numeric(unlist(get_caseargs(case_folder, 'D1-E0-F1-S2-cod', case_files=case_files)$tv_params))
## sd(diff(S2.devs[27:100]))
## devs.df <- data.frame(cbind(years=1:100, S1=S1.devs, S2=S2.devs))
## devs.df.long <- melt(devs.df, 'years', variable.name='S')
## devs.df.long <- merge(devs.df.long, S.df, by='S')
## g <- ggplot(devs.df.long, aes(years, value+50.8, group=om_tv, color=om_tv))+
##     geom_line() + ylab('Sel_P1') + theme_bw()
## ggsave('plots/process_errors.png', g, width=ggwidth, height=ggheight)


## ## Look at differences in OM biomass trajectories for an arbitrary iteration
## yy <- merge(yy, D.df, by="D")
## yy <- merge(yy, S.df, by="S")
## yy <- merge(yy, E.df, by="E")
## yy1 <- subset(yy, E=='E0' & D=='D1')
## yy1 <- ddply(yy1, .(year, replicate), mutate,
##              SSB_re=(SpawnBio_om-SpawnBio_om[which(S=='S1')])/
##                  SpawnBio_om[which(S=='S1')])
## g <- ggplot(subset(yy1,  replicate==1), aes(year, SpawnBio_om, group=om_tv, color=om_tv))+
##     geom_line() + ylim(0,5e9) + theme_bw()
## ggsave('plots/SSB_process_error1.png', g, width=ggwidth, height=ggheight)
## g <- ggplot(yy1, aes(year, SSB_re, group=replicate, color=om_tv))+
##     geom_point() + ylim(-1,1) + theme_bw()
## ggsave('plots/SSB_process_error_re.png', g, width=ggwidth, height=ggheight)
