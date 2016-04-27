
## Dig into the case where EM is misspecified and trying to estimate
## steepness and M.
g <- ggplot(subset(xx, estimated=='h'), aes(weighted, y=SR_BH_steep_em)) + geom_violin() + facet_grid(om.process~em.process)
ggsave('plots/steepness_estimates.png', g, width=ggwidth, height=ggheight)
g <- ggplot(subset(xx, estimated=='M'), aes(weighted, y=NatM_p_1_Fem_GP_1_em)) + geom_violin() + facet_grid(om.process~em.process)
ggsave('plots/M_estimates.png', g, width=ggwidth, height=ggheight)


temp <- subset(xx, estimated=='M' & om.process=='OM: sigma=.5' &
                 em.process=='EM sigma=0',
               select=c('ID','NatM_p_1_Fem_GP_1_em', 'SR_BH_steep_em'))
temp.ts <- subset(yy, estimated=='M' & om.process=='OM: sigma=.5' & em.process=='EM sigma=0' )
temp2 <- merge(temp.ts, temp, by='ID')
g <- ggplot(temp2, aes(year, SpawnBio_re, color=NatM_p_1_Fem_GP_1_em, group=replicate)) +
  geom_line(alpha=.5) + facet_grid(weighted~om.process) +
    scale_colour_gradient(low='red')
ggsave('plots/M_vs_SSB.png', g, width=ggwidth, height=ggheight)

vars <- c('M_re', 'steepness_re', 'Fmsy_re', 'SSB_MSY_re',
          'terminalSSB_re', 'depletion_re')
myylim <- ylim(-1.5,1.5)
for(v in vars){
    g <- ggplot(xx, aes_string(x='estimated', y=v, fill='estimated')) + geom_violin()+
        facet_grid(weighted~om.process+em.process) +  geom_hline(yintercept=0, col='red') +
            myylim + theme(legend.position='none')
    ggsave(paste0('plots/scalars_',v,'.png'), g, width=ggwidth, height=ggheight)
}

## Look at time series plots by estimation group (E) and OM process error (S)
myylim <- ylim(-4,4)
for(e in E.df$estimated){
    for(s in S.df$om.process){
        df <- droplevels(subset(yy, estimated==e & om.process == s))
        for(v in c('SpawnBio_re', 'F_re', 'Recruit_0_re')[1]){
            g <- ggplot(df, aes_string('year', v, group='replicate'))+ geom_line(alpha=.5) +
                facet_grid(weighted~em.process) + theme_bw() + myylim +
                    geom_hline(yintercept=0, col='red') +
                        ggtitle(paste0(s, "; ", e))
            temp <- paste('plots/ts', S.df$S[S.df$om.process==s], v,
                          E.df$E[E.df$estimated==e], sep='_')
            ggsave(paste0(temp, '.png'), g, width=ggwidth, height=7)
        }
    }
}

### ------------------------------------------------------------
### Look at the estimated deviations from codtv models
## get the OM true values
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

## Plot the random walks as a function of E for a single rep
for(replicate.temp in 1:1){
    rep.devs.long <- droplevels(subset(devs.long, replicate==replicate.temp
                                       & em.process != 'EM sigma=0'))
    g <- ggplot(rep.devs.long, aes(year, randwalk, group=weighted, color=weighted)) +
        geom_line() + facet_grid(estimated~om.process+em.process) + theme_bw()+
            geom_line(data=om.devs, aes(year, randwalk), color='red', lwd=.5) +
                geom_vline(xintercept=years.ages, lwd=.2, col=gray(.8)) +
                    ggtitle(paste("Random walk for replicate", replicate.temp))
    ggsave(paste0('plots/deviations_for_replicate_', replicate.temp, '.png'),
           g, width=ggwidth, height=ggheight)
}
## ## verify these are different, all values should be unique
## subset(rep.devs.long, year==80)

stop('end of file')
### ------------------------------------------------------------


## ## Plot the random walks for fixed ESS for all reps
## E.devs.long <- subset(devs.long, weighted=='Right-weighted')
## g <- ggplot(E.devs.long, aes(year, randwalk, group=replicate)) +
##     geom_line(alpha=.5) + geom_line(data=om.devs, aes(year, randwalk), color='red', lwd=.5) +
##         facet_grid(estimated~om.process) + theme_bw()+
##             geom_vline(xintercept=years.ages, lwd=.2, col=gray(.5)) +
##                 ggtitle(paste("Random walk for weighted=1"))
## ggsave(paste0('plots/deviations_for_ESS=1_','.png'), g, width=ggwidth, height=ggheight)


## ## Quick r4ss plots for Ian to explore
## if(file.exists('D1-E0-F1-S1-cod')){
##     out <- r4ss::SS_output('D1-E0-F1-S1-cod/1/em/', covar=FALSE, ncols=300)
##     SS_plots(replist=out, uncertainty=FALSE, png=TRUE, html=FALSE,
##              printfolder='../../../plots/r4ss')
## }


## ## WARNING! this uses the local variable for replicate.temp from above
## for(year.temp in c(30, 40, 50, 60, 70, 80, 90, 99)){
##     temp <- ddply(subset(xx.devs.long, om.process=='Process Error' & year==year.temp),
##                   .(weighted, om.process), mutate,
##                   dev.re=(dev-dev[which(estimated=='M & h fixed')])/dev[which(estimated=='M & h fixed')])
##     g <- ggplot(temp, aes(weighted, dev.re, group=estimated, color=estimated))+ theme_bw()+
##         geom_line() + ggtitle(paste("Normalized Deviation for replicate",
##                                     replicate.temp, "in year", year.temp))
##     ggsave(paste0('plots/deviations_for_year_', year.temp, '.png'), g, width=9, height=5)
## }





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
## g <- ggplot(devs.df.long, aes(years, value+50.8, group=om.process, color=om.process))+
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
## g <- ggplot(subset(yy1,  replicate==1), aes(year, SpawnBio_om, group=om.process, color=om.process))+
##     geom_line() + ylim(0,5e9) + theme_bw()
## ggsave('plots/SSB_process_error1.png', g, width=ggwidth, height=ggheight)
## g <- ggplot(yy1, aes(year, SSB_re, group=replicate, color=om.process))+
##     geom_point() + ylim(-1,1) + theme_bw()
## ggsave('plots/SSB_process_error_re.png', g, width=ggwidth, height=ggheight)
