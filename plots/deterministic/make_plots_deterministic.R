## Just took the main make_plots script and moved it here and replaced
## data, tweaking as needed. Seemed the easiest way to do this

setwd('plots/deterministic/')

D.deterministic.df <-
    data.frame(pct.ess=ESS.scalar.vec,
               D=paste0('D', c(101,102,103)),
               weighted=factor(c('Under-weighted', 'Right-weighted', 'Over-weighted'),
               levels=c('Under-weighted', 'Right-weighted', 'Over-weighted')))
## Make overall plots of performance
xx <- readRDS('../../results/deterministic_scalars.RData')
yy <- readRDS('../../results/deterministic_ts.RData')
## create meta dataframe to merge into results
xx <- merge(xx, yy[yy$year==100, c('ID', 'year', 'SpawnBio_om',
                   'SpawnBio_em')], by='ID')
xx <- within(xx, {
             depletion_om = SpawnBio_om/SSB_Unfished_om
             depletion_em = SpawnBio_em/SSB_Unfished_em
             depletion_re=(depletion_om-depletion_em)/depletion_om
         terminalSSB_re=(SpawnBio_om-SpawnBio_em)/SpawnBio_om})
xx <- merge(xx, S.df, by="S")
xx <- merge(xx, E.df, by="E")
xx <- merge(xx, D.deterministic.df, by="D")
xx <- merge(xx, species.df, by="species")
yy <- merge(yy, S.df, by="S")
yy <- merge(yy, E.df, by="E")
yy <- merge(yy, D.deterministic.df, by="D")
yy <- merge(yy, species.df, by="species")
xx$selex.em <- xx$SizeSel_1P_1_Fishery_em
## years.ages <- get_caseargs(case_folder, 'D1-E0-F1-S1-cod', case_files=case_files)$agecomp$years[[1]]
xx$M_re <- xx$NatM_p_1_Fem_GP_1_re
xx$steepness_re <- xx$SR_BH_steep_re
xx$Fmsy_re <- xx$F_MSY_re
## ## make sure we got the right number of replicates
nrow(subset(xx, S=='S1' & E=='E0' & species=='cod' & D=='D102'))

## make scalar plots
vars <- c('M_re', 'steepness_re', 'Fmsy_re', 'SSB_MSY_re',
          'terminalSSB_re', 'depletion_re')
myylim <- ylim(-.2,.2)
for(v in vars){
    g <- ggplot(xx, aes_string(x='estimated', y=v, fill='estimated')) + geom_violin(scale='width')+
        facet_grid(weighted~om.process+em.process) +  geom_hline(yintercept=0, col='red') +
            myylim + theme(legend.position='none') +
                ggtitle('Deterministic runs')
    ggsave(paste0('scalars_',v,'.png'), g, width=ggwidth, height=ggheight)
}

## Look at time series plots by estimation group (E) and OM process error (S)
for(e in E.df$estimated){
    for(s in S.df$om.process){
        df <- droplevels(subset(yy, estimated==e & om.process == s))
        for(v in c('SpawnBio_re', 'F_re', 'Recruit_0_re')[1]){
            g <- ggplot(df, aes_string('year', v, group='replicate'))+ geom_line(alpha=.5) +
                facet_grid(weighted~em.process) + theme_bw() + myylim +
                    geom_hline(yintercept=0, col='red') +
                        ggtitle(paste0(s, "; ", e))
            temp <- paste('ts', S.df$S[S.df$om.process==s], v,
                          E.df$E[E.df$estimated==e], sep='_')
            ggsave(paste0(temp, '.png'), g, width=ggwidth, height=7)
        }
    }
}
