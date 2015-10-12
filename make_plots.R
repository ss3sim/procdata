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
sd(diff(S2.devs[27:100]))
devs.df <- data.frame(cbind(years=1:100, S1=S1.devs, S2=S2.devs))
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
