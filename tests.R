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
