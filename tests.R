## Test new TV estimation model (codtv)
for(spp in species[2]){
    scenarios <-
 expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases), species=spp)
    run_ss3sim(iterations=1:1, scenarios=scenarios, parallel=FALSE,
               case_folder=case_folder, om_dir=om.paths[spp],
               em_dir=em.paths[spp], case_files=case_files)
}
get_results_all(parallel=TRUE)



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
