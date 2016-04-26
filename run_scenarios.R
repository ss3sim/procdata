### Source this file to run all of the scenarios and replicates, gather
### results, and write them to file.

scenarios.cod <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='cod')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.cod,
           bias_adjust=TRUE, bias_nsim=Nbias, hess_always=TRUE,
           parallel=TRUE, parallel_iterations=FALSE,
           case_folder=case_folder, om_dir=om.paths['cod'],
           em_dir=em.paths['cod'], case_files=case_files)
scenarios.codtv <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='codtv')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.codtv,
           bias_adjust=TRUE, bias_nsim=Nbias, hess_always=TRUE,
           parallel=TRUE, parallel_iterations=FALSE,
           case_folder=case_folder, om_dir=om.paths['codtv'],
           em_dir=em.paths['codtv'], case_files=case_files)
scenarios.codtvx <-
    expand_scenarios(cases=list(D=D.cases, F=1, S=S.cases, E=E.cases),
                     species='codtvx')
run_ss3sim(iterations=1:Nsim, scenarios=scenarios.codtvx,
           bias_adjust=TRUE, bias_nsim=Nbias, hess_always=TRUE,
           parallel=TRUE, parallel_iterations=FALSE,
           case_folder=case_folder, om_dir=om.paths['codtvx'],
           em_dir=em.paths['codtvx'], case_files=case_files)
## Read in results for both species and save them to file
get_results_all(user=c(scenarios.codtvx, scenarios.codtv, scenarios.cod),
                parallel=TRUE)
xx <- calculate_re(read.csv("ss3sim_scalar.csv"))
saveRDS(xx, file='results/scalars.RData')
yy <- calculate_re(read.csv("ss3sim_ts.csv"))
## This file is too big so drop unused columns
yy <- yy[, c('D', 'S', 'E', 'species', 'SpawnBio_om', 'SpawnBio_em', 'year', 'replicate',
             'SpawnBio_re', 'ID')]
saveRDS(yy, file='results/ts.RData')
## Save an OM r4ss output list which is used in some plots
replist <- SS_output(dir='D1-E0-F1-S1-cod/1/om/', covar=FALSE, ncols=250)
saveRDS(replist, file='results/replist.RDS')

## Quick code to check for .cor file, which is our way of testing for
## convergence (i.e., Hessian was inverted).
scens <- id_scenarios(getwd())
converged <- ldply(scens, function(i){
    x <- list.files(i)
    print(i)
    reps <- x[-grep('bias|.csv', x)]
    ldply(reps, function(x){
        data.frame(ID=paste0(i, '-', x), rep=as.numeric(x),
                   converged.cor=file.exists(file.path(i, x, 'em','ss3_24~2.cor')),
                   converged.sso=file.exists(file.path(i, x, 'em','covar.sso')),
                   converged.covar=file.exists(file.path(i, x, 'em','admodel.cov'))
                   )})
})
write.table(x=converged, file='results/converged.csv', sep=',', row.names=FALSE)
