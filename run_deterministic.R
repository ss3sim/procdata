
## Deterministic runs
Nsim.deter <- 50
scenarios.cod <-
    expand_scenarios(cases=list(D=c(101,102,103), F=1, S=S.cases, E=E.cases),
                     species='cod')
run_ss3sim(iterations=1:Nsim.deter, scenarios=scenarios.cod,
           parallel=TRUE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths['cod'],
           em_dir=em.paths['cod'], case_files=case_files)
scenarios.codtv <-
    expand_scenarios(cases=list(D=c(101,102,103), F=1, S=S.cases, E=E.cases),
                     species='codtv')
run_ss3sim(iterations=1:Nsim.deter, scenarios=scenarios.codtv,
           parallel=TRUE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths['codtv'],
           em_dir=em.paths['codtv'], case_files=case_files)
scenarios.codtvx <-
    expand_scenarios(cases=list(D=c(101,102,103), F=1, S=S.cases, E=E.cases),
                     species='codtvx')
run_ss3sim(iterations=1:Nsim.deter, scenarios=scenarios.codtvx,
           parallel=TRUE, parallel_iterations=TRUE,
           case_folder=case_folder, om_dir=om.paths['codtvx'],
           em_dir=em.paths['codtvx'], case_files=case_files)
## Read in results for both species and save them to file
get_results_all(user=c(scenarios.codtvx, scenarios.codtv, scenarios.cod),
                parallel=TRUE)
xx <- calculate_re(read.csv("ss3sim_scalar.csv"))
saveRDS(xx, file='results/deterministic_scalars.RData')
yy <- calculate_re(read.csv("ss3sim_ts.csv"))
saveRDS(yy, file='results/deterministic_ts.RData')
