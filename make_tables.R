## Source this to make tables in the results folder

mare <- function(x) round(median(abs(x)),2)

table.long <-
  ddply(xx, .(om.process, em.process, estimated, weighted),
        summarize, count=length(converged), converged.pct=mean(converged),
        runtime=median(RunTime),
        mare.SSB_MSY=mare(SSB_MSY_re),
        mare.Fmsy=mare(Fmsy_re),
        mare.terminalSSB=mare(terminalSSB_re))
table.SSB_MSY.mare <-
  dcast(table.long, om.process+em.process+estimated~weighted, value.var='mare.SSB_MSY')
write.table(table.MSY.mare,'results/table.SSB_MSY.mare.csv', sep=',', row.names=FALSE)
table.Fmsy.mare <-
  dcast(table.long, om.process+em.process+estimated~weighted, value.var='mare.Fmsy')
write.table(table.MSY.mare,'results/table.Fmsy.mare.csv', sep=',', row.names=FALSE)
table.terminalSSB.mare <-
  dcast(table.long, om.process+em.process+estimated~weighted, value.var='mare.terminalSSB')
write.table(table.MSY.mare,'results/table.terminalSSB.mare.csv', sep=',', row.names=FALSE)
table.converged <- dcast(table.long, om.process+em.process+estimated~weighted, value.var='converged.pct')
write.table(table.converged,'results/table.converged.csv', sep=',', row.names=FALSE)
table.runtime <- dcast(table.long, om.process+em.process+estimated~weighted, value.var='runtime')
write.table(table.runtime,'results/table.runtime.csv', sep=',', row.names=FALSE)

table.long.ts <-
  ddply(yy, .(om.process, em.process, estimated, weighted),
        summarize,
        mare.SpawnBio=mare(SpawnBio_re))
table.SpawnBio.mare <-
  dcast(table.long.ts, om.process+em.process+estimated~weighted, value.var='mare.SpawnBio')
write.table(table.MSY.mare,'results/table.SpawnBio.mare.csv', sep=',', row.names=FALSE)



