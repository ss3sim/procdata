## Figure 2 shows the median biomass of the OMs for a single scenario
## (they're all the same) for the two levels of process error, vs the
## fishing pattern.
xy <- c(.95,.95)
lwd <- 1.5
cex.lab <- .8
line.lab <- 1
xlim <- c(20,100)
make.file(file.type, filename="figure2_SSB", width=width2,
          height=4.5, res=500)
par(mfcol=c(2,1), mar=c(.5,0,0,0), oma=c(1.5,2,.5,.5), mgp=c(2,.1,0),
    cex.axis=.6, tck=-.02, col.axis=col.label)
cols <- c(gray(.1), gray(.5))
plot(0,0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=c(0, 4.1))
with(om.ssb.wide2, lines(year, med.s0/1e9, col=cols[1], lwd=lwd))
with(om.ssb.wide2, lines(year, med.s5/1e9, col=cols[2], lwd=lwd))
legend('bottom', legend=c('Without OM process error',
                     'With OM process error'), lty=1, cex=.6,
       col=cols, bty='n')
print.letter('(a)', xy, cex=.6)
axis(2, col=col.tick); box(col=col.border)
mtext("Spawning Biomass (x 1e9)", 2, line=line.lab, cex=cex.lab)
plot(F1$year, F1$F, type='l', axes=FALSE, ann=FALSE, lwd=lwd, xlim=xlim)
print.letter('(b)', xy, cex=.6)
axis(2, col=col.tick); box(col=col.border)
mtext("Fishing Effort (F)", 2, line=line.lab, cex=cex.lab)
axis(1, col=col.tick); box(col=col.border)
mtext("Year", 1, line=line.lab, cex=cex.lab)
dev.off()
