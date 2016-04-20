### Figure 3: median random walk patterns for scenarios across all
### replicates for just the 'fixed' case
xy <- c(.05,.95)
lwd <- 3
cex.lab <- 1.2
line.lab <- 1.2
xlim <- c(20,100)
ylim <- c(40,71)
cols <- c(gray(.2), gray(.5), gray(.7))
ltys <- c(1,1,1)
sp <- c(50.8, -3, 5.1, 15, -999, 10)
years <- c(26, 40, 100)
k <- 1
weights <- as.character(unique(devs.long.medians$weighted))
make.file(file.type, filename="figure3_randwalk", width=width,
          height=5, res=500)
par(mfcol=c(2,2), mar=0*c(1,1,1,1), oma=c(2.75,2.75,2.5,1), mgp=c(2,.1,0),
    cex.axis=1, tck=-.02, col.axis=col.label)
for(em in unique(devs.long.medians$em.process)){
  for(om in unique(devs.long.medians$om.process)){
    plot(0,0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=ylim)
    aa <- droplevels(subset(om.devs, om.process==om & estimated=='Fixed'))
    for(ww in 1:3){
      ## Plot the true one
      with(aa, lines(year, randwalk, lwd=lwd/1.25, col=gray(.5), lty=3))
      ## loop through each weighted case and plot lines
      zz <- subset(devs.long.medians,
                   em.process==em & om.process==om & weighted == weights[ww])
      lines(x=zz$year, y=zz$randwalk.median, col=cols[ww], lty=ltys[ww], lwd=lwd)
      if(k==1)
        legend('topright', legend=weights, cex=1, col=cols, bty='n',
               lty=ltys, lwd=lwd)
      print.letter(paste0('(',letters[k], ')'), xy, cex=1);
      if(k==1 | k==2) axis(2, col=col.tick); box(col=col.border)
      if(k==2 | k==4) axis(1, col=col.tick); box(col=col.border)
      if(k==1) mtext(em, side=3, line=.5, cex=cex.lab*1.15)
      if(k==3) mtext(em, side=3, line=.5, cex=cex.lab*1.15)
    }
    k <- k+1
  }
}
mtext("Year", 1, line=line.lab, cex=cex.lab, outer=TRUE)
mtext("Size of Full Selectivity", 2, line=line.lab, cex=cex.lab, outer=TRUE)
dev.off()
