## I stole this from the r4ss functon sel.line. Modified it to add argument sp.
sel <- function(x, sp) {
  sel <- rep(NA, length(x))
  startbin <- 1
  peak <- sp[1]
  upselex <- exp(sp[3])
  downselex <- exp(sp[4])
  final <- sp[6]
  if (sp[5] < -1000) {
    j1 <- -1001 - round(sp[5])
    sel[1:j1] <- 1e-06
  }
  if (sp[5] >= -1000) {
    j1 <- startbin - 1
    if (sp[5] > -999) {
      point1 <- 1/(1 + exp(-sp[5]))
      t1min <- exp(-(x[startbin] - peak)^2/upselex)
    }
  }
  if (sp[6] < -1000)
    j2 <- -1000 - round(sp[6])
  if (sp[6] >= -1000)
    j2 <- length(x)
  peak2 <- peak + 2 + (0.99 * x[j2] - peak - 2)/(1 +
                                                   exp(-sp[2]))
  if (sp[6] > -999) {
    point2 <- 1/(1 + exp(-final))
    t2min <- exp(-(x[j2] - peak2)^2/downselex)
  }
  t1 <- x - peak
  t2 <- x - peak2
  join1 <- 1/(1 + exp(-(20/(1 + abs(t1))) * t1))
  join2 <- 1/(1 + exp(-(20/(1 + abs(t2))) * t2))
  if (sp[5] > -999)
    asc <- point1 + (1 - point1) * (exp(-t1^2/upselex) -
                                      t1min)/(1 - t1min)
  if (sp[5] <= -999)
    asc <- exp(-t1^2/upselex)
  if (sp[6] > -999)
    dsc <- 1 + (point2 - 1) * (exp(-t2^2/downselex) -
                                 1)/(t2min - 1)
  if (sp[6] <= -999)
    dsc <- exp(-(t2)^2/downselex)
  sel[(j1 + 1):j2] <- asc * (1 - join1) + join1 * (1 -
                                                     join2 + dsc * join2)
  if (startbin > 1 && sp[5] >= -1000) {
    sel[1:startbin] <- (x[1:startbin]/x[startbin])^2 *
      sel[startbin]
  }
  if (j2 < length(x))
    sel[(j2 + 1):length(x)] <- sel[j2]
  return(sel)
}


## Figure 1 shows the selex trend for the process errors and the impact on
## the selex curve.
xy <- c(.95,.95)
lwd <- 1.5
cex.lab <- .8
line.lab <- 1
xlim <- c(20,100)
cols <- c(gray(.1), gray(.5))
ltys <- c(1,1)
sp <- c(50.8, -3, 5.1, 15, -999, 10)
years <- c(26, 40, 100)
cols2 <- sapply(seq(.2, .8, len=length(years)), function(i) gray(i))
make.file(file.type, filename="figure1_selex", width=width2,
          height=4.5, res=500)
par(mfcol=c(2,1), mar=c(2.5,0,0,0), oma=c(0,2,.5,.5), mgp=c(2,.1,0),
    cex.axis=.6, tck=-.02, col.axis=col.label)
plot(0,0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=c(40, 75))
lines(x=c(1,100), y=c(50.8, 50.8), col=cols[1], lty=ltys[1], lwd=lwd)
lines(devs.df$years, devs.df$S2,col=cols[2], lty=ltys[2], lwd=lwd)
points(x=devs.df[devs.df$year %in% years,'years'], devs.df[devs.df$year
         %in% years,'S2'], pch=16, col=cols2)
legend('topleft', legend=c('Without OM process error',
                     'With OM process error'),  cex=.6,
       col=cols, bty='n', lty=ltys)
print.letter('(a)', xy, cex=.6)
axis(2, col=col.tick); box(col=col.border)
axis(1, col=col.tick); box(col=col.border)
mtext("Size of Full Selectivity", 2, line=line.lab, cex=cex.lab)
mtext("Year", 1, line=line.lab, cex=cex.lab)
## Add selex patterns for some diffeernt years
x <- seq(1, 80, by=1)
plot(0,0, type='n', axes=FALSE, ann=FALSE, xlim=range(x), ylim=c(0, 1))
for(i in 1:length(years)){
  sp[1] <- devs.df[devs.df$year==years[i],]$S2
  lines(x, y=sel(x=x, sp=sp), col=cols2[i], lwd=lwd)
}
legend('topleft', legend=paste0('Year=',years),  cex=.6,
       col=cols2, bty='n', lty=1)
print.letter('(b)', xy, cex=.6)
axis(2, col=col.tick); box(col=col.border)
mtext("Selectivity", 2, line=line.lab, cex=cex.lab)
axis(1, col=col.tick); box(col=col.border)
mtext("Length (cm)", 1, line=line.lab, cex=cex.lab)
dev.off()

