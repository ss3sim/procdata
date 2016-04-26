
### Figure 4,5,6: Time series of relative error for SSB.  Look at time series
### plots by estimation group (E) and OM process error (S)
add.polygon <- function(df, x=as.numeric(gsub("X", x=names(df), "")),
                        alpha.level, alpha.min=0, alpha.max=1){
    ## Pass this a series of years (x) and a matrix of trajectories (df) and it
    ## adds a polygon to the current plot with whose area contains alpha.level
    ## alpha.min and alpha.max are used to truncate the gray scale color for
    ## betting viewing on some displays
    if(length(x) !=ncol(df) ) df <- t(df)
    alpha.level <- sort(alpha.level)
    for(i in 1:length(alpha.level)){
        alpha <- alpha.level[i]
        alpha.col <- alpha.min+alpha*(alpha.max-alpha.min)
        col.poly <- rgb(1-alpha.col,1-alpha.col,1-alpha.col, alpha=1)
        quantiles.temp <-
            as.matrix(t(apply(df, 2, quantile, probs=c(alpha/2,1-alpha/2))))
        polygon(x=c(x, rev(x)), y=c(quantiles.temp[,1],
                                      rev(quantiles.temp[,2])),
                col=col.poly, border=NA)
    }
}
plot.ts <- function(e, s, ylim=c(-2,2), alpha.levels){
  df <- droplevels(subset(yy, estimated==e & om.process == s,
                          select=c(em.process, SpawnBio_re, year, replicate,weighted)))
  xy <- c(.05,.95)
  cex.lab <- 1
  line.lab <- 1.2
  xlim=c(0,101)
  par(mfcol=c(3,3), mar=0*c(1,1,1,1), oma=c(3,5,3,1),mgp=c(2,.1,0),
      cex.axis=1, tck=-.02, col.axis=col.label)
  k <- 1
  for(em in unique(df$em.process)){
    for(ww in c('Under-weighted', 'Right-weighted', 'Over-weighted')){
      df2 <- droplevels(subset(df, em.process==em & weighted == ww))
      df2 <- df2[order(df2$replicate, df2$year),c('year', 'replicate', 'SpawnBio_re')]
      df2.wide <- dcast(df2, year~replicate, value.var='SpawnBio_re')[,-1]
      plot(0,0, type='n', xlim=xlim, ylim=ylim, axes=FALSE, ann=FALSE)
      add.polygon(df=t(df2.wide), x=1:100, alpha.level=alpha.levels)
      abline(h=0, col='red')
      box(col=col.border)
      if(k %in% 1:3){
        axis(2, col=col.tick)
        mtext(ww, side=2, line=2.75, cex=cex.lab*1.15)
      }
      if(k %in% c(3,6,9)) axis(1, col=col.tick)
      if(k==1) mtext(expression(EM[0]: ~ sigma[s] == 0), side=3, line=.5, cex=cex.lab*1.15)
      if(k==4) mtext(expression(EM[1]: ~ sigma[s] == 0.5), side=3, line=.5, cex=cex.lab*1.15)
      if(k==7) mtext(expression(EM[2]: ~ sigma[s] == 1), side=3, line=.5,
           cex=cex.lab*1.15)
      k <- k+1
    }
  }
  mtext('Year', side=1, outer=TRUE, line=line.lab, cex=cex.lab)
  mtext('Relative Error in Spawning Biomass', side=2, line=line.lab,
        cex=cex.lab, outer=TRUE)
}

make.file(file.type, filename='figure4', width=width,
          height=5, res=500)
plot.ts(e='Fixed', s='OM: sigma=0', alpha.levels=alpha.levels, ylim=ylim)
dev.off()

make.file(file.type, filename='figure5', width=width,
          height=5, res=500)
plot.ts(e='Fixed', s='OM: sigma=.5', alpha.levels=alpha.levels, ylim=ylim)
dev.off()

make.file(file.type, filename='figure6', width=width,
          height=5, res=500)
plot.ts(e='M', s='OM: sigma=.5', alpha.levels=alpha.levels, ylim=ylim)
dev.off()

make.file(file.type, filename='figure7', width=width,
          height=5, res=500)
plot.ts(e='h', s='OM: sigma=.5', alpha.levels=alpha.levels, ylim=ylim)
dev.off()
