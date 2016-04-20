## Source this file to make the figures for the paper. Figures are just
## plots that appear in the paper. See also make_plots for more.
library(ggplot2)
library(r4ss)
library(dplyr)
## global options
width <- 7                             # inches
height <- 5
width2 <- 3
col.label <- gray(.3)
col.border <- gray(.5)
col.tick <- gray(.6)
print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {
    tmp <- par("usr")
    text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
    text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
    text(x=text.x, y=text.y, labels=label, ...)
}
make.file <- function(type=c("png","pdf", "none"), filename,
                      width, height, res){
    ## Pass it file type and dimensions in inches. It creates file.
    type <- match.arg(type)
    ## If no extension given, add one
    if(length(grep(type, filename))==0)
        filename <- paste0(filename,".",type)
    if(type=="png") png(filename, width=width,
       height=height, units="in", res=res)
    else if(type=="pdf"){pdf(filename, width=width, height=height)}
    else if(dev.cur()==1) dev.new(width=width, height=height)
}


selex <- replist$sizeselex

par(mfcol=c(2,1), mar=c(1,0,0,0), oma=c(2,3,2,1), mgp=c(2,.2,0),
    cex.axis=.8, tck=-.02, col.axis=col.label)
plot(x,y, type='l', ann=FALSE, axes=FALSE)
print.letter('(a)', xy, cex=.8)
axis(2, col=col.tick); box(col=col.border)
mtext("y", 2, line=1.5, cex=1)
plot(x,y, type='l', ann=FALSE, axes=FALSE)
axis(2, col=col.tick);
axis(1, col=col.tick);
mtext("y", 2, line=1.5, cex=1)
mtext("x", 1, line=1.5, cex=1)
mtext("Title", 3, line=.25, cex=1.3, outer=TRUE)
print.letter("fig 2", xy, cex=.8)
box(col=col.border)


## End(Not run)

setwd('figures')
file.type <- 'png'
## 1. image plot / figure/table of lit. review
source("figure1.R")
## 2. experimental design
source("figure2.R")
## 3. OM pop bin results
source("figure3.R")
## 4. estimation bin results
source('figure4.R')
## 5. performance: runtime, iterations, convergence etc.
source('figure5.R')
## 6. run time and accuracy for binning scenarios
source('figure6.R')
setwd('..')
