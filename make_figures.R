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


setwd('figures')
file.type <- 'png'
## 1. selex patterns in the OM
source("figure1.R")
## 2. OM SSB trends
source("figure2.R")
## 3. Estimates of random walk devs
source("figure3.R")
## 4, 5 and 6. Relative error in SSB for different cases
alpha.levels <- c(.25,.5, .75, .95)
ylim <- c(-1.1,1.1)
source('figure456.R')
source('figure78.R')
setwd('..')
