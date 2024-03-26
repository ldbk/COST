# auxiliary R-functions defined to make it possible to run the caa program 
# both on R and Splus. A corresponding file exists for Splus

read.table.h <- function(filename, header=T, ...){    # read.table.h
    nr <- length(scan(filename, what="", sep="\n", nmax=10, quiet=T))
    tmp <- sep.find(filename, skip=min(10, nr))
    read.table(filename, sep=tmp$sep, dec=tmp$dec, header=header, as.is=T, 
        quote="")
}

assign1 <- function(...) assign(..., pos=1)         # assign1
dump.data <- function(...) dump(...)                # dump.data
restore.data <- function(...) source(...)           # restore.data
ltsreg1 <- function(y,x,...) ltsreg(y~x, na.action = na.exclude)
open.dyn <- function(...) dyn.load(...)
close.dyn <- function(...) dyn.unload(...)
cur.time <- function() Sys.time()
time.span <- function(t1,t2) as.numeric(difftime(t1,t2,units="secs"))
scann <- function(...) scan(..., quiet=T)
try0 <- function(...) try(..., silent=T)
as.variable <- function(x) x

# open a graphic device dependent on platform type
dev.new <- function(width=7, height=7, record=T,
        xpos=ifelse(is.null(dev.list()),-20, -max(dev.list())*20-00),
        ypos=ifelse(is.null(dev.list()),0,max(dev.list())*20-20),...){
    if (.Platform$OS.type=="windows") 
        windows(width=width, height=height, record=record, xpos=xpos, 
        ypos=ypos, ...) 
    else if (.Platform$OS.type=="unix")
        x11(width=width, height=height, ...)
    else {
        stop(paste("To use diva on other platforms than windows and unix\n",
        "the function 'dev.new' in diva.r must be changed"))
   }
    #bringToTop(-1)  # cannot be used in linux version
    par(mar=c(5,4,4,2))
}

bowrain <- function(n=100){
    n0 <- floor(n*1.25)
    breaks <- floor(n0*c(0.3,0.7))
    use <- c(1:breaks[1], seq(breaks[1]+1,breaks[2],by=2),(breaks[2]+1):n0)
    rev(rainbow(n0,start=.03,end=.6))[use]
}

tsplot <- function(x){
    if (is.matrix(x)) cols <- bowrain(dim(x)[2]) else cols <- 1
    ts.plot(x, col=cols)
}

error.bar <- function(x, mean, lower, add, lwd, upper=lower, incr=F,
        gap=F, log=""){
    if (incr)
        arrows(x, (mean-lower), x, (mean+upper), code=3, angle=90, length=0.05)
    else
        arrows(x, lower, x, upper, code=3, angle=90, length=0.05)
}

