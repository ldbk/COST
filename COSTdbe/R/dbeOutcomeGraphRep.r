
 
#################################################################################
#                                                                               #
#  Plot of bootstrap distribution for 'totalW', 'totalN' estimates (histogram)  #
#                                                                               #
#################################################################################

 
dbePlotRepDist <- function(object,                               #'dbeOutput' object 
                           Slot,                                 #ex: "lenStruc"
                           dispKey=TRUE,                         #if TRUE and if various panels are displayed, a describing key is displayed      
                           ...){                                 #further graphical parameters

if (!Slot%in%c("totalW","totalN")) stop("Wrong 'Slot' parameter!!")

  #-----------------------------------------------------------------------------
  # Graphical parameters
  #-----------------------------------------------------------------------------

dots <- list(...)  

if (is.null(dots$col)) dots$col <- "skyblue"
if (is.null(dots$lwd)) dots$lwd <- 1
if (is.null(dots$lty)) dots$lty <- 1
if (is.null(dots$pch)) dots$pch <- 21

if (is.null(dots$cex)) dots$cex <- 0.8
if (is.null(dots$p.cex)) dots$p.cex <- 1.2
if (is.null(dots$k.cex)) dots$k.cex <- 1.2
if (is.null(dots$cex.lab)) dots$cex.lab <- 1.1
if (is.null(dots$cex.sub)) dots$cex.sub <- 1.1
if (is.null(dots$cex.axis)) dots$cex.axis <- 1
if (is.null(dots$cex.main)) dots$cex.main <- 1.2

if (is.null(dots$col.lab)) dots$col.lab <- "black"
if (is.null(dots$col.sub)) dots$col.sub <- "black"
if (is.null(dots$col.axis)) dots$col.axis <- "black"
if (is.null(dots$col.main)) dots$col.main <- "black"

if (is.null(dots$font)) dots$font <- 6
if (is.null(dots$font.lab)) dots$font.lab <- 7
if (is.null(dots$font.sub)) dots$font.sub <- 6
if (is.null(dots$font.axis)) dots$font.axis <- 7
if (is.null(dots$font.main)) dots$font.main <- 7

if (is.null(dots$rot)) dots$rot <- 90
if (is.null(dots$layout)) dots$layout <- NULL 
if (is.null(dots$as.table)) dots$as.table <- FALSE 

  #-----------------------------------------------------------------------------
  # Extraction of the numerical data, and formatting process
  #-----------------------------------------------------------------------------

tab <- slot(object,Slot)$rep
#NAs values are removed
tab <- tab[!is.na(tab$value),]
if (nrow(tab)==0) stop("no available data!!")
if (all(is.na(tab))) stop("no available data!!")
if (all(levels(factor(as.character(tab$time)))=="all")) tab$time <- NA
if (all(levels(factor(as.character(tab$space)))=="all")) tab$space <- NA
if (all(levels(factor(as.character(tab$technical)))=="all")) tab$technical <- NA

timeStrata <- spaceStrata <- techStrata <- TRUE
if (all(is.na(tab$time))) timeStrata <- FALSE
if (all(is.na(tab$space))) spaceStrata <- FALSE
if (all(is.na(tab$technical))) techStrata <- FALSE

#we get rid of unused levels
if (timeStrata) tab$time <- factor(tab$time)
if (spaceStrata) tab$space <- factor(tab$space)
if (techStrata) tab$technical <- factor(tab$technical)
   
if (is.null(dots$xlab)) dots$xlab <- "" 
if (is.null(dots$ylab)) dots$ylab <- "Frequency"
tstSp <- !is.na(object@species)
tstCat <- !is.na(object@catchCat)
if (is.null(dots$main)) dots$main <- paste("'",Slot,"' replicates distribution \n",
    paste(c("for ",paste("\"",object@species,"\" species",sep="")," and ",
    paste("\"",object@catchCat,"\" fraction",sep=""))[c(tstSp|tstCat,tstSp,tstSp&tstCat,tstCat)],collapse=""),sep="")   

strip.col <- trellis.par.get("strip.background")$col



  #-----------------------------------------------------------------------------
  # Graphical display
  #-----------------------------------------------------------------------------

indStr <- c(timeStrata,spaceStrata,techStrata)
intLeg <- c(object@strataDesc@timeStrata,object@strataDesc@spaceStrata,object@strataDesc@techStrata)

if (all(is.na(intLeg))) intLeg <- c("time","space","technical") 


nTst <- sum(indStr)==0

eval(parse('',text=paste("histogram(~value",paste("|",paste(c("time","space","technical")[indStr],collapse="*",sep=""),sep="")[!nTst],
  ",data=tab,breaks=NULL,main=list(dots$main,font=dots$font.main,col=dots$col.main,cex=dots$cex.main),",
  "col=dots$col,lwd=dots$lwd,lty=dots$lty,pch=dots$pch,cex=dots$p.cex,fill=dots$col,par.strip.text=list(font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,",
  "scales=list(x=list(relation=\"free\",rot=dots$rot,cex=dots$cex),y=list(relation=\"free\",cex=dots$cex),font=dots$font.axis,col=dots$col.axis,cex=dots$cex.axis),",
  "ylab=list(dots$ylab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab),xlab=list(dots$xlab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab),",
  ",key=list(points=list(pch=15,cex=dots$k.cex,col=strip.col[1:sum(indStr)]),text=list(intLeg[indStr]),space=\"right\",font=dots$font.lab,columns=1,border=TRUE)"[dispKey & !nTst],
  ")",sep="")))                 
}


###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
 

###########################################################################
#                                                                         #
#  Plot of bootstrap estimation for 'lenStruc' or 'ageStruc'  (barchart)  #
#                                                                         #
###########################################################################


 
dbePlotRepLA <- function(object,                               #'dbeOutput' object 
                         Slot,                                 #ex: "lenStruc"
                         probs=c(0.05,0.95),                   #if NA, no IC displayed
                         step=NA,                              #specified numerical step for length or age classes (if NA, no filled gaps) 
                         origin=TRUE,                          #if TRUE, estimates from the original dataset are added ('iter' value = 0)
                         dispKey=TRUE,                         #if TRUE and if various panels are displayed, a describing key is displayed
                         KurtSkew=FALSE,                       #display of 'skewness' and 'kurtosis' values per strata and per length or age class
                         ...){                                 #further graphical parameters

if (!Slot%in%c("lenStruc","ageStruc")) stop("Wrong 'Slot' parameter!!")

  #-----------------------------------------------------------------------------
  # Graphical parameters
  #-----------------------------------------------------------------------------

dots <- list(...)  

if (is.null(dots$lwd)) dots$lwd <- 1
if (is.null(dots$lty)) dots$lty <- 1
if (is.null(dots$pch)) dots$pch <- 21

if (is.null(dots$p.col)) dots$p.col <- "black"

if (is.null(dots$l.col)) dots$l.col <- "red"
if (is.null(dots$l.lwd)) dots$l.lwd <- 2

if (is.null(dots$cex)) dots$cex <- 0.8
if (is.null(dots$p.cex)) dots$p.cex <- 0.9
if (is.null(dots$k.cex)) dots$k.cex <- 1.2
if (is.null(dots$cex.lab)) dots$cex.lab <- 1.1
if (is.null(dots$cex.sub)) dots$cex.sub <- 1.1
if (is.null(dots$cex.axis)) dots$cex.axis <- 1
if (is.null(dots$cex.main)) dots$cex.main <- 1.2

if (is.null(dots$col.lab)) dots$col.lab <- "black"
if (is.null(dots$col.sub)) dots$col.sub <- "black"
if (is.null(dots$col.axis)) dots$col.axis <- "black"
if (is.null(dots$col.main)) dots$col.main <- "black"

if (is.null(dots$font)) dots$font <- 6
if (is.null(dots$font.lab)) dots$font.lab <- 7
if (is.null(dots$font.sub)) dots$font.sub <- 6
if (is.null(dots$font.axis)) dots$font.axis <- 7
if (is.null(dots$font.main)) dots$font.main <- 7

if (is.null(dots$rot)) dots$rot <- 90
if (is.null(dots$layout)) dots$layout <- NULL 
if (is.null(dots$as.table)) dots$as.table <- FALSE 

  #-----------------------------------------------------------------------------
  # Extraction of the numerical data, and formatting process
  #-----------------------------------------------------------------------------

tab <- slot(object,Slot)$rep
#NAs values are removed
tab <- tab[!is.na(tab$value),]
if (nrow(tab)==0) stop("no available data!!")
if (all(is.na(tab))) stop("no available data!!")
if (all(levels(factor(as.character(tab$time)))=="all")) tab$time <- NA
if (all(levels(factor(as.character(tab$space)))=="all")) tab$space <- NA
if (all(levels(factor(as.character(tab$technical)))=="all")) tab$technical <- NA

#nb Iter
B <- length(unique(tab$iter))              #added MM 15/03/2011


timeStrata <- spaceStrata <- techStrata <- TRUE
if (all(is.na(tab$time))) timeStrata <- FALSE
if (all(is.na(tab$space))) spaceStrata <- FALSE
if (all(is.na(tab$technical))) techStrata <- FALSE

#we get rid of unused levels
if (timeStrata) tab$time <- factor(tab$time)
if (spaceStrata) tab$space <- factor(tab$space)
if (techStrata) tab$technical <- factor(tab$technical)

vrbl <- switch(Slot,
               lenStruc="length",
               lenVar="length",
               ageStruc="age",
               ageVar="age")

val <- as.numeric(as.character(tab[,vrbl]))
if (!is.na(step)) {
tab[,vrbl] <- factor(as.character(val),levels=seq(min(val,na.rm=TRUE),max(val,na.rm=TRUE),by=step))
} else {
tab[,vrbl] <- factor(val)
}

tstSp <- !is.na(object@species)
tstCat <- !is.na(object@catchCat)

strip.col <- trellis.par.get("strip.background")$col
ic <- (all(is.na(probs)))

#tab of original estimates is distinguished
tab0 <- tab[tab$iter==0,] ; if (nrow(tab0)==0) origin <- FALSE
tab <- tab[tab$iter!=0,]

fieldSpec <- paste(vrbl,"=tab$",vrbl,sep="")
tab1 <- tab2 <- tab3 <- NULL
if (object@param%in%c("sex","length","weight","maturity")) {
  eval(parse('',text=paste("tab1 <- aggregate(tab$value,list(",
                              paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                              "),mean,na.rm=TRUE))",sep="")))
} else {
  eval(parse('',text=paste("tab1 <- aggregate(tab$value,list(",
                              paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                              "),function(x) mean(c(x,rep(0,length=B-length(x))),na.rm=TRUE))",sep="")))
}
names(tab1)[ncol(tab1)] <- "mean"

if (KurtSkew) {

  if (is.null(dots$col)) dots$col <- c("skyblue","violetred2")
  if (is.null(dots$xlab)) dots$xlab <- vrbl
  if (is.null(dots$ylab)) dots$ylab <- ""
  if (is.null(dots$main)) dots$main <- paste("Skewness and Kurtosis values from '",Slot,"' replicates \n",
    paste(c("for ",paste("\"",object@species,"\" species",sep="")," and ",
    paste("\"",object@catchCat,"\" fraction",sep=""))[c(tstSp|tstCat,tstSp,tstSp&tstCat,tstCat)],collapse=""),sep="")   

if (object@param%in%c("sex","length","weight","maturity")) {
  eval(parse('',text=paste("tab2 <- aggregate(tab$value,list(",        #skewness
                             paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                             "),function(x) sum( (x-mean(x,na.rm=TRUE))^3 / sqrt(var(x,na.rm=TRUE))^3 , na.rm=TRUE)/sum(!is.na(x)) )",sep="")))
} else {
  eval(parse('',text=paste("tab2 <- aggregate(tab$value,list(",        #skewness
                             paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                             "),function(x) sum( (c(x,rep(0,length=B-length(x)))-mean(c(x,rep(0,length=B-length(x))),na.rm=TRUE))^3 / sqrt(var(c(x,rep(0,length=B-length(x))),na.rm=TRUE))^3, na.rm=TRUE)/ (B-sum(is.na(x))) )",sep="")))
}
  names(tab2)[ncol(tab2)] <- "skewness"

if (object@param%in%c("sex","length","weight","maturity")) {
  eval(parse('',text=paste("tab3 <- aggregate(tab$value,list(",        #kurtosis
                             paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                             "),function(x) sum( (x-mean(x,na.rm=TRUE))^4 / var(x,na.rm=TRUE)^2, na.rm=TRUE)/sum(!is.na(x)) - 3 )",sep="")))
} else {  
  eval(parse('',text=paste("tab3 <- aggregate(tab$value,list(",        #kurtosis
                             paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                             "),function(x) sum( (c(x,rep(0,length=B-length(x)))-mean(c(x,rep(0,length=B-length(x))),na.rm=TRUE))^4 / var(c(x,rep(0,length=B-length(x))),na.rm=TRUE)^2, na.rm=TRUE)/ (B-sum(is.na(x))) - 3 )",sep="")))
}
  names(tab3)[ncol(tab3)] <- "kurtosis"

  TAB <- merge(tab1,tab2) ; TAB <- merge(TAB,tab3)
  TAB$skewness[is.nan(TAB$skewness)] <- NA
  TAB$kurtosis[is.nan(TAB$kurtosis)] <- NA

} else {

  if (is.null(dots$col)) dots$col <- "skyblue"
  if (is.null(dots$xlab)) dots$xlab <- vrbl
  if (is.null(dots$ylab)) dots$ylab <- Slot
  if (is.null(dots$main)) dots$main <- paste("Mean estimates from '",Slot,"' replicates \n",
    paste(c("for ",paste("\"",object@species,"\" species",sep="")," and ",
    paste("\"",object@catchCat,"\" fraction",sep=""))[c(tstSp|tstCat,tstSp,tstSp&tstCat,tstCat)],collapse=""),sep="")   

  if (!ic) {

if (object@param%in%c("sex","length","weight","maturity")) {
    eval(parse('',text=paste("tab2 <- aggregate(tab$value,list(",
                             paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                             "),function(x) quantile(x,probs=probs)[1])",sep="")))
} else {
    eval(parse('',text=paste("tab2 <- aggregate(tab$value,list(",
                             paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                             "),function(x) quantile(c(x,rep(0,length=B-length(x))),probs=probs)[1])",sep="")))
}
    names(tab2)[ncol(tab2)] <- "down"

if (object@param%in%c("sex","length","weight","maturity")) {
    eval(parse('',text=paste("tab3 <- aggregate(tab$value,list(",
                             paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                             "),function(x) quantile(x,probs=probs)[2])",sep="")))
} else {  
    eval(parse('',text=paste("tab3 <- aggregate(tab$value,list(",
                             paste(c("time=tab$time"[timeStrata],"space=tab$space"[spaceStrata],"technical=tab$technical"[techStrata],fieldSpec),sep="",collapse=","),
                             "),function(x) quantile(c(x,rep(0,length=B-length(x))),probs=probs)[2])",sep="")))
}
    names(tab3)[ncol(tab3)] <- "up"

    TAB <- merge(tab1,tab2) ; TAB <- merge(TAB,tab3)

  } else {

    TAB <- tab1
    TAB$down <- TAB$up <- TAB$mean

}

  if (origin) {
  
  TAB <- merge(TAB,tab0[,c("time"[timeStrata],"space"[spaceStrata],"technical"[techStrata],vrbl,"value")],all.x=TRUE)  #'value' is the raw estimate

  }

}

  #-----------------------------------------------------------------------------
  # Graphical display
  #-----------------------------------------------------------------------------

indStr <- c(timeStrata,spaceStrata,techStrata)
intLeg <- c(object@strataDesc@timeStrata,object@strataDesc@spaceStrata,object@strataDesc@techStrata)

if (all(is.na(intLeg))) intLeg <- c("time","space","technical") 

nTst <- sum(indStr)==0


if (KurtSkew) {

eval(parse('',text=paste("xyplot(skewness~",vrbl,paste("|",paste(c("time","space","technical")[indStr],collapse="*",sep=""),sep="")[!nTst],
  ",drop.unused.levels=FALSE,data=TAB,main=list(dots$main,font=dots$font.main,col=dots$col.main,cex=dots$cex.main),",
  "prepanel = function(x,y,subscripts,...) {Kurt <- TAB$kurtosis[subscripts] ; ",
                                           "if (length(subscripts)>0 & !all(is.na(c(y,Kurt)))) {list(ylim = range(c(0,y,Kurt),na.rm=TRUE))",
                                           "} else {",
                                           "list(ylim=c(0,1))}},", 
  "panel=function(x,y,subscripts,...){panel.xyplot(x,y,col=dots$col[1],fill=dots$col[1],lwd=dots$lwd[1],lty=dots$lty[1],pch=dots$pch[1],cex=dots$p.cex[1],...) ;",
                                     "panel.xyplot(x,TAB$kurtosis[subscripts],col=dots$col[2],fill=dots$col[2],lwd=dots$lwd[1],lty=dots$lty[1],pch=dots$pch[1],cex=dots$p.cex[1],...) ;",
                                     "panel.abline(h=0,lty=2)},",
  
  "par.strip.text=list(font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,",
  "scales=list(x=list(relation=\"free\",rot=dots$rot,cex=dots$cex),y=list(relation=\"free\",cex=dots$cex),font=dots$font.axis,col=dots$col.axis,cex=dots$cex.axis),",
  "ylab=list(dots$ylab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab),xlab=list(dots$xlab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab)",
  paste(",key=list(points=list(pch=c(rep(15,sum(indStr)),rep(dots$pch[1],2)),",
                         "cex=c(rep(dots$k.cex,sum(indStr)),rep(dots$p.cex[1],2)),",
                         "col=c(","strip.col[1:sum(indStr)],"[!nTst],"dots$col[1:2]),",                         
                         "fill=c(","strip.col[1:sum(indStr)],"[!nTst],"dots$col[1:2])),",
                         "text=list(c(intLeg[indStr],\"skewness\",\"kurtosis\")),",
                         "space=\"right\",font=dots$font.lab,columns=1,border=TRUE)",sep="")[dispKey],
  ")",sep=""))) 

} else {

eval(parse('',text=paste("barchart(mean~",vrbl,paste("|",paste(c("time","space","technical")[indStr],collapse="*",sep=""),sep="")[!nTst],
  ",horizontal=FALSE,drop.unused.levels=FALSE,data=TAB,main=list(dots$main,font=dots$font.main,col=dots$col.main,cex=dots$cex.main),",
  "prepanel = function(x,y,subscripts,...) {if (all(y==0)) y <- 1 ; ",
                                           "if (length(subscripts)>0) {list(ylim = c(0, 1.05*max(c(y,TAB$up[subscripts]",",TAB$value[subscripts]"[origin],"))))",
                                           "} else {",
                                           "list(ylim=c(0,1))}},", 
  "panel=function(x,y,subscripts,...){panel.barchart(x,y,col=dots$col,lwd=dots$lwd,lty=dots$lty,pch=dots$pch,cex=dots$p.cex,...) ; ",
                                     "if (origin) panel.xyplot(x,TAB$value[subscripts],col=dots$p.col,pch=dots$pch,cex=dots$p.cex) ;",
                                     "if (length(subscripts)>0 & !ic) panel.segments(x,TAB$down[subscripts],x,TAB$up[subscripts],col=dots$l.col[1],lwd=dots$l.lwd[1],lty=dots$lty[1])},",
  "fill=dots$col,par.strip.text=list(font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,",
  "scales=list(x=list(relation=\"free\",rot=dots$rot,cex=dots$cex),y=list(relation=\"free\",cex=dots$cex,axs=\"i\"),font=dots$font.axis,col=dots$col.axis,cex=dots$cex.axis),",
  "ylab=list(dots$ylab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab),xlab=list(dots$xlab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab),",
  ",key=list(points=list(pch=c(rep(15,sum(indStr))",",dots$pch"[origin],"),cex=c(rep(dots$k.cex,sum(indStr))",",dots$p.cex"[origin],
              "),col=c(strip.col[1:sum(indStr)]",",dots$p.col"[origin],")),text=list(c(intLeg[indStr]",",\"raw estimates\""[origin],")),space=\"right\",font=dots$font.lab,columns=1,border=TRUE)"[dispKey & !nTst],
  ",key=list(points=list(pch=dots$pch,cex=dots$p.cex,col=dots$p.col),text=list(\"raw estimates\"),space=\"right\",font=dots$font.lab,columns=1,border=TRUE)"[dispKey & nTst & origin],
  ")",sep=""))) 
}
                 
}




###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################



#############################################################################
#                                                                           #
# Levelplot of the correlation matrix between estimates at age (replicates) #
#                                                                           #
#############################################################################



 
dbeCorrPlotAge <- function(object,                               #'dbeOutput' object 
                           dispKey=TRUE,                         #if TRUE and if various panels are displayed, a describing key is displayed      
                           ...){                                 #further graphical parameters


  #-----------------------------------------------------------------------------
  # Graphical parameters
  #-----------------------------------------------------------------------------

dots <- list(...)  
lt <- get("lattice.theme", envir = lattice:::.LatticeEnv)

if (is.null(dots$col)) dots$col <- colorRampPalette(c("lightskyblue","white","darksalmon"))(100)    #to be modified if needed
if (is.null(dots$lwd)) dots$lwd <- 1
if (is.null(dots$lty)) dots$lty <- 1
if (is.null(dots$pch)) dots$pch <- 21

if (is.null(dots$cex)) dots$cex <- 0.8
if (is.null(dots$p.cex)) dots$p.cex <- 1.2
if (is.null(dots$k.cex)) dots$k.cex <- 1.2
if (is.null(dots$cex.lab)) dots$cex.lab <- 1.1
if (is.null(dots$cex.sub)) dots$cex.sub <- 1.1
if (is.null(dots$cex.axis)) dots$cex.axis <- 1
if (is.null(dots$cex.main)) dots$cex.main <- 1.2

if (is.null(dots$col.lab)) dots$col.lab <- "black"
if (is.null(dots$col.sub)) dots$col.sub <- "black"
if (is.null(dots$col.axis)) dots$col.axis <- "black"
if (is.null(dots$col.main)) dots$col.main <- "black"

if (is.null(dots$font)) dots$font <- 6
if (is.null(dots$font.lab)) dots$font.lab <- 7
if (is.null(dots$font.sub)) dots$font.sub <- 6
if (is.null(dots$font.axis)) dots$font.axis <- 7
if (is.null(dots$font.main)) dots$font.main <- 7

if (is.null(dots$rot)) dots$rot <- 90
if (is.null(dots$layout)) dots$layout <- NULL 
if (is.null(dots$as.table)) dots$as.table <- FALSE 

  #-----------------------------------------------------------------------------
  # Extraction of the numerical data, and formatting process
  #-----------------------------------------------------------------------------

tab <- object@ageStruc$rep      #ageStruc <--------
#iter=0 values are removed
tab <- tab[tab$iter!=0,]
#NAs values are removed
tab <- tab[!is.na(tab$value),]
if (nrow(tab)==0) stop("no available data!!")
if (all(is.na(tab))) stop("no available data!!")
if (all(levels(factor(as.character(tab$time)))=="all")) tab$time <- NA
if (all(levels(factor(as.character(tab$space)))=="all")) tab$space <- NA
if (all(levels(factor(as.character(tab$technical)))=="all")) tab$technical <- NA

timeStrata <- spaceStrata <- techStrata <- TRUE
if (all(is.na(tab$time))) timeStrata <- FALSE
if (all(is.na(tab$space))) spaceStrata <- FALSE
if (all(is.na(tab$technical))) techStrata <- FALSE

#we get rid of unused levels
if (timeStrata) tab$time <- factor(tab$time)
if (spaceStrata) tab$space <- factor(tab$space)
if (techStrata) tab$technical <- factor(tab$technical)
   
if (is.null(dots$xlab)) dots$xlab <- "" 
if (is.null(dots$ylab)) dots$ylab <- "Age"

tstSp <- !is.na(object@species)
tstCat <- !is.na(object@catchCat)
if (is.null(dots$main)) dots$main <- paste("Levelplot of correlation matrix between estimates at age \n",
    paste(c("for ",paste("\"",object@species,"\" species",sep="")," and ",
    paste("\"",object@catchCat,"\" fraction",sep=""))[c(tstSp|tstCat,tstSp,tstSp&tstCat,tstCat)],collapse=""),sep="")   

strip.col <- trellis.par.get("strip.background")$col

#calculation of stratified correlation matrix
tab$iter <- factor(as.numeric(as.character(tab$iter)))
tab$age <- factor(as.numeric(as.character(tab$age)))
#index per stratum
ind <- unique(tab[,c("time","space","technical")])
gh <- do.call("rbind",lapply(1:nrow(ind),function(x) {df <- merge(tab,ind[x,],all.y=TRUE) 
                                                      CorMat <- cor(tapply(df$value,list(df$iter,df$age),function(z) z))
                                                      mat <- as.data.frame(CorMat)
                                                      names(mat) <- paste("value.",names(mat),sep="")
                                                      dd <- reshape(mat,direction="long",varying=names(mat)) ; names(dd)[1] <- "X"
                                                      dd$Y <- rownames(mat) 
                                                      dd$time <- ind[x,"time"] ; dd$space <- ind[x,"space"] ; dd$technical <- ind[x,"technical"] 
                                                      return(dd)}))
gh$X <- as.numeric(as.character(gh$X))
gh$Y <- as.numeric(as.character(gh$Y))

  #-----------------------------------------------------------------------------
  # Graphical display
  #-----------------------------------------------------------------------------

indStr <- c(timeStrata,spaceStrata,techStrata)
intLeg <- c(object@strataDesc@timeStrata,object@strataDesc@spaceStrata,object@strataDesc@techStrata)

if (all(is.na(intLeg))) intLeg <- c("time","space","technical") 


nTst <- sum(indStr)==0

eval(parse('',text=paste("levelplot(value~X*Y",paste("|",paste(c("time","space","technical")[indStr],collapse="*",sep=""),sep="")[!nTst],
  ",data=gh,main=list(dots$main,font=dots$font.main,col=dots$col.main,cex=dots$cex.main),at=seq(-1,1,length=20),",
  "col.regions=dots$col,lwd=dots$lwd,lty=dots$lty,pch=dots$pch,cex=dots$p.cex,fill=dots$col,par.strip.text=list(font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,",
  "colorkey=list(at=seq(-1,1,length=20),col=dots$col,space=\"bottom\"),",
  "scales=list(x=list(relation=\"free\",rot=dots$rot,cex=dots$cex),y=list(relation=\"free\",cex=dots$cex),font=dots$font.axis,col=dots$col.axis,cex=dots$cex.axis),",
  "ylab=list(dots$ylab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab),xlab=list(dots$xlab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab),",
  "key=list(points=list(pch=15,cex=dots$k.cex,col=strip.col[1:sum(indStr)]),text=list(intLeg[indStr]),space=\"right\",font=dots$font.lab,columns=1,border=TRUE)"[dispKey & !nTst],
  ")",sep="")))                 

}



###############################################################################################
###############################################################################################
###############################      Methods to be exported      ##############################
###############################################################################################
###############################################################################################


setGeneric("dbePlotRep", function(object,
                               Slot,   
                               probs=c(0.05,0.95),
                               step=NA,    
                               origin=TRUE,                                                                       
                               dispKey=TRUE,            
                               KurtSkew=FALSE,             
                               ...){
standardGeneric("dbePlotRep")}
)


setMethod("dbePlotRep",signature(object="dbeOutput"),        
           function(object,                               #'dbeOutput' object 
                    Slot,                                 #ex: "totalW", "totalN", "lenStruc", "ageStruc"
                    probs=c(0.05,0.95),                   #if NA, no IC displayed
                    step=NA,                              #specified numerical step for length or age classes (if NA, no filled gaps) 
                    origin=TRUE,                          #if TRUE, raw estimates from the original dataset are added ('iter' value = 0)
                    dispKey=TRUE,                         #if TRUE and if various panels are displayed, a describing key is displayed
                    KurtSkew=FALSE,                       #display of 'skewness' and 'kurtosis' values per strata and per length or age class
                    ...){                                 #further graphical parameters

if (Slot%in%c("totalN","totalW")) {
  dbePlotRepDist(object=object,Slot=Slot,dispKey=dispKey,...)
} else {
  if (Slot%in%c("lenStruc","ageStruc")) {
    dbePlotRepLA(object=object,Slot=Slot,probs=probs,step=step,origin=origin,dispKey=dispKey,KurtSkew=KurtSkew,...)
   } else {
    stop("Wrong 'Slot' parameter!!")
   }
}
})

          
          
          

setGeneric("dbeCorrPlot", function(object,
                                   dispKey=TRUE,            
                                   ...){
standardGeneric("dbeCorrPlot")}
)


setMethod("dbeCorrPlot",signature(object="dbeOutput"),function(object,          #'dbeOutput' object 
                                                               dispKey=TRUE,    #if TRUE and if various panels are displayed, a describing key is displayed
                                                               ...){            #further graphical parameters
dbeCorrPlotAge(object=object,dispKey=dispKey,...)
})



          
          
          
################################################################################################
################################################################################################
################################      EXAMPLE      #############################################
################################################################################################
################################################################################################
#                                                                               
##------------------------------
## creation of object (example)
##------------------------------
#strDef <- strIni(timeStrata="quarter",techStrata="foCatEu5")
#obj <- new("dbeOutput",species="Solea solea",catchCat="LAN",strataDesc=strDef,methodDesc="analytical")
#newObj1 <- totVolume(obj,csDataCons(csDataVal(sole.cs),strDef),ceDataCons(ceDataVal(sole.ce),strDef))
##creation of 'iter' element for 'totalW' slot
#Rnorm <- abs(rnorm(250,1,1))
#newObj1@totalW$rep <- do.call("rbind",lapply(1:250,function(x) {
#                                         df <- newObj1@totalW$estim
#                                         df$iter <- x
#                                         df$value <- Rnorm[x]*df$value
#                                         return(df)}))
##creation of a 'lenStruc' slot
#norm <- dnorm(7:25,15,6)
#newObj1@lenStruc$estim <- do.call("rbind",lapply(1:19,function(x) {
#                                         df <- newObj1@totalW$estim
#                                         df$length <- as.character(6+x)
#                                         df$value <- norm[x]*df$value
#                                         return(df)}))
##creation of 'iter' element for 'lenStruc' slot
#Rnorm <- abs(rnorm(250,1,1))
#newObj1@lenStruc$rep <- do.call("rbind",lapply(1:250,function(x) {
#                                         df <- newObj1@lenStruc$estim
#                                         df$iter <- x
#                                         df$value <- abs(rnorm(nrow(df),4,1))*df$value
#                                         return(df)}))
#
#newObj1@lenStruc$rep$iter[newObj1@lenStruc$rep$iter==20] <- 0     #for 'origin' parameter
#
#################################################################################
#################################################################################
#
#windows(width=30,height=20)
#dbePlotRep(newObj1,"totalW")
#windows(width=30,height=20)
#dbePlotRep(newObj1,"lenStruc",probs=c(0.05,0.95))
#windows(width=30,height=20)
#dbePlotRep(newObj1,"lenStruc",KurtSkew=TRUE)
#
#
#
#                                                                               
##------------------------------
## for testing process, 'lenStruc$rep' element is inserted in 'ageStruc' slot
##------------------------------
#
#dat <- newObj1@lenStruc$rep
#names(dat)[5] <- "age"
#newObj1@ageStruc$rep <- dat
#windows()
#dbeCorrPlot(newObj1)       #warnings if sd = 0 
#                           #result should be better with real data!!
#                           
                           
                           




