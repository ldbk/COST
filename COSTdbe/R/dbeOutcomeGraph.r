

#library(COSTcore)
#library(lattice)
#source("C:/Documents and Settings/mmerzere/Bureau/outcomeObject.r")
# 

#############################################
#                                           #
# Graphical display from 'dbeOutput' object #
#                                           #
#############################################

        
setGeneric("dbePlot", function(object,
                               elmt,                                 
                               type="bar",                           
                               Xstratum=NULL,
                               step=NA,                        
                               dispKey=TRUE, 
                               indScale=FALSE,                        
                               ...){
standardGeneric("dbePlot")}
)



 
setMethod("dbePlot",signature(object="dbeOutput"),        
           function(object,                               #'dbeOutput' object 
                    elmt,                                 #ex: "lenStruc$estim"
                    type="bar",                           #to be chosen between "bar", "line", "point" 
                    Xstratum=NULL,                        #stratum displayed on x-axis if chosen slot is in c("nSamp","nMes","totalN","totalNvar","totalW","totalWvar")
                    step=NA,                              #length or age classes step (if NA, empty classes are not drawn)
                    dispKey=TRUE,                         #if TRUE and if various panels are displayed, a describing key is displayed
                    indScale=FALSE,                       #if TRUE, y-scale is free.  
                    ...){                                 #further graphical parameters
tab <- NULL
eval(parse('',text=paste("tab <- object@",elmt,sep=""))) 

if (!is.data.frame(tab)) stop("Wrong 'elmt' parameter!!")
if (!type%in%c("bar","point","line")) stop("Wrong 'type' parameter!!")
ciIndex <- FALSE ; if ("inf"%in%names(tab)) ciIndex <- TRUE



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

#indicates if elmt is in c("lenStruc","lenVar","ageStruc","ageVar") 
lStruc <- elmt%in%c("lenStruc$estim","lenVar","lenNum$ci","lenNum$cv","ageStruc$estim","ageVar","ageNum$ci","ageNum$cv")
vrbl <- ""

if (lStruc) {

vrbl <- "age" ; if (elmt%in%c("lenStruc$estim","lenVar","lenNum$ci","lenNum$cv")) vrbl <- "length"
              
#levels of 'vrbl' field must be defined as numerics
val <- as.numeric(as.character(tab[,vrbl]))
if (!is.na(step)) {
tab[,vrbl] <- factor(as.character(val),levels=seq(min(val,na.rm=TRUE),max(val,na.rm=TRUE),by=step))
} else {
tab[,vrbl] <- factor(val)
}

Xstratum <- NULL

if (is.null(dots$xlab)) dots$xlab <- vrbl 
} 


if (is.null(dots$ylab)) dots$ylab <- elmt
tstSp <- !is.na(object@species)
tstCat <- !is.na(object@catchCat)
if (is.null(dots$main)) dots$main <- paste("'",elmt,"' estimates \n",
    paste(c("for ",paste("\"",object@species,"\" species",sep="")," and ",
    paste("\"",object@catchCat,"\" fraction",sep=""))[c(tstSp|tstCat,tstSp,tstSp&tstCat,tstCat)],collapse=""),sep="")   

strip.col <- trellis.par.get("strip.background")$col

#according to 'type', the call will be quite different
plotFun <- switch(type,
                  bar="barchart",
                  line="xyplot",
                  point="xyplot")
typePar <- switch(type,
                 bar="",
                 line="type=\"l\",",
                 point="type=\"p\",") 


  #-----------------------------------------------------------------------------
  # Graphical display
  #-----------------------------------------------------------------------------

indStr <- c(timeStrata,spaceStrata,techStrata)
intLeg <- c(object@strataDesc@timeStrata,object@strataDesc@spaceStrata,object@strataDesc@techStrata)

if (all(is.na(intLeg))) intLeg <- c("time","space","technical") 

if (is.null(Xstratum)) { 

nTst <- sum(indStr)==0

eval(parse('',text=paste(plotFun,"(value ~ ",c("rep(0,nrow(tab))",vrbl)[c(!lStruc,lStruc)],paste("|",paste(c("time","space","technical")[indStr],collapse="*",sep=""),sep="")[!nTst],
  ",data=tab,horizontal=FALSE,",typePar,"drop.unused.levels=FALSE,"[lStruc & !is.na(step)],
  "main=list(dots$main,font=dots$font.main,col=dots$col.main,cex=dots$cex.main),","ylim=c(0,max(c(tab$value,tab$sup),na.rm=TRUE)*1.05),"[!indScale],
  "col=dots$col,lwd=dots$lwd,lty=dots$lty,pch=dots$pch,cex=dots$p.cex,fill=dots$col,par.strip.text=list(font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,",
  
  "prepanel = function(x,y,subscripts,...) {if (all(y==0)) y <- 1 ; "[indScale],
                                           "if (length(subscripts)>0) {list(ylim = c(0, 1.05*max(c(y,tab$sup[subscripts],tab$value[subscripts]),na.rm=TRUE)))"[indScale],
                                           "} else {"[indScale],
                                           "list(ylim=c(0,1))}},"[indScale], 
  "panel=function(x,y,subscripts,...){panel."[ciIndex | indScale],plotFun[ciIndex | indScale],"(x,y,...) ; "[ciIndex | indScale],
                                "if (length(subscripts)>0) panel.segments(x,tab$inf[subscripts],x,tab$sup[subscripts],col=dots$l.col[1],lwd=dots$l.lwd[1],lty=dots$lty[1])"[ciIndex],
                                 "},"[ciIndex | indScale],
  "scales=list(x=list(rot=dots$rot,cex=dots$cex",",draw=FALSE"[!lStruc],"),y=list(cex=dots$cex,","relation=\"free\","[indScale],"axs=\"i\"),font=dots$font.axis,col=dots$col.axis,cex=dots$cex.axis),",
  "xlab=list(dots$xlab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab),ylab=list(dots$ylab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab)",
  ",key=list(points=list(pch=15,cex=dots$k.cex,col=strip.col[1:sum(indStr)]),text=list(intLeg[indStr]),space=\"right\",font=dots$font.lab,columns=1,border=TRUE)"[dispKey & !nTst],
  ")",sep="")))                 

} else {

Str <- c("time","space","technical")[indStr] ; STR <- intLeg[indStr]
if (!Xstratum%in%Str) stop("Wrong 'Xstratum' parameter!!")
if (is.null(dots$xlab)) dots$xlab <- STR[match(Xstratum,Str)] 
newStr <- Str[-match(Xstratum,Str)] ; newSTR <- STR[-match(Xstratum,Str)]

indStr[match(Xstratum,c("time","space","technical"))] <- FALSE
nTst <- sum(indStr)==0

eval(parse('',text=paste(plotFun,"(value ~ ",Xstratum,paste("|",paste(newStr,collapse="*",sep=""),sep="")[!nTst],",data=tab,horizontal=FALSE,",typePar,
  "main=list(dots$main,font=dots$font.main,col=dots$col.main,cex=dots$cex.main),","ylim=c(0,max(c(tab$value,tab$sup),na.rm=TRUE)*1.05),"[!indScale],
  "col=dots$col,lwd=dots$lwd,lty=dots$lty,pch=dots$pch,cex=dots$p.cex,fill=dots$col,par.strip.text=list(font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,",
  
  "prepanel = function(x,y,subscripts,...) {if (all(y==0)) y <- 1 ; "[indScale],
                                           "if (length(subscripts)>0) {list(ylim = c(0, 1.05*max(c(y,tab$sup[subscripts],tab$value[subscripts]),na.rm=TRUE)))"[indScale],
                                           "} else {"[indScale],
                                           "list(ylim=c(0,1))}},"[indScale], 
  "panel=function(x,y,subscripts,...){panel."[ciIndex | indScale],plotFun[ciIndex | indScale],"(x,y,...) ; "[ciIndex | indScale],
                                "if (length(subscripts)>0) panel.segments(x,tab$inf[subscripts],x,tab$sup[subscripts],col=dots$l.col[1],lwd=dots$l.lwd[1],lty=dots$lty[1])"[ciIndex],  
                                "},"[ciIndex | indScale],
  "scales=list(x=list(rot=dots$rot,cex=dots$cex),y=list(cex=dots$cex,","relation=\"free\","[indScale],"axs=\"i\"),font=dots$font.axis,col=dots$col.axis,cex=dots$cex.axis),",
  "xlab=list(dots$xlab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab),ylab=list(dots$ylab,font=dots$font.lab,col=dots$col.lab,cex=dots$cex.lab)",
  ",key=list(points=list(pch=15,cex=dots$k.cex,col=strip.col[1:length(newStr)]),text=list(newSTR),space=\"right\",font=dots$font.lab,columns=1,border=TRUE)"[dispKey & !nTst],
  ")",sep=""))) 

}                
})



###############################################################################################
###############################################################################################
###############################      EXAMPLE      #############################################
###############################################################################################
###############################################################################################
                                                                               



###usage of 'dbePlot'
#source("C:/Documents and Settings/mmerzere/Bureau/CI_CV_dbeOutput.r") #loading 'object' created with code examples
#dbePlot(object,"lenStruc$estim",type="point",col="gold",pch=15,p.cex=1.7)
#dbePlot(object,"lenStruc$estim",col="gold")
#dbePlot(object,"lenVar",col="gold")
#dbePlot(object,"lenNum$ci",col="gold",indScale=TRUE)
#dbePlot(object,"ageStruc$estim",col="gold")
#dbePlot(object,"ageVar",col="gold",indScale=TRUE)         
#dbePlot(object,"ageNum$cv",col="gold",indScale=TRUE)
#dbePlot(object,"totalN$estim",col="gold")
#dbePlot(object,"totalNvar",col="gold",Xstratum="space")
#dbePlot(object,"totalNnum$ci",col="gold",Xstratum="space")
#dbePlot(object,"totalNnum$cv",col="gold",Xstratum="time",indScale=TRUE)
#




    