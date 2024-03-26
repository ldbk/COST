###########################################
##                                       ##
## Plots of Other biological  parameters ##
##                                       ##
##            MM 07/02/2008              ##
###########################################


###########################################
# scatterplot of individual weight~length #
###########################################

setGeneric("wlPlot", function(object,
                              species="all",
                              selection=FALSE,...){
	standardGeneric("wlPlot")}
)




setMethod("wlPlot", signature(object="csData"), function(object,
                                                         species="all",
                                                         selection=FALSE,...){

tab <- object@ca 
if (!"all"%in%species) tab <- tab[tab$spp%in%species,]

#tests on ca table fields
if (all(is.na(tab$indWt))) stop("no individual weight data in ca table!!")
if (all(is.na(tab$lenCls))) stop("no length class data in ca table!!") 

data(GraphsPar,envir=environment())                                                                                                               
dots <- list(...)
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Length(mm)"
if (is.null(dots$ylab)) 
  dots$ylab <- "Weight(g)" 
if (is.null(dots$main)) 
  dots$main <- "Scatter plot of individual weight at length" 

#missing length classes are taken into account
lenC <- c(1,5,10,25)
names(lenC) <- c("mm","scm","cm","25mm")
tab$lenCls <- factor(tab$lenCls,levels=seq(min(tab$lenCls),max(tab$lenCls),by=lenC[as.character(tab$lenCode[1])]))

if (selection){

  print(xyplot(indWt~lenCls,data=tab,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),
               ylab=list(dots$ylab,font=dots$font.lab),pch=dots$pch[1],col=dots$col[1],cex=dots$p.cex[1],fill=dots$p.bg[1],lwd=dots$p.lwd[1],
               scales=list(font=dots$font.axis,x=list(rot=dots$rot[1])),drop.unused.levels=FALSE))
  trellis.focus("panel",1,1)
  Reponse <- panel.identify()
  id.tab <- tab[Reponse,] 
  tabOcc <- paste(tab$indWt,tab$lenCls,sep=":::")
  idOcc <- paste(id.tab$indWt,id.tab$lenCls,sep=":::")
  invisible(list(l=Reponse,id.tab=tab[tabOcc%in%idOcc,]))
  
} else {

  xyplot(indWt~lenCls,data=tab,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),
         ylab=list(dots$ylab,font=dots$font.lab),pch=dots$pch[1],col=dots$col[1],cex=dots$p.cex[1],fill=dots$p.bg[1],lwd=dots$p.lwd[1],
         scales=list(font=dots$font.axis,x=list(rot=dots$rot[1])),drop.unused.levels=FALSE)
}
})



#############################################
# scatterplot of individual maturity~length #
#############################################


setGeneric("mlPlot", function(object,
                              species="all",
                              selection=FALSE,
                              ...){
	standardGeneric("mlPlot")}
)



setMethod("mlPlot", signature(object="csData"), function(object,
                                                         species="all",
                                                         selection=FALSE,
                                                         ...){
tab <- object@ca 
if (!"all"%in%species) tab <- tab[tab$spp%in%species,]
#tests on ca table fields
if (all(is.na(tab$matStage))) stop("no maturity stage data in ca table!!")
if (all(is.na(tab$lenCls))) stop("no length class data in ca table!!") 


data(GraphsPar,envir=environment())                                                                                                                  
dots <- list(...) 
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Length(mm)" 
if (is.null(dots$ylab)) 
  dots$ylab <- "Maturity"
if (is.null(dots$main)) 
  dots$main <- "Scatter plot of individual maturity at length"
 
tab$matStage <- factor(tab$matStage)
#missing length classes are taken into account
lenC <- c(1,5,10,25)
names(lenC) <- c("mm","scm","cm","25mm")
tab$lenCls <- factor(tab$lenCls,levels=seq(min(tab$lenCls),max(tab$lenCls),by=lenC[as.character(tab$lenCode[1])]))


if (selection){

  print(xyplot(matStage~lenCls,data=tab,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),
        ylab=list(dots$ylab,font=dots$font.lab),pch=dots$pch[1],col=dots$col[1],cex=dots$p.cex[1],fill=dots$p.bg[1],lwd=dots$p.lwd[1],
        scales=list(font=dots$font.axis,x=list(rot=dots$rot[1])),drop.unused.levels=FALSE))
  trellis.focus("panel",1,1)
  Reponse <- panel.identify()
  id.tab <- tab[Reponse,] 
  tabOcc <- paste(tab$matStage,tab$lenCls,sep=":::") ; idOcc <- paste(id.tab$matStage,id.tab$lenCls,sep=":::")
  invisible(list(l=Reponse,id.tab=tab[tabOcc%in%idOcc,]))

} else {

  xyplot(matStage~lenCls,data=tab,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),
         ylab=list(dots$ylab,font=dots$font.lab),pch=dots$pch[1],col=dots$col[1],cex=dots$p.cex[1],fill=dots$p.bg[1],lwd=dots$p.lwd[1],
         scales=list(font=dots$font.axis,x=list(rot=dots$rot[1])),drop.unused.levels=FALSE)
}
})



########################################
# scatterplot of individual sex~length #
########################################


setGeneric("slPlot", function(object,
                              species="all",
                              selection=FALSE,
                              ...){
	standardGeneric("slPlot")}
)



setMethod("slPlot", signature(object="csData"), function(object,
                                                         species="all",
                                                         selection=FALSE,
                                                         ...){

tab <- object@ca 
if (!"all"%in%species) tab <- tab[tab$spp%in%species,]

#tests on ca table fields
if (all(is.na(tab$sex))) stop("no sex data in ca table!!")
if (all(is.na(tab$lenCls))) stop("no length class data in ca table!!") 


data(GraphsPar,envir=environment())                                                                                                                  
dots <- list(...) 
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep="")))))
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Length(mm)"
if (is.null(dots$ylab)) 
  dots$ylab <- "Sex"
if (is.null(dots$main))
  dots$main <- "Scatter plot of individual sex at length"
 
tab$sex <- factor(as.character(tab$sex),exclude="U")
#missing length classes are taken into account
lenC <- c(1,5,10,25)
names(lenC) <- c("mm","scm","cm","25mm")
tab$lenCls <- factor(tab$lenCls,levels=seq(min(tab$lenCls),max(tab$lenCls),by=lenC[as.character(tab$lenCode[1])]))

if (selection){

  print(xyplot(sex~lenCls,data=tab,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),
        ylab=list(dots$ylab,font=dots$font.lab),pch=dots$pch[1],col=dots$col[1],cex=dots$p.cex[1],fill=dots$p.bg[1],lwd=dots$p.lwd[1],
        scales=list(font=dots$font.axis,x=list(rot=dots$rot[1])),drop.unused.levels=FALSE))
  trellis.focus("panel",1,1)
  Reponse <- panel.identify()
  id.tab <- tab[Reponse,] 
  tabOcc <- paste(tab$sex,tab$lenCls,sep=":::") ; idOcc <- paste(id.tab$sex,id.tab$lenCls,sep=":::")
  invisible(list(l=Reponse,id.tab=tab[tabOcc%in%idOcc,]))

} else {

  xyplot(sex~lenCls,data=tab,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),
         ylab=list(dots$ylab,font=dots$font.lab),pch=dots$pch[1],col=dots$col[1],cex=dots$p.cex[1],fill=dots$p.bg[1],lwd=dots$p.lwd[1],
         scales=list(font=dots$font.axis,x=list(rot=dots$rot[1])),drop.unused.levels=FALSE)
}
})



#######################################
# boxplot of individual weight~length #
#######################################



setGeneric("wlBoxplot", function(object,species="all",...){
	standardGeneric("wlBoxplot")}
)



setMethod("wlBoxplot", signature(object="csData"), function(object,species="all",...){

tab <- object@ca 
if (!"all"%in%species) tab <- tab[tab$spp%in%species,]

#tests on ca table fields
if (all(is.na(tab$indWt))) stop("no individual weight data in ca table!!")
if (all(is.na(tab$lenCls))) stop("no length class data in ca table!!") 

data(GraphsPar,envir=environment())                                                                                                           
dots <- list(...)
if (is.null(dots$pch)) dots$pch <- 20
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Length(mm)" 
if (is.null(dots$ylab)) 
  dots$ylab <- "Weight(g)"
if (is.null(dots$main)) 
  dots$main <- "Boxplot of individual weight at length"

#missing length classes are taken into account
lenC <- c(1,5,10,25)
names(lenC) <- c("mm","scm","cm","25mm")
tab$lenCls <- factor(tab$lenCls,levels=seq(min(tab$lenCls),max(tab$lenCls),by=lenC[as.character(tab$lenCode[1])]))

bwplot(indWt~lenCls,data=tab,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),
       ylab=list(dots$ylab,font=dots$font.lab),pch=dots$pch[1],cex=1.6,fill=dots$p.bg[1],scales=list(font=dots$font.axis,x=list(rot=dots$rot[1])),
       par.settings=list(box.rectangle=list(col=dots$col[1]),box.umbrella=list(col=dots$col[1],lty=dots$lty[1]),
       plot.symbol=list(col=dots$col[1])),drop.unused.levels=FALSE)
})





#########################################
# boxplot of individual length~maturity #
#########################################



setGeneric("mlBoxplot", function(object,species="all",...){
	standardGeneric("mlBoxplot")}
)



setMethod("mlBoxplot", signature(object="csData"), function(object,species="all",...){

tab <- object@ca
if (!"all"%in%species) tab <- tab[tab$spp%in%species,]

#tests on ca table fields
if (all(is.na(tab$matStage))) stop("no maturity stage data in ca table!!")
if (all(is.na(tab$lenCls))) stop("no length class data in ca table!!") 


data(GraphsPar,envir=environment())                                                                                                           
dots <- list(...)
if (is.null(dots$pch)) dots$pch <- 20
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Length(mm)"
if (is.null(dots$ylab)) 
  dots$ylab <- "Maturity"
if (is.null(dots$main)) 
  dots$main <- "Boxplot of individual maturity at length"

tab$matStage <- factor(tab$matStage)
#missing length classes are taken into account
lenC <- c(1,5,10,25)
names(lenC) <- c("mm","scm","cm","25mm")
tab$lenCls <- factor(tab$lenCls,levels=seq(min(tab$lenCls),max(tab$lenCls),by=lenC[as.character(tab$lenCode[1])]))

bwplot(as.numeric(as.character(matStage))~lenCls,data=tab,horizontal=FALSE,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),
       ylab=list(dots$ylab,font=dots$font.lab),pch=dots$pch[1],cex=2,fill=dots$p.bg[1],scales=list(font=dots$font.axis,x=list(rot=dots$rot[1])),
       par.settings=list(box.rectangle=list(col=dots$col[1]),box.umbrella=list(col=dots$col[1],lty=dots$lty[1]),
       plot.symbol=list(col=dots$col[1])),drop.unused.levels=FALSE)
})




####################################
# boxplot of individual length~sex #
####################################



setGeneric("slBoxplot", function(object,species="all",...){
	standardGeneric("slBoxplot")}
)


setMethod("slBoxplot", signature(object="csData"), function(object,species="all",...){

tab <- object@ca
if (!"all"%in%species) tab <- tab[tab$spp%in%species,]

#tests on ca table fields
if (all(is.na(tab$sex))) stop("no sex data in ca table!!")
if (all(is.na(tab$lenCls))) stop("no length class data in ca table!!") 

data(GraphsPar,envir=environment())                                                                                                                  
dots <- list(...)
if (is.null(dots$pch)) dots$pch <- 20
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab))
  dots$xlab <- "Length(mm)"
if (is.null(dots$ylab)) 
  dots$ylab <- "Sex"
if (is.null(dots$main)) 
  dots$main <- "Boxplot of individual sex at length"

tab$sex <- factor(as.character(tab$sex),exclude="U")
#missing length classes are taken into account
lenC <- c(1,5,10,25)
names(lenC) <- c("mm","scm","cm","25mm")
tab$lenCls <- factor(tab$lenCls,levels=seq(min(tab$lenCls),max(tab$lenCls),by=lenC[as.character(tab$lenCode[1])]))

bwplot(sex~as.numeric(as.character(lenCls)),data=tab,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),
       ylab=list(dots$ylab,font=dots$font.lab),pch=dots$pch[1],cex=2,fill=dots$p.bg[1],scales=list(font=dots$font.axis,x=list(rot=dots$rot[1])),
       par.settings=list(box.rectangle=list(col=dots$col[1]),box.umbrella=list(col=dots$col[1],lty=dots$lty[1]),
       plot.symbol=list(col=dots$col[1])),drop.unused.levels=FALSE)
})


##############################################################################
##############################################################################
######################### Methods to export ##################################
##############################################################################
##############################################################################


########################
# Specific plot.design #
########################

setGeneric("csPlot.design", function(object,species="all",...){
	standardGeneric("csPlot.design")}
)


setMethod("csPlot.design", signature(object="csDataVal"), function(object,species="all",...){
 
tab <- ca(object)
if (!"all"%in%species) tab <- tab[tab$spp%in%species,]
tab$age <- factor(tab$age,exclude=NA)
tab$matStage <- factor(tab$matStage,exclude=NA)
tab$sex <- factor(as.character(tab$sex),exclude="U")
tab$lenCls <- as.numeric(as.character(tab$lenCls))

des.tab <- tab[,c("age","matStage","sex","lenCls","indWt")]
#index of 'empty' columns
test <- apply(des.tab,2,function(x) all(is.na(x)))
des.tab <- des.tab[,!test]

if (ncol(des.tab)==0) stop("no data to be used in object!!")

plot.design(des.tab,...) 
})


setMethod("csPlot.design", signature(object="csData"), function(object,species="all",...){
 
tab <- ca(object)
if (!"all"%in%species) tab <- tab[tab$spp%in%species,]
tab$age <- factor(tab$age,exclude=NA)
tab$matStage <- factor(tab$matStage,exclude=NA)
tab$sex <- factor(as.character(tab$sex),exclude="U")
tab$lenCls <- as.numeric(as.character(tab$lenCls))

des.tab <- tab[,c("age","matStage","sex","lenCls","indWt")]
#index of 'empty' columns
test <- apply(des.tab,2,function(x) all(is.na(x)))
des.tab <- des.tab[,!test]

if (ncol(des.tab)==0) stop("no data to be used in object!!")

plot.design(des.tab,...) 
})


#################
# Bio Par plots #
#################


setGeneric("bioPar.plot", function(object,
                                   type="wl",     #or "ml" or "sl"
                                   species="all",
                                   selection=FALSE,...){
	standardGeneric("bioPar.plot")}
)


setMethod("bioPar.plot", signature(object="csData"), function(object,
                                                              type="wl",
                                                              species="all",
                                                              selection=FALSE,
                                                              ...){
eval(parse('',text=paste(type,"Plot(object,species=species,selection=selection,...)",sep="")))
})


setGeneric("bioPar.boxplot", function(object,
                                      type="wl",     #or "ml" or "sl"
                                      species="all",
                                      ...){
	standardGeneric("bioPar.boxplot")}
)


setMethod("bioPar.boxplot", signature(object="csData"), function(object,
                                                                 type="wl",
                                                                 species="all",
                                                                 ...){
eval(parse('',text=paste(type,"Boxplot(object,species=species,...)",sep="")))
})




