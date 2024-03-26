##################################################################
##                                                              ##
## Plots of Volume of Landings/Discards per FO/Fishing Day/Trip ##
##                                                              ##
##                      MM 07/02/2008                           ##
##################################################################



                                                       
#-------------------------------------------------------------------------------
# Calculation from cs datasets of volume (weights) of landings/discards per haul/fd/trip
#-------------------------------------------------------------------------------


landisVolumeFun <- function(object,         #csData object
                            species,        #species to specify
                            fraction,       #"LAN" or "DIS"
                            timeStrata,     #from hh
                            spaceStrata,    #from hh
                            techStrata,     #from hh 
                            sampPar=TRUE,   #'sampPar' checks if given species is considered as automatically sampled (if TRUE, sppReg=Par <-> sppReg=All, i.e. includes sppReg="Par" in the analysis )
                            ...){ 

fraction <- toupper(fraction)                                                   #
object@sl$catchCat <- toupper(object@sl$catchCat)                                                 # MM 29/04/2010
object@hl$catchCat <- toupper(object@hl$catchCat)                                                 #
object@ca$catchCat <- toupper(object@ca$catchCat)                                                 #


M_ik <- y_ikj <- m_ik <- NULL

if (is.na(timeStrata)) timeStrata <- NULL
if (is.na(spaceStrata)) spaceStrata <- NULL
if (is.na(techStrata)) techStrata <- NULL

##only sea sampling data is kept                                                # modif 26/01/2009 : all data is kept          
op.sub <- object@hh 
#  op.sub <- op.sub[op.sub$sampType=="S",]                                      #
capt.sub <- object@sl  
#  capt.sub <- capt.sub[capt.sub$sampType=="S",]                                #
#if (nrow(op.sub)==0) stop("no sea sampling data!!")                            #

#if species parameter is missing, one species from sl table has to be chosen
if (missing(species)) {
  un <- unique(as.character(object@sl$spp))
  un <- un[!is.na(un)]  
  if (length(un)>1) {
    warning("Several species in SL table!! All will be taken into account!")}
  species <- un} 

#restriction of data to specified fraction and species
capt.sub <- capt.sub[capt.sub$catchCat%in%fraction,]                                                                                                                                              
capt.sub <- capt.sub[capt.sub$spp%in%species,]                                                                                                                                              

#trpCode, staNum & date are converted to factors                                                                                                                                                                  
op.sub$trpCode <- factor(op.sub$trpCode)
op.sub$date <- factor(op.sub$date)
op.sub$staNum <- factor(op.sub$staNum)

#If timeStrata="semester", "quarter" or "month", field must be put in HH
if (!is.null(timeStrata)) {
  HHmonth <- as.numeric(sapply(op.sub$date,function(x) strsplit(as.character(x),"-")[[1]][2]))
  if (timeStrata=="month") 
    op.sub$month <- HHmonth
  if (timeStrata=="quarter") 
    op.sub$quarter <- ceiling(HHmonth/3)
  if (timeStrata=="semester") 
    op.sub$semester <- ceiling(HHmonth/6) 
}


#stratification fields are also to be factors
if (!is.null(timeStrata)) 
  op.sub[,timeStrata] <- factor(op.sub[,timeStrata])    
if (!is.null(techStrata)) 
  op.sub[,techStrata] <- factor(op.sub[,techStrata])
if (!is.null(spaceStrata)) 
  op.sub[,spaceStrata] <- factor(op.sub[,spaceStrata])
 


#Number of sampled fishing days by trip, tech,time,space                                                               
expr1 <- paste(",op.sub$",c(timeStrata,techStrata,spaceStrata),sep="",collapse="")
if (expr1==",op.sub$") 
  expr1 <- ""      
           
expr2 <- paste(",tabl1$",c(timeStrata,techStrata,spaceStrata),sep="",collapse="")
if (expr2==",tabl1$") 
  expr2 <- ""                 


#M_ik = total number of FOs by fishing day, by trip, by tech,time,space 
eval(parse('',text=paste("M_ik <- tapply(op.sub$staNum,list(op.sub$trpCode,op.sub$date",expr1,"),function(x) length(unique(x)))",sep="")))


if (fraction=="LAN") {
  fract <- "Lan" 
} else {
  fract <- "Dis" }


  #-----------------------------------------------------------------------------
  # Sampled weight index (Windex)
  #-----------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # A haul is considered as sampled (weights) for a given species and a given fraction if :
    #   1) (catchReg=="All") OR (catchReg==frac)
    #   AND
    #   2) (sppReg=="All") OR (sppReg=="Par" AND sampPar=TRUE)
    #---------------------------------------------------------------------------

#hh-index for sampled(1/0)/non sampled(NA) haul (weights) will be the combination of 2 indexes
indexCat <- indexSpp <- rep(0,nrow(op.sub))
#indexCat==1 if catReg=="All" or frac
indexCat[op.sub$catReg%in%c("All",fract)] <- 1
#indexSpp==1 if sppReg=="All" or if sppReg=="Par" & sampPar==TRUE
capt.sub$ind <- 1 ; indSpp <- merge(op.sub,unique(capt.sub[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode","staNum","ind")]),all.x=TRUE,sort=FALSE)$ind     #indSpp <-> index of hauls with related information in sl for given species and fraction
#indexSpp[op.sub$sppReg=="All" | (op.sub$sppReg=="Par" & sampPar)] <- 1
indexSpp[op.sub$sppReg=="All" | (op.sub$sppReg=="Par" & (sampPar | (!is.na(indSpp)) ))] <- 1
#so, Windex = indexCat*indexSpp (sampled haul index)
Windex <- indexCat*indexSpp
indZero <- (Windex==1) & (is.na(indSpp))    #indZero : index of sampled hauls with 0 values 
#finally,...
Windex[Windex==0] <- NA ; Windex[indZero] <- 0

#'ind2', the valid sampling indicator, is built   
Windex[op.sub$foVal!="V"] <- NA
op.sub$ind <- Windex


#m_ik = Number of sampled FOs by fishing day, by trip, by tech,time,space 
eval(parse('',text=paste("m_ik <- tapply(!is.na(Windex),list(op.sub$trpCode,op.sub$date",expr1,"),sum)",sep="")))          


#essentially to homogenize vectors sizes 
tabl1 <- merge(op.sub,aggregate(capt.sub$wt,list(sampType=capt.sub$sampType,
                                                  landCtry=capt.sub$landCtry,
                                                  vslFlgCtry=capt.sub$vslFlgCtry,
                                                  year=capt.sub$year,
                                                  proj=capt.sub$proj,
                                                  trpCode=capt.sub$trpCode,
                                                  staNum=capt.sub$staNum),
                                              sum),
               all.x=TRUE)                

names(tabl1)[ncol(tabl1)] <- "wt"    
tabl1$wt[is.na(tabl1$wt)] <- 0    #this means that species is absent (or not sampled)                                            
  
#ind is the sampling indicator in tabl1 (~Windex) 
tabl1 <- tabl1[!is.na(tabl1$ind),]  

#ADDED 10/10/2008 : if 'aggLev' = T, all FOs are considered to be sampled,so M_ik data is put in m_ik for these trips ###############################################################
comma <- paste(rep(",",length(dim(M_ik))-2),collapse="",sep="")                                                                                                                     #
if (any(tabl1$aggLev%in%"T")) {                                                                                            #modif 09/12/2008                                                         #
 eval(parse('',text=paste("m_ik[as.character(tabl1$trpCode)[tabl1$aggLev%in%\"T\"],",comma,"] <- M_ik[as.character(tabl1$trpCode)[tabl1$aggLev%in%\"T\"],",comma,"]",sep="")))      #
}                                                                                                                                                                                   #

#y_ikj = Total sampled weight by fishing day, by trip, by tech,time,space   
eval(parse('',text=paste("y_ikj <- tapply(tabl1$wt,list(tabl1$trpCode,tabl1$date",expr2,"),sum,na.rm=TRUE)",sep="")))
  
#y_ikj_hat = Weight of each sample by fishing day and by trip                                                                                          
y_ikj_hat <- split(tabl1$wt,paste(tabl1$trpCode,tabl1$date,sep=":-:"),drop=TRUE)

#y_ik = Mean weight of samples by fishing day and by trip 
y_ik <- unlist(lapply(y_ikj_hat,mean))

#y_IK = Raised sampled weight by fishing day, trip, time, space and tech
y_IK <- M_ik*y_ikj/m_ik

#MAT is built to define groupings for y_ik_hat
ll <- sum(!is.null(techStrata),!is.null(timeStrata),!is.null(spaceStrata))
indic <- ll+2
val <- expand.grid(dimnames(y_IK)[1:indic])
#  valChar <- apply(val[,-2,drop=FALSE],1,function(x) paste(as.character(x),collapse=":-:"))    
#  MAT <- array(valChar,dim=dim(y_IK))
#  MAT[is.na(y_IK)] <- NA
valChar <- apply(val[!is.na(y_IK),-2,drop=FALSE],1,function(x) paste(as.character(x),collapse=":-:")) 
MAT <- array(y_IK,dim(y_IK))
MAT[!is.na(MAT)] <- valChar



#y_ik_hat = Raised Weight for each fishing day by trip   
y_ik_hat <- split(y_IK,MAT,drop=TRUE) 

#y_ik_hat = Mean Raised Weight for fishing days by trip  
y_i <- unlist(lapply(y_ik_hat,mean))


result <- list(fraction=fraction,species=species,timeStrata=timeStrata,techStrata=techStrata,spaceStrata=spaceStrata,
               VolFO_FDTR=y_ikj_hat,MeanFO_FDTR=y_ik,VolFD_TR=y_ik_hat,MeanFD_TR=y_i)

return(new("edaResult",desc="landisVol",outPut=result))

}  


################################################################################
################################################################################




#-------------------------------------------------------------------------------
# Plots of MeanFD_TR values
#-------------------------------------------------------------------------------

fdPlot <- function(x,
                   groups=NULL,
                   ...){       

stratas <- c("timeStrata","techStrata","spaceStrata")
timeStrata <- x@outPut$timeStrata
techStrata <- x@outPut$techStrata
spaceStrata <- x@outPut$spaceStrata 

index <- c(timeStrata,techStrata,spaceStrata)

  #-----------------------------------------------------------------------------
  # Update of graphical parameters
  #-----------------------------------------------------------------------------

data(GraphsPar,envir=environment())                                                                                                       
dots <- list(...)
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Trip Code" 
if (is.null(dots$ylab)) 
  dots$ylab <- "Weight (g)" 
if (is.null(dots$layout)) dots$layout <- NULL 
if (is.null(dots$as.table)) dots$as.table <- FALSE 


if (all(is.null(timeStrata),is.null(techStrata),is.null(spaceStrata))) {
  if (is.null(dots$main)) 
    dots$main <- paste("Mean Weight by Fishing Day for each Trip\nSpecies :",paste(x@outPut$species,collapse=", "),"    Fraction :",paste(x@outPut$fraction,collapse=", "))
  df <- data.frame(trp=names(x@outPut$MeanFD_TR),bb=as.numeric(x@outPut$MeanFD_TR))

  print(xyplot(bb~trp,data=df,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),
         scales=list(font=dots$font.axis,x=list(rot=dots$rot)),pch=dots$pch[1],fill=dots$p.bg[1],cex=dots$p.cex[1],col=dots$col[1]))
                     
} else {    

  if (is.null(dots$main)) 
    dots$main <- paste("Mean Weight by Fishing Day for each Trip\nSpecies :",
                       paste(x@outPut$species,collapse=", "),"    Fraction :",paste(x@outPut$fraction,collapse=", "),"\n",
                       paste("Time Strata :",timeStrata)[!is.null(timeStrata)],
                       paste("   Technical Strata :",techStrata)[!is.null(techStrata)],
                       paste("   Space Strata :",spaceStrata)[!is.null(spaceStrata)])
                       
datas <-  x@outPut$MeanFD_TR
df <- as.data.frame(do.call("rbind",lapply(names(datas),function(x) strsplit(x,":-:")[[1]])))    
names(df) <- c("trp",timeStrata,techStrata,spaceStrata)
df$bb <- as.numeric(x@outPut$MeanFD_TR)

strip.col <- trellis.par.get("strip.background")$col

  #-----------------------------------------------------------------------------
  # Graphical display 
  #-----------------------------------------------------------------------------

      
if (is.null(groups)) {
          
  eval(parse('',text=paste("print(xyplot(bb~trp|",paste(index,collapse="*"),",data=df,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,",
                           "ylab=list(dots$ylab,font=dots$font.lab),par.strip.text=list(font=dots$font.lab),scales=list(font=dots$font.axis,x=list(relation=\"free\",rot=dots$rot,cex=dots$cex.lab[1])),",
                           "key=list(points=list(pch=15,cex=dots$p.cex[1],col=strip.col[1:length(index)]),text=list(index),font=dots$font.lab,columns=1,border=TRUE,space=\"right\"),",
                           "prepanel=function(x,y,...){x <- x[,drop=TRUE] ; prepanel.default.xyplot(x,y,...)},",
                           "panel = function(x,y,...){x <- x[,drop=TRUE] ; panel.xyplot(x,y,pch=dots$pch[1],fill=dots$p.bg[1],cex=dots$p.cex[1],col=dots$col[1],...)}))",sep="")))

} else {

  indexStr <- index[!index%in%eval(parse('',text=groups))]
  l1 <- length(indexStr)
  LEV <- levels(df[,eval(parse('',text=groups))])
  l2 <- length(LEV)
  groups <- eval(parse('',text=groups))

  eval(parse('',text=paste("print(xyplot(bb~trp",paste("|",paste(indexStr,collapse="*"),sep="")[l1>0],",data=df,groups=",groups,",main=list(dots$main,font=dots$font.main),layout=dots$layout,as.table=dots$as.table,",
                           "xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),scales=list(font=dots$font.axis,x=list(relation=\"free\",rot=dots$rot,cex=dots$cex.lab[1])),",
                           "key=list(points=list(pch=c(rep(dots$pch[1],l2),NA,",c("rep(15,l1)","NA")[c((l1>0),(l1==0))],"),fill=dots$p.bg[1:l2],cex=dots$p.cex[1],lwd=dots$p.lwd[1],",
                           "col=c(rep(dots$col[1],l2),NA",",strip.col[1:l1]"[l1>0],")),text=list(c(LEV,\"\",\"",paste(indexStr,collapse="\",\""),"\")),title=\"",groups,"\",",
                           "cex.title=0.8,space=\"right\",font=dots$font.lab,columns=1,border=TRUE),par.strip.text=list(font=dots$font.lab),",
                           "prepanel=function(x,y,...){x <- x[,drop=TRUE] ; prepanel.default.xyplot(x,y,...)},",
                           "panel = function(x,y,...){x <- x[,drop=TRUE] ; panel.xyplot(x,y,pch=dots$pch[1],fill=dots$p.bg","[1]"[l2==0],",cex=dots$p.cex[1],col=dots$col[1],...)}))",sep="")))
}}
}


################################################################################
################################################################################


#-------------------------------------------------------------------------------
# Boxplots of VolFD_TR values
#-------------------------------------------------------------------------------

fdBoxplot <- function(x,...){       

stratas <- c("timeStrata","techStrata","spaceStrata")
timeStrata <- x@outPut$timeStrata
techStrata <- x@outPut$techStrata
spaceStrata <- x@outPut$spaceStrata 

index <- c(timeStrata,techStrata,spaceStrata)
  
data(GraphsPar,envir=environment())                                                                                                        
dots <- list(...)

if (is.null(dots$pch)) 
  dots$pch <- 19

sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Trip Code" 
if (is.null(dots$ylab)) 
  dots$ylab <- "Weight (g)" 
if (is.null(dots$layout)) dots$layout <- NULL 
if (is.null(dots$as.table)) dots$as.table <- FALSE 


if (all(is.null(timeStrata),is.null(techStrata),is.null(spaceStrata))) {

  if (is.null(dots$main)) 
    dots$main <- paste("Weight by Fishing Day for each Trip\nSpecies :",
                       paste(x@outPut$species,collapse=", "),"    Fraction :",paste(x@outPut$fraction,collapse=", "))

  obj <- x@outPut$VolFD_TR 
  names(obj) <- sapply(names(x@outPut$VolFD_TR),function(x) strsplit(x,":-:")[[1]][1])    
  vec <- unlist(x@outPut$VolFD_TR)
  nvec <- unlist(lapply(x@outPut$VolFD_TR,length)) 
  df <- data.frame(aa=rep(names(obj),nvec),bb=vec)

  print(bwplot(bb~aa,data=df,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),
               pch=dots$pch[1],fill=dots$p.bg[1],scales=list(font=dots$font.axis,x=list(rot=dots$rot)),layout=dots$layout,as.table=dots$as.table,
               par.settings=list(box.rectangle=list(col=dots$col[1]),box.umbrella=list(col=dots$col[1],lty=dots$lty[1]),
               plot.symbol=list(col=dots$col[1]))))

} else {

  if (is.null(dots$main)) 
    dots$main <- paste("Weight by Fishing Day for each Trip\nSpecies :",
                       paste(x@outPut$species,collapse=", "),"    Fraction :",paste(x@outPut$fraction,collapse=", "),"\n",
                       paste("Time Strata :",timeStrata)[!is.null(timeStrata)],
                       paste("   Technical Strata :",techStrata)[!is.null(techStrata)],
                       paste("   Space Strata :",spaceStrata)[!is.null(spaceStrata)])
                       
datas <-  x@outPut$VolFD_TR
df <- as.data.frame(do.call("rbind",lapply(rep(names(datas),lapply(datas,length)), function(x) strsplit(x,":-:")[[1]])))    
names(df) <- c("trp",timeStrata,techStrata,spaceStrata)
df$bb <- as.numeric(unlist(datas))

strip.col <- trellis.par.get("strip.background")$col

eval(parse('',text=paste("print(bwplot(bb~trp|",paste(index,collapse="*"),",data=df,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,",
                          "ylab=list(dots$ylab,font=dots$font.lab),par.strip.text=list(font=dots$font.lab),scales=list(font=dots$font.axis,x=list(relation=\"free\",rot=dots$rot,cex=dots$cex.lab[1])),",
                          "key=list(points=list(pch=15,cex=dots$p.cex[1],col=strip.col[1:length(index)]),text=list(index),font=dots$font.lab,columns=1,border=TRUE,space=\"right\"),",
                          "prepanel=function(x,y,...){x <- x[,drop=TRUE] ; prepanel.default.bwplot(x,y,...)},",
                          "panel = function(x,y,...){x <- x[,drop=TRUE] ; panel.bwplot(x,y,pch=dots$pch[1],fill=dots$p.bg[1],cex=dots$p.cex[1],col=dots$col[1],",
                          "par.settings=list(box.rectangle=list(col=dots$col[1]),box.umbrella=list(col=dots$col[1],lty=dots$lty[1]),plot.symbol=list(col=dots$col[1])),...)}))",sep="")))

}
}






################################################################################
################################################################################


#-------------------------------------------------------------------------------
# Plots of MeanFO_FDTR values
#-------------------------------------------------------------------------------


foPlot <- function(x,...){       

data(GraphsPar,envir=environment())                                                                                                   
dots <- list(...)
if (is.null(dots$rot)) 
  dots$rot <- 0
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))

if (is.null(dots$xlab)) 
  dots$xlab <- "Fishing day" 
if (is.null(dots$ylab)) 
  dots$ylab <- "Weight (g)" 
if (is.null(dots$main)) 
  dots$main <- paste("Mean Weight by FO for each Fishing day and Trip\nSpecies :",
                     paste(x@outPut$species,collapse=", "),"    Fraction :",paste(x@outPut$fraction,collapse=", "))
if (is.null(dots$layout)) dots$layout <- NULL 
if (is.null(dots$as.table)) dots$as.table <- FALSE 
 
mat <- t(sapply(names(x@outPut$MeanFO_FDTR),function(x) strsplit(x,":-:")[[1]]))       

df <- data.frame(trpCode=mat[,1],date=mat[,2],Fday=as.numeric(unlist(tapply(mat[,1],list(mat[,1]),function(x) 1:length(x)))),val=as.numeric(x@outPut$MeanFO_FDTR))
   
print(xyplot(val~Fday|trpCode,data=df,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),
             scales=list(font=dots$font.axis,x=list(rot=dots$rot,cex=dots$cex.lab[1])),par.strip.text=list(font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,
             pch=dots$pch[1],fill=dots$p.bg[1],cex=dots$p.cex[1],col=dots$col[1]))
}


################################################################################
################################################################################

#-------------------------------------------------------------------------------
# Boxplots of VolFO_FDTR values
#-------------------------------------------------------------------------------


foBoxplot <- function(x,...){       

data(GraphsPar,envir=environment())                                                                                                        
dots <- list(...)
if (is.null(dots$rot)) 
  dots$rot <- 0 
if (is.null(dots$pch)) 
  dots$pch <- 19
  
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Fishing day"
if (is.null(dots$ylab)) 
  dots$ylab <- "Weight (g)" 
if (is.null(dots$main)) 
  dots$main <- paste("Weight by Fishing Operation for each Fishing day and Trip\nSpecies :",
                     paste(x@outPut$species,collapse=", "),"    Fraction :",paste(x@outPut$fraction,collapse=", "))
if (is.null(dots$layout)) dots$layout <- NULL 
if (is.null(dots$as.table)) dots$as.table <- FALSE 

vec <- unlist(x@outPut$VolFO_FDTR)
nvec <- unlist(lapply(x@outPut$VolFO_FDTR,length))
nvec2 <- rep(names(nvec),nvec)
mat <- t(sapply(nvec2,function(x) strsplit(x,":-:")[[1]])) 

nvec3 <- sapply(unique(nvec2),function(x) strsplit(x,":-:")[[1]][1])    
ind1 <- as.numeric(unlist(tapply(nvec3,list(nvec3),function(x) 1:length(x))))
ind2 <- as.numeric(unlist(tapply(nvec2,list(nvec2),length)))

df <- data.frame(trpCode=mat[,1],date=mat[,2],Fday=rep(ind1,ind2),val=vec)
df$Fday <- as.factor(df$Fday)

print(bwplot(val~Fday|trpCode,data=df,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),layout=dots$layout,as.table=dots$as.table,
             pch=dots$pch[1],fill=dots$p.bg[1],scales=list(font=dots$font.axis,x=list(rot=dots$rot,cex=dots$cex.lab[1])),par.strip.text=list(font=dots$font.lab),
             par.settings=list(box.rectangle=list(col=dots$col[1]),box.umbrella=list(col=dots$col[1],lty=dots$lty[1]),
             plot.symbol=list(col=dots$col[1]))))
}


plotVol <- function(x,type,...){        #type="FD" or "FO"
if (missing(type)) type <- "FD"
if (type=="FD") {
  fdPlot(x,...)
} else {
  if (type=="FO") {
    foPlot(x,...)
  } else {
    stop("'type' parameter is not valid!!")
  }
}
}

boxplotVol <- function(x,type,...){        #type="FD" or "FO"
if (missing(type)) type <- "FD"
if (type=="FD") {
  fdBoxplot(x,...)
} else {
  if (type=="FO") {
    foBoxplot(x,...)
  } else {
    stop("'type' parameter is not valid!!")
  }
}
}

      #########################
      ##                     ##
      ##  Methods to export  ##
      ##                     ##
      #########################


#-------------------------------------------------------------------------------
# Calculation from cs datasets of volume of landings/discards per haul/fd/trip
#-------------------------------------------------------------------------------


    #---------------------------------------------------------------------------
    # Raw objects
    #---------------------------------------------------------------------------

                                                           

setGeneric("landisVol", function(object,
                                 strDef,                                   
                                 species,
                                 fraction="LAN",                                
                                 sampPar=TRUE,
                                 ...){
	standardGeneric("landisVol")
})

    
                                                       

setMethod("landisVol", signature(object="csData",strDef="strIni"), function(object,
                                                              strDef,
                                                              species,         
                                                              fraction="LAN",  #or "DIS"
                                                              sampPar=TRUE,
                                                              ...){ 

landisVolumeFun(object,species=species,fraction=fraction,timeStrata=strDef@timeStrata,spaceStrata=strDef@spaceStrata,techStrata=strDef@techStrata,sampPar=sampPar)

})

setMethod("landisVol", signature(object="csData",strDef="missing"), function(object,
                                                               species,         
                                                               fraction="LAN",  #or "DIS"
                                                               sampPar=TRUE,
                                                               ...){ 

landisVolumeFun(object,species=species,fraction=fraction,timeStrata=NA,spaceStrata=NA,techStrata=NA,sampPar=sampPar)

})

#-------------------------------------------------------------------------------
# plot (--> 'edaResult' with desc="landisVol" )  cf MarketSampGraphs_ExploreSimplify.r
#-------------------------------------------------------------------------------

