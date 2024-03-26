######################################
##                                  ##
## Delta plots (outliers detection) ##
##    Length Distributions (hl)     ##
##                                  ##
##         MM 31/01/2008            ##
######################################




####################
# fast 'aggregate' #
####################
slSex <- COSTcore:::slSex
spdAgreg <- COSTcore:::spdAgreg

#spdAgreg <- function(X,BY,FUN,...){
#FactCar <- sapply(BY,as.character)
#val <- apply(FactCar,1,function(x) paste(x,collapse=":-:"))
#valAg <- aggregate(X,list(val=val),FUN,...)
#tab <- as.data.frame(matrix(unlist(strsplit(as.character(valAg$val),":-:")),ncol=length(BY),byrow=TRUE))
#tab.ag <- data.frame(tab,valAg[,-1])
#namBY <- names(BY) ; namX <- names(X)
#if (is.null(namBY)) namBY <- rep("",length(BY)) ; if (is.null(namX)) namX <- rep("",length(X))
#namBY[namBY==""] <- paste("c.",1:sum(namBY==""),sep="") ; namX[namX==""] <- paste("v.",1:sum(namX==""),sep="")
#names(tab.ag) <- c(namBY,namX)
#return(tab.ag)}
#


#################
#################

#-------------------------------------------------------------------------------
# Internal function to merge hh, sl and hl information from cs datasets for specified species
#-------------------------------------------------------------------------------


UE.proc <- function(object,species,...){

#only valid samples are kept
hh <- object@hh[object@hh$foVal=="V",]

#more time fields are added
hh$month <- sapply(hh$date,function(x) {
                              return(as.numeric(strsplit(as.character(x),"-")[[1]][2]))})
hh$quarter <- ceiling(hh$month/3)
hh$semester <- ceiling(hh$quarter/2)

#hh & sl are merged and subset for specified species
newsl <- object@sl[object@sl$spp%in%species,] 
slhh <- merge(newsl,hh,by=c("sampType",
                            "landCtry",
                            "vslFlgCtry",
                            "year",
                            "proj",
                            "trpCode",
                            "staNum"),all=FALSE)

#hl & slhh are merged and subset for specified species --> sex field is transformed according to sl$sex values
Hl <- slSex(object@sl,object@hl)                                                         #
Hl <- Hl[,-ncol(Hl)]                                                                     #      'lsex' field is removed
newhl <- Hl[Hl$spp%in%species,]#newhl <- object@hl[object@hl$spp%in%species,]            #modif 09/12/2008
hlslhh <- merge(newhl,slhh,by=c("sampType",
                                "landCtry",
                                "vslFlgCtry",
                                "year",
                                "proj",
                                "trpCode",
                                "staNum",
                                "spp",
                                "catchCat",
                                "landCat",
                                "commCatScl",
                                "commCat",
                                "subSampCat",
                                "sex"),all=FALSE)

invisible(list(spp=species,slhh=slhh,hlslhh=hlslhh))
}





############################################################################################################
############################################################################################################


#-------------------------------------------------------------------------------
# Delta calculation function
#-------------------------------------------------------------------------------



deltCalcFun <- function(object,
                        species,
                        fraction="LAN", #or "all" or "DIS"
                        strategy="metier", #or "cc"
                        timeStrata,
                        spaceStrata,
                        techStrata,
                        indSamp=TRUE,
                        method="delta",  #ou subsample si on ne procede pas a l elevation des echantillons au niveau de l OP actif seulement si indSamp=TRUE
                        tpRec,
                        spRec,
                        tcRec,
                        ...){
                        
if (method=="subsample" & indSamp) subsamp <- TRUE else subsamp <- FALSE        
                
fraction <- toupper(fraction)                                                   #
object@sl$catchCat <- toupper(object@sl$catchCat)                                                 # MM 29/04/2010
object@hl$catchCat <- toupper(object@hl$catchCat)                                                 #
object@ca$catchCat <- toupper(object@ca$catchCat)                                                 #

nbTot_Lg <- WkvTot <- DELTA <- Delta <- NkMatrix <- WkMatrix <- SampDeltaMat <- K <- Kid <- NULL 
#strategy="metier" & techStrata="commCat" don't match
if (is.na(techStrata)==FALSE & strategy=="metier" & techStrata=="commCat") 
    stop("'commCat' technical stratification does not match with 'metier' strategy!!") 

#if strata specification = NA, it's changed to NULL
if (is.na(timeStrata)) timeStrata <- NULL
if (is.na(spaceStrata)) spaceStrata <- NULL
if (is.na(techStrata)) techStrata <- NULL


#all information that we need is in tabHL (hh,sl,hl info)
tabHL <- UE.proc(object,species)$hlslhh 
#restriction to specified catch category
if (fraction!="ALL")  tabHL <- tabHL[tabHL$catchCat%in%fraction,]
 
#measured numbers are raised to sample-level
tabHL$Number <- tabHL$lenNum*(tabHL$wt/tabHL$subSampWt)
#data is aggregated within 'subSampCat' & 'sex' field
    #weights 
tb <- unique(tabHL[,c(1:12,17:18,24)])
tabHL.Wt <- spdAgreg(list(wt=tb$wt,
                          subSampWt=tb$subSampWt),
                     list(sampType=tb$sampType,
                          landCtry=tb$landCtry,
                          vslFlgCtry=tb$vslFlgCtry,
                          year=tb$year,proj=tb$proj,
                          trpCode=tb$trpCode,
                          staNum=tb$staNum,
                          spp=tb$spp,
                          catchCat=tb$catchCat,
                          landCat=tb$landCat,
                          commCatScl=tb$commCatScl,
                          commCat=tb$commCat,
                          date=tb$date),
                     sum,na.rm=TRUE)
    #measured numbers at length                    
th <- tabHL[,c(1:12,15,45,19,24)]
tabHL.Lg <- spdAgreg(list(Number=th$Number),
                     list(sampType=th$sampType,
                          landCtry=th$landCtry,
                          vslFlgCtry=th$vslFlgCtry,
                          year=th$year,
                          proj=th$proj,
                          trpCode=th$trpCode,
                          staNum=th$staNum,
                          spp=th$spp,
                          catchCat=th$catchCat,
                          landCat=th$landCat,
                          commCatScl=th$commCatScl,
                          commCat=th$commCat,
                          date=th$date,
                          lenCode=th$lenCode,
                          lenCls=th$lenCls),
                     sum,na.rm=TRUE)

#now that all data is aggregated, it can be merged
one <- unique(tabHL[,c(1:12,20:(ncol(tabHL)-1))])
two <- merge(one,tabHL.Wt,all.y=TRUE)
tabHL <- merge(two,tabHL.Lg,all.y=TRUE)

#numerical field are converted
tabHL$lenCls <- as.numeric(as.character(tabHL$lenCls))
tabHL$wt <- as.numeric(as.character(tabHL$wt))
tabHL$subSampWt <- as.numeric(as.character(tabHL$subSampWt))
tabHL$Number <- as.numeric(as.character(tabHL$Number))


#index of specified strata or not
Ntp <- is.null(timeStrata)
Nsp <- is.null(spaceStrata)
Ntc <- is.null(techStrata)

#if a stratification is specified but field is empty, stratification is considered not specified 
if (!Ntp) {
  if (all(is.na(tabHL[,timeStrata]))) {
    timeStrata <- NULL
    Ntp <- TRUE}
}

if (!Nsp) {
  if (all(is.na(tabHL[,spaceStrata]))) {
    spaceStrata <- NULL
    Nsp <- TRUE}
}

if (!Ntc) {
  if (all(is.na(tabHL[,techStrata]))) {
    techStrata <- NULL 
    Ntc <- TRUE}
}

#recoding procedure                                                                                              #  added 09/04/2010
recFun <- function(df,field,rec) {                                                                               #
  Typ <- class(df[,field])                                                                                       #
  fc <- factor(df[,field])                                                                                       #
  #Lev <- levels(fc)[!levels(fc)%in%rec$from]                                                                     #
  #df[,field] <- factor(fc,levels=c(Lev,rec$from),labels=c(Lev,rec$to))                                           #
  df[,field] <- mapvalues(fc,from=rec$from,to=rec$to)                                           #
  eval(parse('',text=paste("df[,field] <- as.",Typ,"(as.character(df[,field]))",sep="")))                        #
  return(df)                                                                                                     #
}                                                                                                                #

#stratification fields are converted to factors (& recoded : added MM 09/04/2010) 
if (!Ntp) {
  tempS <- factor(tabHL[,timeStrata])
  #modification of levels to sort numerical value correctly
  vv <- levels(tempS)
  index <- is.na(suppressWarnings(as.numeric(vv)))
  tabHL[,timeStrata] <- factor(tabHL[,timeStrata],levels=c(as.character(sort(as.numeric(vv[!index]))),vv[index]))
  if (!is.na(tpRec[1])) tabHL <- recFun(tabHL,timeStrata,tpRec)                 #added 09/04/2010
}
if (!Nsp) {
  tabHL[,spaceStrata] <- factor(tabHL[,spaceStrata])
  if (!is.na(spRec[1])) tabHL <- recFun(tabHL,spaceStrata,spRec)                #added 09/04/2010
}
if (!Ntc) {
  tabHL[,techStrata] <- factor(tabHL[,techStrata])
  if (!is.na(tcRec[1])) tabHL <- recFun(tabHL,techStrata,tcRec)                 #added 09/04/2010
}


#sample definition depends on strategy definition
if (strategy=="cc") {
  UniteInt <- apply(tabHL[,1:13],1,paste,collapse=":-:")                     
} else {                                                                                              
  UniteInt <- apply(tabHL[,1:11],1,paste,collapse=":-:")}                                    
    
lev <- levels(factor(UniteInt))
#tabHL$Unite <- factor(UniteInt,levels=lev,labels=1:length(lev))
tabHL$Unite <- mapvalues(factor(UniteInt),from=lev,to=1:length(lev))                                           #

#sample index -->  trpCode/staNum/spp/cc if strategy=="cc" or trpCode/staNum/spp if strategy=="metier"
extract <- function(vec,elmts) unlist(lapply(strsplit(vec,":-:"),function(x) x[elmts]))   #":-:" to be modified
#DFsamp is a data.frame to allow user to have from 'SampNum' index a quick description of corresponding sample 
if (strategy=="cc") {
  DFsamp <- data.frame(SampNum=(1:length(lev)),trpCode=extract(lev,6),staNum=extract(lev,7),spp=extract(lev,8),commCat=extract(lev,12))
} else {
  DFsamp <- data.frame(SampNum=(1:length(lev)),trpCode=extract(lev,6),staNum=extract(lev,7),spp=extract(lev,8))
}


#length data specification allows to take into account the empty classes
lenSp <- c(1,5,10,25)
names(lenSp) <- c("mm","scm","cm","25mm")
tabHL$Length <- factor(tabHL$lenCls,levels=seq(min(tabHL$lenCls,na.rm=TRUE),max(tabHL$lenCls,na.rm=TRUE),by=lenSp[as.character(unique(tabHL$lenCode)[1])]))

vecN <- tabHL$Number
if (subsamp) vecN <- tabHL$Number*tabHL$subSampWt/tabHL$wt
#number by length classes and stratification
eval(parse('',text=paste("nbTot_Lg <- tapply(vecN,list(tabHL$Length",","[any(c(!Ntp,!Nsp,!Ntc))],
                          paste(c("as.character(tabHL[,timeStrata])"[!Ntp],"as.character(tabHL[,spaceStrata])"[!Nsp],
                          "as.character(tabHL[,techStrata])"[!Ntc]),collapse=",",sep=""),"),sum,na.rm=TRUE)",sep="")))
TotLg <- apply(nbTot_Lg,1,sum,na.rm=TRUE)                             #total number on all crossed strata

#weights by sample and stratification, depending on strategy
vecW <- tabHL$wt ; field <- "wt"
if (subsamp) {vecW <- tabHL$subSampWt ; field <- "subSampWt"}

if (strategy=="cc") {
  eval(parse('',text=paste("WkvTot <- tapply(vecW,list(as.character(tabHL$Unite)",","[any(c(!Ntp,!Nsp,!Ntc))],
                           paste(c("as.character(tabHL[,timeStrata])"[!Ntp],"as.character(tabHL[,spaceStrata])"[!Nsp],
                           "as.character(tabHL[,techStrata])"[!Ntc]),collapse=",",sep=""),"),mean,na.rm=TRUE)",sep="")))                  
} else {                                                                                           
  tabtabHL <- cbind(tabHL[,1:13],tabHL[,c(timeStrata,spaceStrata,techStrata,field,"Unite")])
  tabtabHL <- unique(tabtabHL)
  vecW2 <- tabtabHL[,field]                       
  eval(parse('',text=paste("WkvTot <- tapply(vecW2,list(as.character(tabtabHL$Unite)",","[any(c(!Ntp,!Nsp,!Ntc))],
                           paste(c("as.character(tabtabHL[,timeStrata])"[!Ntp],"as.character(tabtabHL[,spaceStrata])"[!Nsp],
                           "as.character(tabtabHL[,techStrata])"[!Ntc]),collapse=",",sep=""),"),sum,na.rm=TRUE)",sep="")))                          
}       

#total weight
TotW <- sum(WkvTot,na.rm=TRUE)


#internal procedure with different outputs : ind=1 --> delta squared (within specified stratum) --> for variance calculation
#                                            ind=2 --> Nk (number of samples in the stratum)
#                                            ind=3 --> Wk (total weight in the stratum)
#                                            ind=4 --> DELTA (overall) --> for outlier detection process
progint <- function(x,ind,tabHL,subs=FALSE) {           #x is a row index for tabHL corresponding to a specific stratum 
  tab <- tabHL[x,]
  Djkv <- tapply(tab$Number,list(tab$Length,as.character(tab$Unite)),sum,na.rm=TRUE)
  if (subsamp) Djkv <- tapply(tab$Number*tab$subSampWt/tab$wt,list(tab$Length,as.character(tab$Unite)),sum,na.rm=TRUE)
  Djkv[is.na(Djkv)] <- 0
  
  if (strategy=="cc") {
    Wkv <- tapply(tab$wt,list(as.character(tab$Unite)),mean)
    subWkv <- tapply(tab$subSampWt,list(as.character(tab$Unite)),mean)          #MM 27/07/2010         
  } else {                                                                                           
    tabtab <- cbind(tab[,1:13],tab[,c("wt","Unite")])
    tabtab <- unique(tabtab)                       
    Wkv <- tapply(tabtab$wt,list(as.character(tabtab$Unite)),sum,na.rm=TRUE)
    subtabtab <- cbind(tab[,1:13],tab[,c("subSampWt","Unite")])                                  #MM 27/07/2010  
    subtabtab <- unique(subtabtab)                                                               #MM 27/07/2010  
    subWkv <- tapply(subtabtab$subSampWt,list(as.character(subtabtab$Unite)),sum,na.rm=TRUE)     #MM 27/07/2010  
  }
  if (subsamp) Wkv <- subWkv  
  Kunit <- tapply(1:nrow(tab),list(as.character(tab$Unite)),function(x) sum(tab[x,]$Number*tab[x,]$subSampWt*((tab[x,]$lenCls/10)^3)/tab[x,]$wt,na.rm=TRUE))     #MM 27/07/2010
  Kunit <- Kunit/subWkv                                                                                                                                           #MM 27/07/2010
                                                                                                  
  DjkvSum <- apply(Djkv,1,sum,na.rm=TRUE)
  WkvSum <- sum(Wkv,na.rm=TRUE)
  Nk <- length(Wkv)
  Wk <- sum(Wkv,na.rm=TRUE)
  delta <- Djkv - (DjkvSum/WkvSum)%*%t(Wkv)
  delta2 <- Djkv - (TotLg/TotW)%*%t(Wkv)
  deltaSamp <- apply(delta2,2,sum,na.rm=TRUE)
  eval(parse('',text=paste("DELTA <- data.frame(samp=names(deltaSamp),delta=deltaSamp",","[any(c(!Ntp,!Nsp,!Ntc))],
                           paste(c("tp=as.character(tab[1,timeStrata])"[!Ntp],"sp=as.character(tab[1,spaceStrata])"[!Nsp],
                           "tc=as.character(tab[1,techStrata])"[!Ntc]),collapse=",",sep=""),")",sep="")))
  eval(parse('',text=paste("K <- data.frame(samp=names(Kunit),K=Kunit",","[any(c(!Ntp,!Nsp,!Ntc))],                            #MM 27/07/2010
                           paste(c("tp=as.character(tab[1,timeStrata])"[!Ntp],"sp=as.character(tab[1,spaceStrata])"[!Nsp],      #MM 27/07/2010
                           "tc=as.character(tab[1,techStrata])"[!Ntc]),collapse=",",sep=""),")",sep="")))                       #MM 27/07/2010

  deltaSq <- apply(delta^2,1,sum,na.rm=TRUE)
  ll <- list(deltaSq,Nk,Wk,DELTA,K)[[ind]]                                                                                      #MM 27/07/2010
  return(ll)
}


if (!indSamp) {

  #-----------------------------------------------------------------------------
  # Output will be used for variance calculation
  #-----------------------------------------------------------------------------
 
  eval(parse('',text=paste("Delta <- tapply(1:nrow(tabHL),list(",
                           paste(c("as.character(tabHL[,timeStrata])"[!Ntp],"as.character(tabHL[,spaceStrata])"[!Nsp],
                           "as.character(tabHL[,techStrata])"[!Ntc]),collapse=",",sep=""),"),function(x) progint(x,1,tabHL))",sep="")))
  
  #Delta information is transfered into a more friendly object --> DeltaMatrix
  if (all(c(Ntp,Nsp,Ntc))) {                                                            # added 08/10/2008
    DeltaMatrix <- Delta[[1]]                                                           #
  } else {                                                                              #
    DeltaMatrix <- array(0,dim=c(length(levels(tabHL$Length)),dim(Delta)),dimnames=unlist(list(list(levels(tabHL$Length)),dimnames(Delta)),recursive=FALSE))
    index <- apply(expand.grid(dimnames(Delta)),1,function(x) paste("\"",x,"\"",collapse=",",sep=""))
    invisible(sapply(index,function(x) eval(parse('',text=paste("if (!is.null(Delta[",index,"][[1]])) DeltaMatrix[,",index,"] <<- Delta[",index,"][[1]]",sep="")))))
  }                                                                                     #
  
  eval(parse('',text=paste("NkMatrix <- tapply(1:nrow(tabHL),list(",
                           paste(c("as.character(tabHL[,timeStrata])"[!Ntp],"as.character(tabHL[,spaceStrata])"[!Nsp],
                           "as.character(tabHL[,techStrata])"[!Ntc]),collapse=",",sep=""),"),function(x) progint(x,2,tabHL))",sep="")))
  
  eval(parse('',text=paste("WkMatrix <- tapply(1:nrow(tabHL),list(",
                          paste(c("as.character(tabHL[,timeStrata])"[!Ntp],"as.character(tabHL[,spaceStrata])"[!Nsp],
                          "as.character(tabHL[,techStrata])"[!Ntc]),collapse=",",sep=""),"),function(x) progint(x,3,tabHL))",sep="")))

  result <- list(species=species,
                 fraction=fraction,
                 strategy=strategy,
                 timeStrata=timeStrata,
                 spaceStrata=spaceStrata,
                 techStrata=techStrata,
                 DeltaMatrix=DeltaMatrix,
                 NkMatrix=NkMatrix,
                 WkMatrix=WkMatrix)
                 
  invisible(new("edaResult",desc="varDeltaCalc",outPut=result))
  
} else {

  #-----------------------------------------------------------------------------
  # Output will be used for outlier detection process
  #-----------------------------------------------------------------------------
 
  eval(parse('',text=paste("SampDeltaMat <- do.call(\"rbind\",tapply(1:nrow(tabHL),list(",
                           paste(c("as.character(tabHL[,timeStrata])"[!Ntp],"as.character(tabHL[,spaceStrata])"[!Nsp],
                           "as.character(tabHL[,techStrata])"[!Ntc]),collapse=",",sep=""),"),function(x) progint(x,4,tabHL,subsamp)))",sep="")))

  eval(parse('',text=paste("Kid <- do.call(\"rbind\",tapply(1:nrow(tabHL),list(",
                           paste(c("as.character(tabHL[,timeStrata])"[!Ntp],"as.character(tabHL[,spaceStrata])"[!Nsp],
                           "as.character(tabHL[,techStrata])"[!Ntc]),collapse=",",sep=""),"),function(x) progint(x,5,tabHL)))",sep="")))

  result <- list(species=species,
                 fraction=fraction,
                 strategy=strategy,
                 timeStrata=timeStrata,
                 spaceStrata=spaceStrata,
                 techStrata=techStrata,
                 SampDeltaMat=SampDeltaMat,
                 Kid=Kid,
                 DFsamp=DFsamp,
                 tab=tabHL[,c("Unite","wt","Length","Number")])
   
   invisible(new("edaResult",desc="sampDeltaCalc",outPut=result))
}

}




#-------------------------------------------------------------------------------
# Delta values displaying function
#-------------------------------------------------------------------------------




plotDelta <- function(x,    #x is to be an object 'edaResult' with desc="sampDeltaCalc"  
                      elmts=list(tp="all",sp="all",tc="all"),   #zoom on classes
                      strat1,strat2="NULL",# graphical display (to be chosen between "timeStrata", "spaceStrata" or "techStrata")
                      selection=FALSE,
                      show.legend="right",
                      shift=FALSE,
                      Kplot=FALSE,         
                      ...){  

#importation of specifications from x object
species <- x@outPut$species
fraction <- x@outPut$fraction
strategy <- x@outPut$strategy
timeStrata <- x@outPut$timeStrata
spaceStrata <- x@outPut$spaceStrata
techStrata <- x@outPut$techStrata

#elmts list
if (is.null(elmts$tp)) elmts$tp <-"all"
if (is.null(elmts$sp)) elmts$sp <-"all"
if (is.null(elmts$tc)) elmts$tc <-"all"
elmts <- list(tp=elmts$tp,sp=elmts$sp,tc=elmts$tc)
 
stra <- c(timeStrata,spaceStrata,techStrata)

#if no specified stratification 
if (length(stra)==0) {

#-------------------------------------------------------------------------------
# Case n1 : no stratification
#-------------------------------------------------------------------------------
 
object2 <- x@outPut$SampDeltaMat

if (Kplot) object2 <- x@outPut$Kid

if (is.null(dots$l.col)) dots$l.col <- "#8CC2FFE6"
#graphical parameters specification
data(GraphsPar,envir=environment())                                                                                                                          
dots <- list(...)
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Samples"
if (is.null(dots$ylab)) { 
  dots$ylab <- "Delta values"
  if (Kplot) dots$ylab <- "K values"
  } 
if (is.null(dots$main)) 
  dots$main <- paste("Delta plot / Species : ",paste(species,collapse=", "),sep="") 

XX <- 1:nrow(object2)
YY <- object2$delta
if (Kplot) YY <- object2$K

  #-----------------------------------------------------------------------------
  # Graphical display
  #-----------------------------------------------------------------------------
                                                                                                                    
print(xyplot(YY~XX,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),scales=list(font=dots$font.axis),
             panel= function(x,y,...) {
              panel.xyplot(x,y,pch=dots$pch[1],fill=dots$p.bg[1],cex=dots$p.cex[1],lwd=dots$lwd[1],col=dots$col[1])
              panel.abline(h=0,lwd=dots$l.lwd[1],lty=dots$lty[1],col=dots$l.col[1])}        
))

} else {

#-------------------------------------------------------------------------------
# Case n2 : specified stratification
#-------------------------------------------------------------------------------
 

#if 'strat1' is missing, first specified stratification is taken
if (missing(strat1)) {
  strat1 <- c("timeStrata","spaceStrata","techStrata")[c(!is.null(timeStrata),!is.null(spaceStrata),!is.null(techStrata))][1]
  strat2 <- "NULL"
}

object <- x@outPut$SampDeltaMat
if (Kplot) object <- x@outPut$Kid


#selection of samples specified by 'elmts' parameter
invisible(sapply(names(object)[3:ncol(object)],function(x) {
                                                elm <- unlist(elmts[x])
                                                if (!"all"%in%elm) object <<- object[as.character(object[,x])%in%elm,]
                                                })) 

#new field names for 'object', and redefinition of time factor if necessary                             
if (ncol(object)>2) {
  names(object)[3:ncol(object)] <- c(timeStrata,spaceStrata,techStrata)   
  if (any(c("year","semester","quarter","month")%in%names(object))) 
    object[,timeStrata] <- factor(object[,timeStrata],levels=as.character(sort(as.numeric(levels(object[,timeStrata])))))
}

                    
#graphical parameters specification
data(GraphsPar,envir=environment())                                                                                                                          
dots <- list(...)
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Samples"
if (is.null(dots$ylab)) { 
  dots$ylab <- "Delta values"
  if (Kplot) dots$ylab <- "K values"
  }  
if (is.null(dots$main)) 
  dots$main <- paste("Delta plot / Species : ",paste(species,collapse=", "),"\n Primary strata : ",eval(parse('',text=strat1)),sep="") 


#graphical display depending on 'strat1' and 'strat2' parameters
FStr <- eval(parse('',text=strat1))
SStr <- eval(parse('',text=strat2))
  #ordering illustrated strata 
if (!is.null(SStr))  
  object2 <- object[order(object[,FStr],object[,SStr]),] 
else 
  object2 <- object[order(object[,FStr]),]

XX <- 1:nrow(object2)
YY <- object2$delta
if (Kplot) YY <- object2$K
  #intra strata color distribution
if (!is.null(SStr)) 
  #ff <- factor(object2[,SStr],levels=levels(object2[,SStr]),labels=rep(dots$p.bg,length=length(levels(object2[,SStr])))) 
  ff <- mapvalues(object2[,SStr],from=levels(object2[,SStr]),to=rep(dots$p.bg,length=length(levels(object2[,SStr])))) 
else 
  ff <- dots$p.bg[1]
  #inter strata delimitation
delimit <- tapply(object2[,FStr],list(object2[,FStr]),length)
delimit <- delimit[!is.na(delimit)]
indLab <- cumsum(delimit)
  #shifting option for text
if (Kplot) {
  amp <- max(object2$K)-min(object2$K)
} else {
  amp <- max(object2$delta)-min(object2$delta)
}
if (shift) 
  decal <- rep(c(1,-1),length=length(delimit)) 
else 
  decal <- 0                                                                                  

if (Kplot) {
  MIN <- min(object2$K)
} else {
  MIN <- min(object2$delta)
}
  #-----------------------------------------------------------------------------
  # Graphical display
  #-----------------------------------------------------------------------------
                                                                                                                    
print(xyplot(YY~XX,main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),scales=list(font=dots$font.axis),
             key=eval(parse('',text=c("NULL",paste("list(points=list(pch=dots$pch[1],fill=levels(ff),cex=dots$p.cex[1],lwd=dots$lwd[1],col=dots$col[1]),",
                                                   "text=list(levels(object2[,SStr])),title=SStr,cex.title=0.8,font=dots$font.lab,space=show.legend,columns=1,border=TRUE)",
                                                   sep=""))[c(is.null(SStr),!is.null(SStr))])),
             panel= function(x,y,...) {
              panel.xyplot(x,y,pch=dots$pch[1],fill=as.character(ff),cex=dots$p.cex[1],lwd=dots$lwd[1],col=dots$col[1])
              panel.abline(v=indLab[-length(indLab)]+0.5,lwd=dots$l.lwd[1],lty=dots$lty[1],col=dots$l.col[1])
              panel.abline(h=0,lwd=dots$l.lwd[1],lty=dots$lty[1],col=dots$l.col[1])
              panel.text(0.5+indLab-delimit/2,MIN+amp*0.03+amp*0.04*decal,names(indLab),col="black",font=4,cex=dots$cex.sub[1])}        
))
}

  #-----------------------------------------------------------------------------
  # Output depends on 'selection' parameter
  #-----------------------------------------------------------------------------
 
if (selection) {
  trellis.focus("panel",1,1)
  Ident <- panel.identify(labels=object2$samp)
	#identification process
	listOP <- object2$samp[Ident]
	#identified samples
	result <- list(species=species,
	               fraction=fraction,
                 sampId=x@outPut$DFsamp[x@outPut$DFsamp$SampNum%in%listOP,,drop=FALSE],
                 tabId=x@outPut$tab[x@outPut$tab$Unite%in%listOP,,drop=FALSE],
                 tab=x@outPut$tab)
 
  invisible(new("edaResult",desc="sampDeltaId",outPut=result))

} else {

  invisible(object2)}

}



#-------------------------------------------------------------------------------
# Graphical function to display length distributions of identified samples during Delta process
#-------------------------------------------------------------------------------



plotDeltaId <- function(x,        #x is to be an object 'edaResult' with desc="sampDeltaId"
                        smpNum="all",                                  #ajout
                        show.legend="right",
                        ...){
tabID <- x@outPut$tabId
if (!"all"%in%smpNum) tabID <- tabID[tabID$Unite%in%smpNum,]            #ajout
TAB <- x@outPut$tab
tabID$Unite <- as.character(tabID$Unite) 
VecMesId <- tapply(tabID$Number,list(tabID$Unite,tabID$Length),sum,na.rm=TRUE,drop=FALSE)
Som.djku <- apply(tapply(TAB$Number,list(TAB$Unite,TAB$Length),sum,na.rm=TRUE,drop=FALSE),2,sum,na.rm=TRUE)
tabUn <- unique(tabID[,c("Unite","wt")])
tabUnPop <- unique(TAB[,c("Unite","wt")])
Som.wku <- sum(tabUnPop$wt,na.rm=TRUE)
w.ku <- tapply(tabUn$wt,list(tabUn$Unite),sum,na.rm=TRUE)
VecSomme <- w.ku%*%t(Som.djku/Som.wku) 

data(GraphsPar,envir=environment())                                                                                                                        
dots <- list(...)
sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))

if (is.null(dots$col)) 
  dots$col <- dots$l.col
if (is.null(dots$lwd)) 
  dots$lwd <- dots$l.lwd[1]  
if (is.null(dots$xlab)) 
  dots$xlab <- "Length" 
if (is.null(dots$ylab)) 
  dots$ylab <- "Numbers"
if (is.null(dots$main)) 
  dots$main <- "Delta Length Frequencies" 


df <- data.frame(x1=as.numeric(rep(colnames(VecSomme),each=nrow(VecSomme))),
                 y1=rep(rownames(VecSomme),ncol(VecSomme)),
                 Ech=as.vector(VecMesId),
                 Exp=as.vector(VecSomme))     

print(xyplot(Ech+Exp~x1|y1 ,data=df,type=c("h","l"),lty=rep(dots$lty,length=2),par.strip.text=list(font=dots$font.lab),
             col=rep(dots$col,length=2),lwd=rep(dots$lwd,length=2),distribute.type=TRUE,scales=list(font=dots$font.axis,x=list(rot=90)),
             key=list(lines=list(lty=rep(dots$lty,length=2),col=rep(dots$col,length=2),lwd=rep(dots$lwd,length=2)),text=list(c("Sampled","Overall")),
             font=dots$font.lab,space=show.legend,columns=1,border=TRUE),
             main=list(dots$main,font=dots$font.main),xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab)))

invisible(df)
}

#-------------------------------------------------------------------------------
# Length distribution for one species (modified 19/01/2009)
#-------------------------------------------------------------------------------


lenDisPlotFun <- function(x,        # a csData or csDataVal object
                          species,
                          fraction="LAN",
                          trpCode="all",     #a vector of trips
                          level="trip",      # or "fo" (trip level or fo level)
                          ...){      
 
fraction <- toupper(fraction)                                                   #
x@sl$catchCat <- toupper(x@sl$catchCat)                                                 # MM 29/04/2010
x@hl$catchCat <- toupper(x@hl$catchCat)                                                 #
x@ca$catchCat <- toupper(x@ca$catchCat)                                                 #
                                                 
if (length(species)!=1) 
  stop("One species is to be specified!!")

if (!all(fraction%in%c("LAN","DIS"))) 
  stop("Wrong 'fraction' parameter!!")

if (length(level)!=1) 
  stop("Wrong 'level' parameter!!")

if (!level%in%c("trip","fo")) 
  stop("Wrong 'level' parameter!!")
#if (length(trpCode)!=1) stop("One trip code is to be specified !!")

if ("all"%in%trpCode) 
  trpCode <- unique(sl(x)$trpCode) 
trpCode <- as.character(trpCode)
#staNum <- as.character(staNum)

if (level=="trip") {
  title.lev <- "trip"
  panel.ind<- FALSE
} else {
 title.lev <- "fishing operation"
 panel.ind <- TRUE
} 

title.frac <- paste("\"",fraction,"\" fraction",sep="")
if (length(title.frac)>1) title.frac <- "total catch" 

data(GraphsPar,envir=environment())                                                                                                                       
object <- hl(x)
lgthCode <- as.character(sl(x)[(sl(x)$trpCode%in%trpCode)&(sl(x)$spp%in%species),"lenCode"][1])

stepp <- c(1,5,10,25)
names(stepp) <- c("mm","scm","cm","25mm")
ste <- stepp[lgthCode]

dots <- list(...)
if (is.null(dots$p.col)) 
  dots$p.col <- "black" 
if (is.null(dots$p.bg)) 
  dots$p.bg <- "lightblue" 
if (is.null(dots$cex.axis)) 
  dots$cex.axis <- 0.8

sapply(names(gp),function(x) 
                  if (is.null(eval(parse('',text=paste("dots$",x,sep=""))))) 
                    eval(parse('',text=paste("dots$",x," <<- gp$",x,sep=""))))
if (is.null(dots$xlab)) 
  dots$xlab <- "Length"
if (is.null(dots$ylab))                           
  dots$ylab <- "Number" 
if (is.null(dots$main)) 
  dots$main <- paste("Length distribution of ",title.frac," of \"",species,"\" species by ",title.lev,sep="") 
if (is.null(dots$layout)) dots$layout <- NULL 
if (is.null(dots$as.table)) dots$as.table <- FALSE 

df <- object[(object$trpCode%in%trpCode)&(object$catchCat%in%fraction)&(object$spp%in%species),]
if (nrow(df)==0) stop("No data for specified trip code and fraction!!")
#if ("all"%in%staNum) {
#  staNum <- unique(as.character(df$staNum))
#} else {                                                          
#  if ("allSum"%in%staNum) df$staNum <- staNum <- "all" }            

#df <- df[df$staNum%in%staNum,]
#df$staNum <- factor(df$staNum)
#if (nrow(df)==0) stop("No data for specified station number!!")
df$trpCode <- paste("Trip #",df$trpCode,sep="")
df$staNum <- paste("FO #",df$staNum,sep="")

#empty length classes are considered
df$lenCls <- factor(df$lenCls,levels=seq(min(df$lenCls),max(df$lenCls),by=ste))  
eval(parse('',text=paste("LD <- aggregate(df$lenNum,list(lenCls=df$lenCls,","staNum=df$staNum,"[panel.ind],"trpCode=df$trpCode),sum,na.rm=TRUE)",sep="")))
LD[is.na(LD)] <- 0 ; LD <- LD[,c(rev(1:(ncol(LD)-1)),ncol(LD))]
names(LD)[ncol(LD)] <- "val"
#ll <- dimnames(LD)
#
#DF <- data.frame(staNum=rep(ll$staNum,ncol(LD)),
#                 lenCls=rep(ll$lenCls,each=nrow(LD)),
#                 val=as.numeric(LD))
#                 
#if (!DF$staNum[1]=="all") DF$staNum <- factor(DF$staNum,levels=as.character(sort(as.numeric(levels(DF$staNum)))))

eval(parse('',text=paste("print(barchart(val~lenCls|trpCode","*staNum"[panel.ind],",data=LD,ylim=c(0,max(LD$val)*1.05),scales=list(x=list(rot=dots$rot,cex=dots$cex.axis),",
                         "font=dots$font.axis),main=list(dots$main,font=dots$font.main),layout=dots$layout,as.table=dots$as.table,",
                         "xlab=list(dots$xlab,font=dots$font.lab),ylab=list(dots$ylab,font=dots$font.lab),",
                         "par.strip.text=list(font=dots$font.lab),col=dots$p.bg,fill=dots$p.bg,drop.unused.levels=FALSE))",sep="")))  

invisible(LD)
}
  







      #########################
      ##                     ##
      ##  Methods to export  ##
      ##                     ##
      #########################


#-------------------------------------------------------------------------------
# deltCalc
#-------------------------------------------------------------------------------


    #---------------------------------------------------------------------------
    # Raw objects
    #---------------------------------------------------------------------------


                                                         
setGeneric("deltCalc", function(data,                #cs/cl/ceData or cs/cl/ceDataVal
                                strDef,              #'strIni' object
                                species,
                                fraction="LAN", #or "all" or "DIS"
                                strategy="metier", #or "cc"
                                indSamp=TRUE,
                                method="delta",    
                                ...){

  standardGeneric("deltCalc")

})




setMethod("deltCalc", signature("csData","strIni"), function(data,
                                                             strDef,
                                                             species,
                                                             fraction="LAN", #or "all" or "DIS"
                                                             strategy="metier", #or "cc"
                                                             indSamp=TRUE,
                                                             method="delta",       
                                                             ...){

deltCalcFun(data,species=species,fraction=fraction,strategy=strategy,timeStrata=strDef@timeStrata,
            spaceStrata=strDef@spaceStrata,techStrata=strDef@techStrata,indSamp=indSamp,method=method,tpRec=strDef@tpRec,spRec=strDef@spRec,
            tcRec=strDef@tcRec,...)
      
})         
  
    #---------------------------------------------------------------------------
    # Validated objects
    #---------------------------------------------------------------------------
  
setMethod("deltCalc", signature("csDataVal","strIni"), function(data,
                                                                strDef,
                                                                species,
                                                                fraction="LAN", #or "all" or "DIS"
                                                                strategy="metier", #or "cc"
                                                                indSamp=TRUE,
                                                                method="delta",       
                                                                ...){

deltCalcFun(data,species=species,fraction=fraction,strategy=strategy,timeStrata=strDef@timeStrata,
            spaceStrata=strDef@spaceStrata,techStrata=strDef@techStrata,indSamp=indSamp,method=method,tpRec=strDef@tpRec,spRec=strDef@spRec,
            tcRec=strDef@tcRec,...)
      
})         
 


#-------------------------------------------------------------------------------
# plot (--> 'edaResult' with desc="sampDeltaCalc" )  cf MarketSampGraphs_ExploreSimplify.r
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# lenDisPlot method
#-------------------------------------------------------------------------------



setGeneric("lenDisPlot", function(x,
                                  species,
                                  fraction="LAN",
                                  trpCode="all",
                                  level="trip",
                                  ...){
	standardGeneric("lenDisPlot")
})


    #---------------------------------------------------------------------------
    # Raw objects
    #---------------------------------------------------------------------------


setMethod("lenDisPlot",signature("csData"), function(x,
                                                     species,
                                                     fraction="LAN",
                                                     trpCode="all",
                                                     level="trip",
                                                     ...){      

lenDisPlotFun(x,species=species,fraction=fraction,trpCode=trpCode,level=level,...)

})
  
    #---------------------------------------------------------------------------
    # Validated objects
    #---------------------------------------------------------------------------


setMethod("lenDisPlot",signature("csDataVal"), function(x,
                                                        species,
                                                        fraction="LAN",
                                                        trpCode="all",
                                                        level="trip",
                                                        ...){      

lenDisPlotFun(x,species=species,fraction=fraction,trpCode=trpCode,level=level,...)

})




