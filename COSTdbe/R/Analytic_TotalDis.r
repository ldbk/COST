       #######################
     ###########################
   ###                        ###
 ##### Design-based estimates #####
   ###                        ###
     ###########################
    #############################
    # Tasks related to discards #
    #############################


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################




################################################################################
# Internal procedures
################################################################################


#COSTcore package function to insert a new 'sex' field in hl : SL$sex is therefore a full-time key field 
slSex <- COSTcore:::slSex                                                                 #modif 11/12/2008
           

#-------------------------------------------------------------------------------
# Formatting function
#-------------------------------------------------------------------------------

procRaise.format <- function(vrbl,lg=TRUE) {                                                                                       #\\#
tempDf <- do.call("rbind",lapply(names(vrbl),function(x) strsplit(x ,":@&@&@:")[[1]]))
DF <- cbind.data.frame(tempDf,vrbl)                                                                                                #\\#
df <- as.data.frame(do.call("rbind",lapply(as.character(DF[,1]),function(x) res <- strsplit(x,":-:")[[1]])))                       #\\#
names(df) <- c("time","space","technical")                                                                                         #\\#
if (lg) df$length <- DF[,2]                                                                                                        #\\#
df$value <- as.numeric(as.character(DF[,ncol(DF)]))                                                                                #\\#
df <- df[order(df[,1],df[,2],df[,3],df[,4]),]
rownames(df) <- 1:nrow(df)
return(df)                                                                                                                         #\\#
}                                                                                                                                  #\\#



             



#-------------------------------------------------------------------------------
# Raising methodology based on 4.1 section of 'Raising procedures for discards : Sampling theory' (J. Vigneau, 2006)  --> raising by trip 
#-------------------------------------------------------------------------------

procRaise.trip <- function(csObject,                      #consolidated CS table
                           ceObject,                      #consolidated CE table (same stratification as csObjet)
                           dbeOutp,                       #'dbeOutput' object with descriptive fields 
                           val="weight",                  #value to raise ("weight" or "number" or "nAtLength")                                   #\\#
                           sampPar=TRUE,                  #'sampPar' checks if given species is considered to be automatically sampled
                           ...) {

dbeOutp@catchCat <- toupper(dbeOutp@catchCat)                               #
csObject@sl$sort <- toupper(csObject@sl$sort)                                   # MM 29/04/2010
csObject@hl$sort <- toupper(csObject@hl$sort)                                   #
csObject@ca$sort <- toupper(csObject@ca$sort)                                   #
                                                                                                                                                  #\\#
if (!val%in%c("weight","number","nAtLength")) {                                                                                                    #\\#
  stop("wrong 'val' parameter!!")                                                                                                                   #\\#
}

fraction <- dbeOutp@catchCat[1]

if (length(dbeOutp@catchCat)>1) {
  warning("only the first specified catch category will be considered!!")
}

if (!fraction%in%c("DIS","LAN")) {
  warning("wrong 'catchCat' slot!!")
}


if (dbeOutp@methodDesc!="analytical") {
  warnings("'dbeOutput' object doesn't match the method!! 'methodDesc' slot will be updated!")
  dbeOutp@methodDesc <- "analytical"}
  
species <- dbeOutp@species
if ("all"%in%species) species <- unique(as.character(csObject@sl$spp))


#according to 'val' and available information, hauls are considered to be sampled or not --> 'sampledFO' method
indSam <- sampledFO(csObject,species=species,fraction=fraction,sampPar=sampPar)

if (val=="weight") indSam <- indSam$sampWt else indSam <- indSam$sampLg
#sampled FOs are ...
samFO <- csObject@hh[!is.na(indSam),c("PSUid","SSUid","time","space","technical")] ; samFO$ind <- 1   #specifies matrix dimensions
samTrip <- unique(samFO[,c("PSUid","time","space","technical","ind")])                                                                   #modif 29/01/2009
nSamp <- aggregate(samTrip$ind,list(technical=samTrip$technical,space=samTrip$space,time=samTrip$time),sum)   #number of samples (trips) #        
names(nSamp)[ncol(nSamp)] <- "value"                                                                                                     #
#so, values to be considered are ...
if (val=="weight") {
  VAL <- csObject@sl
  VAL$vol <- VAL$wt/1000             # weights in kg
  VAL$lenCls <- "all"                                                                                                                          #\\#
} else {
  VAL <- merge(slSex(csObject@sl,csObject@hl),csObject@sl[,c("PSUid","SSUid","TSUid","spp","sort","sex","wt","subSampWt")],all.x=TRUE,sort=FALSE)     #modif 11/12/2008 csObject@hl <-> slSex(csObject@sl,csObject@hl)
  VAL$vol <- VAL$lenNum*VAL$wt/VAL$subSampWt
  if (val=="number") VAL$lenCls <- "all"                                                                                                           #\\#
}                                                                                                                                                

VAL <- VAL[VAL$spp%in%species,] ; VAL <- merge(VAL,samFO,all.x=TRUE)
VAL <- VAL[extCatchCat(VAL$sort)%in%fraction & !is.na(VAL$ind),]                #'extCatchCat' function can be found in '0valuesIndex.r' file
if (nrow(VAL)==0) stop("no available sampling data for specified parameters!!")
VALstrat <- paste(VAL$time,VAL$space,VAL$technical,sep=":-:")
samFOstrat <- paste(samFO$time,samFO$space,samFO$technical,sep=":-:")          
HHstrat <- paste(csObject@hh$time,csObject@hh$space,csObject@hh$technical,sep=":-:")

  ##FOind <- tapply(samFO$ind,list(samFO$PSUid,samFO$SSUid,samFOstrat),function(x) return(0))  #index of all sampled FOs
FOind <- catApply(samFO$ind,list(samFO$PSUid,samFO$SSUid,samFOstrat),function(x) return(0))


#------
# yijk : volume in a haul j of a trip i, for a class k                                                                                         #\\#
#------
if (any(is.na(VAL$vol))) warning("values from hauls defined as sampled hauls are missing!!")
  
  ##yijk <- tapply(VAL$vol,list(factor(VAL$PSUid,levels=levels(factor(samFO$PSUid))),  #all sampled trip should appear
  ##                           factor(VAL$SSUid,levels=levels(factor(samFO$SSUid))),
  ##                           factor(VALstrat,levels=levels(factor(samFOstrat))),
  ##                           VAL$lenCls),                                                                                                         #\\# 
  ##              sum,na.rm=TRUE)
  
yijk <- catApply(VAL$vol,list(as.character(VAL$PSUid),as.character(VAL$SSUid),as.character(VALstrat),VAL$lenCls),sum,na.rm=TRUE)  
  
  
  #sampled FOs that are not recorded in VAL must also appear in yij as 0 values
  ##yijk[is.na(yijk) & rep(!is.na(FOind),dim(yijk)[4])] <- 0  
  
                                                                                         #####
dfTemp <- as.data.frame(t(FOind$ind))                                                        #
cross <- merge(dfTemp,unique(yijk$ind[4,]),all=TRUE)                                         #
yijk.new <- dbeReplic(yijk,as.matrix(t(cross)))                                              #
yijk.new$val[is.na(yijk.new$val)] <- 0                                                   #####

#------
# mi : number of sampled hauls in a trip i
#------
  ##mi <- tapply(samFO$ind,list(samFO$PSUid,factor(samFOstrat,levels=dimnames(yijk)[[3]])),sum)
mi <- catApply(samFO$ind,list(as.character(samFO$PSUid),as.character(samFOstrat)),sum)  

#------
# Mi : total number of hauls in a trip i
#------
  ##Mi <- tapply(csObject@hh$SSUid,list(factor(csObject@hh$PSUid,levels=levels(factor(samFO$PSUid))),
  ##           factor(HHstrat,levels=dimnames(yijk)[[3]])),function(x) length(unique(x)))
Mi <- catApply(csObject@hh$SSUid,list(as.character(csObject@hh$PSUid),as.character(HHstrat)),function(x) length(unique(x)))$val[names(mi$val)]


#------
# n : number of sampled trips
#------
n <- tapply(samFO$PSUid,list(factor(samFOstrat,levels=unique(as.character(yijk.new$ind[3,])))),function(x) length(unique(x)))

#------
# N : total number of trips
#------
if (all(is.na(ceObject@ce$trpNum))) stop("no available data in 'trpNum' field of 'ceObject' for raising process!!")
if (any(is.na(ceObject@ce$trpNum))) warning("missing values for 'trpNum' field in ceObject!!")
CEstrat <- paste(ceObject@ce$time,ceObject@ce$space,ceObject@ce$technical,sep=":-:")
N <- tapply(ceObject@ce$trpNum,list(factor(CEstrat,levels=unique(as.character(yijk.new$ind[3,])))),sum,na.rm=TRUE)

#so, estimate of the total volume is...
  ##yiBar <- RowSum(yijk,c(1,3,4))/as.vector(mi)
tt <- dbeAgg(yijk.new,c(1,3,4),sum,na.rm=TRUE)
yiBar <- list(val = tt$val/dbeReplic(mi,tt$ind[1:2,,drop=FALSE])$val, ind = tt$ind)

  ##yI <- RowSum(yiBar*as.vector(Mi),c(2,3))*as.vector(N/n)         
tt <- dbeAgg(list(val = yiBar$val * dbeReplic(list(val = Mi, ind= mi$ind),tt$ind[1:2,,drop=FALSE])$val , ind = yiBar$ind),2:3,sum,na.rm=TRUE)
yI <- list(val = tt$val*(N/n)[tt$ind[1,]] , ind = tt$ind)

#and, its associated variance is...
  ##yiHat <- yiBar*as.vector(Mi) 
yiHat <- list(val = yiBar$val*dbeReplic(list(val=Mi,ind=mi$ind),yiBar$ind[1:2,,drop=FALSE])$val, ind= yiBar$ind)
  ##yBar <- RowSum(yiHat,c(2,3))/as.vector(n)
tt <- dbeAgg(yiHat,c(2,3),sum,na.rm=TRUE)
yBar <- list(val = tt$val/(n)[tt$ind[1,]] , ind = tt$ind)

  ##s2i <- RowSum(aperm(aperm(yijk,c(1,3,4,2))-rep(yiBar,dim(yijk)[2]),c(1,4,2,3))^2,c(1,3,4))/as.vector(mi-1)
tt <- dbeAgg(list(val = (yijk.new$val - dbeReplic(yiBar,yijk.new$ind[c(1,3:4),,drop=FALSE])$val)^2, ind = yijk.new$ind),c(1,3,4),sum,na.rm=TRUE) 
s2i <- list(val = tt$val/dbeReplic(list(val = mi$val-1, ind = mi$ind),tt$ind[1:2,,drop=FALSE])$val , ind = tt$ind)

  #Nan & Inf values in 's2i' must be replaced by 0 (mi=1 ==> var=0)
s2i$val[is.nan(s2i$val)] <- 0 ; s2i$val[is.infinite(s2i$val)] <- 0 

  #first part of the formula of variance
  ##first <- RowSum((yiHat-rep(yBar,each=dim(yiHat)[1]))^2,c(2,3))*as.vector(N*N/(n*(n-1)))
tt <- dbeAgg(list(val = (yiHat$val - dbeReplic(yBar,yiHat$ind[2:3,,drop=FALSE])$val)^2, ind = yiHat$ind),c(2,3),sum,na.rm=TRUE)
first <- tt$val * (N*N/(n*(n-1)))[tt$ind[1,]]

first[is.nan(first)] <- 0 ; first[is.infinite(first)] <- 0                  #(variance inter trip, so n=1 ==> first=0 ????)
  
  #second part of the formula
  ##second <- RowSum(s2i*as.vector(Mi*(Mi-mi)/mi),c(2,3))*as.vector(N/n)                     #variance intra trip
tt <- dbeAgg(list(val= s2i$val * dbeReplic(list(val = Mi*(Mi-mi$val)/mi$val, ind = mi$ind), s2i$ind[1:2,,drop=FALSE])$val , ind = s2i$ind) , 
                    2:3, sum, na.rm=TRUE) 
second <- tt$val * (N/n)[tt$ind[1,]]
#VyI
VyI <- first + second


#"dbeOutput" object is updated
est <- switch(val,
        weight="totalW",
        number="totalN",
        nAtLength="lenStruc")
vr <- switch(val,
        weight="totalWvar",
        number="totalNvar",
        nAtLength="lenVar")

lgTest <- FALSE ; if (val=="nAtLength") lgTest <- TRUE

#number of samples and number of fish measured 
if (val=="nAtLength") {                                                                                                                                 
nMEAS <- spdAgreg(list(value=VAL$lenNum),BY=list(time=VAL$time,space=VAL$space,technical=VAL$technical),sum,na.rm=TRUE)             #
nMEAS <- merge(nSamp[,c("time","space","technical")],nMEAS,all.x=TRUE,sort=TRUE) ; nMEAS$value[is.na(nMEAS$value)] <- 0
dbeOutp@nMeas$len <- nMEAS                                                                                                                                        #
dbeOutp@nSamp$len <- nSamp[,c("time","space","technical","value")]                  #modif 03/04/2009
}

slot(dbeOutp,est)$estim <- procRaise.format(yI$val,lg=lgTest)
slot(dbeOutp,vr) <- procRaise.format(VyI,lg=lgTest)

return(dbeOutp)
}







#-------------------------------------------------------------------------------
# Raising methodology based on 4.2 section of 'Raising procedures for discards : Sampling theory' (J. Vigneau, 2006)  --> raising by hauls 
# >>>----------> Mbar is estimated from the sample (sum(Mi)/n)
#-------------------------------------------------------------------------------

procRaise.fo <- function(csObject,                      #consolidated CS table
                         ceObject,                      #consolidated CE table (same stratification as csObjet)
                         dbeOutp,                       #'dbeOutput' object with descriptive fields 
                         val="weight",                  #value to raise ("weight"or "number" or "nAtLength"))
                         sampPar=TRUE,                  #'sampPar' checks if given species is considered to be automatically sampled
                         ...) {

dbeOutp@catchCat <- toupper(dbeOutp@catchCat)                               #
csObject@sl$sort <- toupper(csObject@sl$sort)                                   # MM 29/04/2010
csObject@hl$sort <- toupper(csObject@hl$sort)                                   #
csObject@ca$sort <- toupper(csObject@ca$sort)                                   #
                         
if (!val%in%c("weight","number","nAtLength")) {                                                                                                    #\\#
  stop("wrong 'val' parameter!!")                                                                                                                   #\\#
}

fraction <- dbeOutp@catchCat[1]

if (length(dbeOutp@catchCat)>1) {
  warning("only the first specified catch category will be considered!!")
}

if (!fraction%in%c("DIS","LAN")) {
  warning("wrong 'catchCat' slot!!")
}
  
if (dbeOutp@methodDesc!="analytical") {
  warnings("'dbeOutput' object doesn't match the method!! 'methodDesc' slot will be updated!")
  dbeOutp@methodDesc <- "analytitcal"}
  
species <- dbeOutp@species
if ("all"%in%species) species <- unique(as.character(csObject@sl$spp))



#according to 'val' and available information, hauls are considered as sampled or not
indSam <- sampledFO(csObject,species=species,fraction=fraction,sampPar=sampPar)
if (val=="weight") indSam <- indSam$sampWt else indSam <- indSam$sampLg
#sampled FOs are ...
samFO <- csObject@hh[!is.na(indSam),c("PSUid","SSUid","time","space","technical")] ; samFO$ind <- 1   #specifies matrix dimensions
nSamp <- aggregate(samFO$ind,list(technical=samFO$technical,space=samFO$space,time=samFO$time),sum)   #number of samples             #modif 11/12/2008
names(nSamp)[ncol(nSamp)] <- "value"                                                                                                 #
#so, values to be considered are ...
if (val=="weight") {
  VAL <- csObject@sl
  VAL$vol <- VAL$wt/1000                    #weights in kg
  VAL$lenCls <- "all"                                                                                                                          #\\#
} else {
  VAL <- merge(slSex(csObject@sl,csObject@hl),csObject@sl[,c("PSUid","SSUid","TSUid","spp","sort","sex","wt","subSampWt")],all.x=TRUE,sort=FALSE)
  VAL$vol <- VAL$lenNum*VAL$wt/VAL$subSampWt
  if (val=="number") VAL$lenCls <- "all"                                                                                                           #\\#
}

VAL <- VAL[VAL$spp%in%species,] ; VAL <- merge(VAL,samFO,all.x=TRUE)
VAL <- VAL[extCatchCat(VAL$sort)%in%fraction & !is.na(VAL$ind),]
if (nrow(VAL)==0) stop("no available sampling data for specified parameters!!")
#stratification field is built from 'time', 'space' and 'technical' fields for each df
VALstrat <- paste(VAL$time,VAL$space,VAL$technical,sep=":-:")
samFOstrat <- paste(samFO$time,samFO$space,samFO$technical,sep=":-:")          
HHstrat <- paste(csObject@hh$time,csObject@hh$space,csObject@hh$technical,sep=":-:")

  ##FOind <- tapply(samFO$ind,list(samFO$PSUid,samFO$SSUid,samFOstrat),function(x) return(0))  #index of all sampled FOs
FOind <- catApply(samFO$ind,list(samFO$PSUid,samFO$SSUid,samFOstrat),function(x) return(0))

#------
# yijk : volume in a haul j of a trip i in a class k
#------
if (any(is.na(VAL$vol))) warning("values from hauls defined as sampled hauls are missing!!")
  #yijk <- tapply(VAL$vol,list(factor(VAL$PSUid,levels=levels(factor(samFO$PSUid))),  #all sampled trips and FOs should appear
  #                           factor(VAL$SSUid,levels=levels(factor(samFO$SSUid))),
  #                           factor(VALstrat,levels=levels(factor(samFOstrat))),
  #                           VAL$lenCls),
  #              sum,na.rm=TRUE)
  
yijk <- catApply(VAL$vol,list(as.character(VAL$PSUid),as.character(VAL$SSUid),as.character(VALstrat),VAL$lenCls),sum,na.rm=TRUE)  
  
  
  #sampled FOs that are not recorded in VAL must also appear in yij as 0 values, so...
  ##yijk[is.na(yijk) & rep(!is.na(FOind),dim(yijk)[4])] <- 0   

dfTemp <- as.data.frame(t(FOind$ind))                                                        #
cross <- merge(dfTemp,unique(yijk$ind[4,]),all=TRUE)                                         #
yijk.new <- dbeReplic(yijk,as.matrix(t(cross)))                                              #
yijk.new$val[is.na(yijk.new$val)] <- 0                                                   #####


#------
# mi : number of sampled hauls in a trip i
#------
  ##mi <- tapply(samFO$ind,list(samFO$PSUid,factor(samFOstrat,levels=dimnames(yijk)[[3]])),sum)
mi <- catApply(samFO$ind,list(as.character(samFO$PSUid),as.character(samFOstrat)),sum)  

#------
# Mi : total number of hauls in a trip i
#------
  ##Mi <- tapply(csObject@hh$SSUid,list(factor(csObject@hh$PSUid,levels=levels(factor(samFO$PSUid))),
  ##             factor(HHstrat,levels=dimnames(yijk)[[3]])),function(x) length(unique(x)))
Mi <- catApply(csObject@hh$SSUid,list(as.character(csObject@hh$PSUid),as.character(HHstrat)),function(x) length(unique(x)))$val[names(mi$val)]

#------
# mBar : mean of number of sampled hauls by trip
#------

  ##mBar <- apply(mi,2,mean,na.rm=TRUE)
mBar <- dbeAgg(mi,2,mean,na.rm=TRUE)

#------
# MBar : mean of total number of hauls by trip
#------

  ##MBar <- apply(Mi,2,mean,na.rm=TRUE)
MBar <- dbeAgg(list(val = Mi, ind = mi$ind),2,mean,na.rm=TRUE)

#------
# n : number of sampled trips
#------
n <- tapply(samFO$PSUid,list(factor(samFOstrat,levels=unique(as.character(yijk.new$ind[3,])))),function(x) length(unique(x)))

#------
# Mo : total number of FOs
#------
if (all(is.na(ceObject@ce$foNum))) stop("no available data in 'foNum' field of 'ceObject' for raising process!!")
if (any(is.na(ceObject@ce$foNum))) warning("missing values for 'foNum' field in ceObject!!")
CEstrat <- paste(ceObject@ce$time,ceObject@ce$space,ceObject@ce$technical,sep=":-:")
Mo <- tapply(ceObject@ce$foNum,list(factor(CEstrat,levels=unique(as.character(yijk.new$ind[3,])))),sum,na.rm=TRUE)     

#so, estimate of the total volume is...
  ##yiBar <- RowSum(yijk,c(1,3,4))/as.vector(mi)
tt <- dbeAgg(yijk.new,c(1,3,4),sum,na.rm=TRUE)
yiBar <- list(val = tt$val/dbeReplic(mi,tt$ind[1:2,,drop=FALSE])$val, ind = tt$ind)

  ##yII <- RowSum(yiBar*as.vector(Mi),c(2,3))*as.vector(Mo/apply(Mi,2,sum,na.rm=TRUE))         #if NAs, should be coming from Mo (unavailable population level data)
tt <- dbeAgg(list(val = yiBar$val * dbeReplic(list(val = Mi, ind= mi$ind),yiBar$ind[1:2,,drop=FALSE])$val , ind = yiBar$ind),2:3,sum,na.rm=TRUE)
yII <- tt$val * Mo[tt$ind[1,]] / dbeAgg(list(val=Mi,ind=mi$ind),2,sum,na.rm=TRUE)$val[tt$ind[1,]]


#and, its associated variance is...
  ##yBarBar <- RowSum(yiBar,c(2,3))/as.vector(n)   
tt <- dbeAgg(yiBar,c(2,3),sum,na.rm=TRUE)
yBarBar <- list(val = tt$val/(n)[tt$ind[1,]] , ind = tt$ind)

  ##s2i <- RowSum(aperm(aperm(yijk,c(1,3,4,2))-rep(yiBar,dim(yijk)[2]),c(1,4,2,3))^2,c(1,3,4))/as.vector(mi-1)
tt <- dbeAgg(list(val = (yijk.new$val - dbeReplic(yiBar,yijk.new$ind[c(1,3:4),,drop=FALSE])$val)^2, ind = yijk.new$ind),c(1,3,4),sum,na.rm=TRUE) 
s2i <- list(val = tt$val/dbeReplic(list(val = mi$val-1, ind = mi$ind),tt$ind[1:2,,drop=FALSE])$val , ind = tt$ind)

  #Nan & Inf values in 's2i' must be replaced by 0 (mi=1 ==> var=0)
s2i$val[is.nan(s2i$val)] <- 0 ; s2i$val[is.infinite(s2i$val)] <- 0 
 
  #first part of the formula
  ##first <- RowSum(((yiBar-rep(yBarBar,each=dim(yiBar)[1]))*as.vector(Mi))^2,c(2,3))*as.vector(Mo*Mo/(n*MBar*MBar*(n-1)))
tt <- dbeAgg(list(val = ((yiBar$val - dbeReplic(yBarBar,yiBar$ind[2:3,,drop=FALSE])$val) * 
                  dbeReplic(list(val= Mi,ind = mi$ind),yiBar$ind[1:2,,drop=FALSE])$val)^2, ind = yiBar$ind),c(2,3),sum,na.rm=TRUE)
first <- tt$val * (Mo*Mo/(n*(n-1)))[tt$ind[1,]] / (MBar$val*MBar$val)[tt$ind[1,]]

first[is.nan(first)] <- 0 ; first[is.infinite(first)] <- 0                  #(variance inter trip, so n=1 ==> first=0 ????)
  
  #second part of the formula
  ##second <- RowSum(s2i*as.vector(Mi*(Mi-mi)/mi),c(2,3))*as.vector(Mo/(n*MBar))                     #variance intra trip
tt <- dbeAgg(list(val= s2i$val * dbeReplic(list(val = Mi*(Mi-mi$val)/mi$val, ind = mi$ind), s2i$ind[1:2,,drop=FALSE])$val , ind = s2i$ind) , 
                    2:3, sum, na.rm=TRUE) 
second <- tt$val * (Mo/n)[tt$ind[1,]] / MBar$val[tt$ind[1,]]


#VyI
VyII <- first + second                                      

#"dbeOutput" object is updated
est <- switch(val,
        weight="totalW",
        number="totalN",
        nAtLength="lenStruc")
vr <- switch(val,
        weight="totalWvar",
        number="totalNvar",
        nAtLength="lenVar")

lgTest <- FALSE ; if (val=="nAtLength") lgTest <- TRUE
        
if (val=="nAtLength") {                                                                                                                                 
nMEAS <- spdAgreg(list(value=VAL$lenNum),BY=list(time=VAL$time,space=VAL$space,technical=VAL$technical),sum,na.rm=TRUE)             #
nMEAS <- merge(nSamp[,c("time","space","technical")],nMEAS,all.x=TRUE,sort=TRUE) ; nMEAS$value[is.na(nMEAS$value)] <- 0
dbeOutp@nMeas$len <- nMEAS                                                                                                                                        #
dbeOutp@nSamp$len <- nSamp[,c("time","space","technical","value")]                  #modif 03/04/2009
}

slot(dbeOutp,est)$estim <- procRaise.format(yII,lg=lgTest)
slot(dbeOutp,vr) <- procRaise.format(VyII,lg=lgTest)

return(dbeOutp)

}








#-------------------------------------------------------------------------------
# Raising methodology based on 5th section of 'Raising procedures for discards : Sampling theory' (J. Vigneau, 2006)   
# Volume of discards is proportional to an auxiliary variable : Fishing time
#-------------------------------------------------------------------------------

procRaise.time <- function(csObject,                      #consolidated CS table
                           ceObject,                      #consolidated CE table (same stratification as csObjet)
                           dbeOutp,                       #'dbeOutput' object with descriptive fields
                           val="weight",                  #value to raise ("weight" or "number" or "nAtLength")
                           sampPar=TRUE,                  #'sampPar' checks if given species is considered to be automatically sampled
                           ...) {

dbeOutp@catchCat <- toupper(dbeOutp@catchCat)                               #
csObject@sl$sort <- toupper(csObject@sl$sort)                                   # MM 29/04/2010
csObject@hl$sort <- toupper(csObject@hl$sort)                                   #
csObject@ca$sort <- toupper(csObject@ca$sort)                                   #

if (!val%in%c("weight","number","nAtLength")) {                                                                                                    #\\#
  stop("wrong 'val' parameter!!")                                                                                                                   #\\#
}


fraction <- dbeOutp@catchCat[1]

if (length(dbeOutp@catchCat)>1) {
  warning("only the first specified catch category will be considered!!")
}

if (!fraction%in%c("DIS","LAN")) {
  warning("wrong 'catchCat' slot!!")
}
  
if (dbeOutp@methodDesc!="analytical") {
  warnings("'dbeOutput' object doesn't match the method!! 'methodDesc' slot will be updated!")
  dbeOutp@methodDesc <- "analytitcal"}
  
species <- dbeOutp@species
if ("all"%in%species) species <- unique(as.character(csObject@sl$spp))

#first, 'foDur' information in hh table must be checked
if (any(is.na(csObject@hh$foDur))) warning("Fishing operations with missing 'foDur' information will be considered to be non sampled!!")

#according to 'val' and available information, hauls are considered to be sampled or not
indSam <- sampledFO(csObject,species=species,fraction=fraction,sampPar=sampPar)
if (val=="weight") indSam <- indSam$sampWt else indSam <- indSam$sampLg
#as it's been said in the warning message, hauls with missing fishing duration information are considered to be not sampled
indSam[is.na(csObject@hh$foDur)] <- NA

#sampled FOs are ...
samFO <- csObject@hh[!is.na(indSam),c("PSUid","SSUid","time","space","technical")] ; samFO$ind <- 1   #specifies matrix dimensions
samTrip <- unique(samFO[,c("PSUid","time","space","technical","ind")])                                                                   #modif 29/01/2009
nSamp <- aggregate(samTrip$ind,list(technical=samTrip$technical,space=samTrip$space,time=samTrip$time),sum)   #number of samples (trips) #        
names(nSamp)[ncol(nSamp)] <- "value"                                                                                                 #
#so, values to be considered are ...
if (val=="weight") {
  VAL <- csObject@sl
  VAL$vol <- VAL$wt/1000          #weights in kg
  VAL$lenCls <- "all"                                                                                                                          #\\#
} else {
  VAL <- merge(slSex(csObject@sl,csObject@hl),csObject@sl[,c("PSUid","SSUid","TSUid","spp","sort","sex","wt","subSampWt")],all.x=TRUE,sort=FALSE)
  VAL$vol <- VAL$lenNum*VAL$wt/VAL$subSampWt
  if (val=="number") VAL$lenCls <- "all"                                                                                                           #\\#
}


VAL <- VAL[VAL$spp%in%species,] ; VAL <- merge(VAL,samFO,all.x=TRUE)
VAL <- VAL[extCatchCat(VAL$sort)%in%fraction & !is.na(VAL$ind),]
if (nrow(VAL)==0) stop("no available sampling data for specified parameters!!")
#stratification field is built from 'time', 'space' and 'technical' fields for each df
VALstrat <- paste(VAL$time,VAL$space,VAL$technical,sep=":-:")
samFOstrat <- paste(samFO$time,samFO$space,samFO$technical,sep=":-:")          
HHstrat <- paste(csObject@hh$time,csObject@hh$space,csObject@hh$technical,sep=":-:")

  ##FOind <- tapply(samFO$ind,list(samFO$PSUid,samFO$SSUid,samFOstrat),function(x) return(0))  #index of all sampled FOs
FOind <- catApply(samFO$ind,list(samFO$PSUid,samFO$SSUid,samFOstrat),function(x) return(0))

#------
# yijk : volume in a haul j of a trip i for a class k
#------
if (any(is.na(VAL$vol))) warning("values from hauls defined as sampled hauls are missing!!")
  ##yijk <- tapply(VAL$vol,list(factor(VAL$PSUid,levels=levels(factor(samFO$PSUid))),  #all sampled trips and FOs should appear
  ##                           factor(VAL$SSUid,levels=levels(factor(samFO$SSUid))),
  ##                           factor(VALstrat,levels=levels(factor(samFOstrat))),
  ##                           VAL$lenCls),
  ##              sum,na.rm=TRUE)
yijk <- catApply(VAL$vol,list(as.character(VAL$PSUid),as.character(VAL$SSUid),as.character(VALstrat),VAL$lenCls),sum,na.rm=TRUE)  
              
  #sampled FOs that are not recorded in VAL must also appear in yij as 0 values, so...
  ##yijk[is.na(yijk) & rep(!is.na(FOind),dim(yijk)[4])] <- 0   

dfTemp <- as.data.frame(t(FOind$ind))                                                        #
cross <- merge(dfTemp,unique(yijk$ind[4,]),all=TRUE)                                         #
yijk.new <- dbeReplic(yijk,as.matrix(t(cross)))                                              #
yijk.new$val[is.na(yijk.new$val)] <- 0                                                   #####

ref <- dbeAgg(yijk.new,c(1:3),sum,na.rm=TRUE)

#------
# xij : fishing time in a haul j of a trip i (in minutes)
#------
  ##xij <- tapply(csObject@hh$foDur,list(factor(csObject@hh$PSUid,levels=levels(factor(samFO$PSUid))),  
  ##                                     factor(csObject@hh$SSUid,levels=levels(factor(samFO$SSUid))),
  ##                                     factor(HHstrat,levels=levels(factor(samFOstrat)))),
  ##              sum,na.rm=TRUE)
TT <- catApply(csObject@hh$foDur,list(as.character(csObject@hh$PSUid),as.character(csObject@hh$SSUid),as.character(HHstrat)),sum,na.rm=TRUE)  
#xij <- dbeReplic(tt,yijk.new$ind[1:3,])  

##  #sampled FOs are those indexed in FOind, so...
TT <- list(val = TT$val[names(ref$val)], ind = ref$ind)                   
##

#------
# mi : number of sampled hauls in a trip i
#------
  ##mi <- tapply(samFO$ind,list(samFO$PSUid,factor(samFOstrat,levels=dimnames(yijk)[[3]])),sum)
mi <- catApply(samFO$ind,list(as.character(samFO$PSUid),as.character(samFOstrat)),sum) 

#------
# Mi : total number of hauls in a trip i
#------
  ##Mi <- tapply(csObject@hh$SSUid,list(factor(csObject@hh$PSUid,levels=levels(factor(samFO$PSUid))),
  ##             factor(HHstrat,levels=dimnames(yijk)[[3]])),function(x) length(unique(x)))
Mi <- catApply(csObject@hh$SSUid,list(as.character(csObject@hh$PSUid),as.character(HHstrat)),function(x) length(unique(x)))$val[names(mi$val)]

#------
# n : number of sampled trips
#------
n <- tapply(samFO$PSUid,list(factor(samFOstrat,levels=unique(as.character(yijk.new$ind[3,])))),function(x) length(unique(x)))


#------
# N : total number of trips
#------
if (all(is.na(ceObject@ce$trpNum))) stop("no available data in 'trpNum' field of 'ceObject' for raising process!!")
if (all(is.na(ceObject@ce$foDur))) stop("no available data in 'foDur' field of 'ceObject' for raising process!!")

CEstrat <- paste(ceObject@ce$time,ceObject@ce$space,ceObject@ce$technical,sep=":-:")
if (any(is.na(ceObject@ce$trpNum))) warning("missing values for 'trpNum' field in ceObject!!")
if (any(is.na(ceObject@ce$foDur))) warning("missing values for 'foDur' field in ceObject!!")
  ##N <- tapply(ceObject@ce$trpNum,list(factor(CEstrat,levels=dimnames(yijk)[[3]])),sum,na.rm=TRUE)
N <- tapply(ceObject@ce$trpNum,list(factor(CEstrat,levels=unique(as.character(yijk.new$ind[3,])))),sum,na.rm=TRUE)

#------
# X : total stratified fishing time at population level (in minutes)
#------
  ##X <- tapply(ceObject@ce$foDur*60,list(factor(CEstrat,levels=dimnames(yijk)[[3]])),sum,na.rm=TRUE)       #in minutes
X <- tapply(ceObject@ce$foDur*60,list(factor(CEstrat,levels=unique(as.character(yijk.new$ind[3,])))),sum,na.rm=TRUE)

#so, estimate of the total volume is...
  ##yiBar <- RowSum(yijk,c(1,3,4))/as.vector(mi) 
tt <- dbeAgg(yijk.new,c(1,3,4),sum,na.rm=TRUE)
yiBar <- list(val = tt$val/dbeReplic(mi,tt$ind[1:2,,drop=FALSE])$val, ind = tt$ind)

  ##xiBar <- RowSum(xij,c(1,3))/mi 
tt <- dbeAgg(TT,c(1,3),sum,na.rm=TRUE)
xiBar <- list(val = tt$val/mi$val[names(tt$val)], ind = tt$ind)
    
  ##Rhat <- RowSum(yiBar*as.vector(Mi),c(2,3))/as.vector(apply(Mi*xiBar,2,sum,na.rm=TRUE))        
tt <- dbeAgg(list(val = yiBar$val * dbeReplic(list(val = Mi, ind= mi$ind),yiBar$ind[1:2,,drop=FALSE])$val , ind = yiBar$ind),2:3,sum,na.rm=TRUE)
tt2 <- dbeAgg(list(val = xiBar$val * Mi[names(xiBar$val)] , ind = xiBar$ind),2,sum,na.rm=TRUE)
Rhat <- list(val = tt$val / tt2$val[tt$ind[1,]] , ind = tt$ind)
    
yIII <- Rhat$val*X[Rhat$ind[1,]]                                                                    #if NAs, should be coming from X (unavailable population level data)

#and, its associated variance is...

  ##yiHat <- yiBar*as.vector(Mi) 
yiHat <- list(val = yiBar$val*dbeReplic(list(val=Mi,ind=mi$ind),yiBar$ind[1:2,,drop=FALSE])$val, ind= yiBar$ind)

  ##xiHat <- Mi*xiBar
xiHat <- xiBar$val * Mi[names(xiBar$val)]
  
  ##s2iPart1 <- yijk-rep(xij,dim(yijk)[4])*rep(Rhat,each=prod(dim(yijk)[1:2]))  #warning : length data in 'yijk and 'Rhat', but not in 'xij'
s2iPart1 <- list(val = yijk.new$val - dbeReplic(TT,yijk.new$ind[1:3,,drop=FALSE])$val * dbeReplic(Rhat,yijk.new$ind[3:4,,drop=FALSE])$val , ind = yijk.new$ind)

  ##s2iPart2 <- yiBar-rep(xiBar,dim(yijk)[4])*rep(Rhat,each=dim(xiBar)[1])
s2iPart2 <- list(val = yiBar$val - dbeReplic(xiBar,yiBar$ind[1:2,,drop=FALSE])$val * dbeReplic(Rhat,yiBar$ind[2:3,,drop=FALSE])$val , ind = yiBar$ind)

  ##s2i <- RowSum(aperm(aperm(s2iPart1,c(1,3,4,2))-rep(s2iPart2,dim(s2iPart1)[2]),c(1,4,2,3))^2,c(1,3,4))/as.vector(mi-1)
tt <- dbeAgg(list(val = (s2iPart1$val - dbeReplic(s2iPart2,s2iPart1$ind[c(1,3:4),,drop=FALSE])$val)^2 , ind = s2iPart1$ind),c(1,3,4), sum, na.rm=TRUE)
s2i <- list(val = tt$val / (dbeReplic(mi, tt$ind[1:2,,drop=FALSE])$val - 1) , ind = tt$ind)
  #Nan & Inf values in 's2i' must be replaced by 0 (mi=1 ==> var=0)
s2i$val[is.nan(s2i$val)] <- 0 ; s2i$val[is.infinite(s2i$val)] <- 0 
  
  #first part of the formula
  ##first <- RowSum((yiHat-rep(xiHat,dim(yijk)[4])*rep(Rhat,each=dim(xiHat)[1]))^2,c(2,3))*as.vector(N*N/(n*(n-1)))  
tt <- dbeAgg(list(val = (yiHat$val - dbeReplic(list(val = xiHat, ind = xiBar$ind),yiHat$ind[1:2,,drop=FALSE])$val * 
                          dbeReplic(Rhat,yiHat$ind[2:3,,drop=FALSE])$val)^2, ind = yiHat$ind),c(2,3),sum,na.rm=TRUE)
first <- tt$val * (N*N/(n*(n-1)))[tt$ind[1,]]

first[is.nan(first)] <- 0 ; first[is.infinite(first)] <- 0                  #not sure about that (variance inter trip, so n=1 ==> first=0 ????)
  
  #second part of the formula
  ##second <- RowSum(s2i*as.vector(Mi*(Mi-mi)/mi),c(2,3))*as.vector(N/n)                     
tt <- dbeAgg(list(val= s2i$val * dbeReplic(list(val = Mi*(Mi-mi$val)/mi$val, ind = mi$ind), s2i$ind[1:2,,drop=FALSE])$val , ind = s2i$ind) , 
                    2:3, sum, na.rm=TRUE) 
second <- tt$val * (N/n)[tt$ind[1,]]


#VyIII
VyIII <- first + second

#"dbeOutput" object is updated
est <- switch(val,
        weight="totalW",
        number="totalN",
        nAtLength="lenStruc")
vr <- switch(val,
        weight="totalWvar",
        number="totalNvar",
        nAtLength="lenVar")

lgTest <- FALSE ; if (val=="nAtLength") lgTest <- TRUE
        
if (val=="nAtLength") {                                                                                                                                 
nMEAS <- spdAgreg(list(value=VAL$lenNum),BY=list(time=VAL$time,space=VAL$space,technical=VAL$technical),sum,na.rm=TRUE)             #
nMEAS <- merge(nSamp[,c("time","space","technical")],nMEAS,all.x=TRUE,sort=TRUE) ; nMEAS$value[is.na(nMEAS$value)] <- 0
dbeOutp@nMeas$len <- nMEAS                                                                                                                                        #
dbeOutp@nSamp$len <- nSamp[,c("time","space","technical","value")]                  #modif 03/04/2009
}
slot(dbeOutp,est)$estim <- procRaise.format(yIII,lg=lgTest)
slot(dbeOutp,vr) <- procRaise.format(VyIII,lg=lgTest)

return(dbeOutp)

}







#-------------------------------------------------------------------------------
# Raising methodology based on 5th section of 'Raising procedures for discards : Sampling theory' (J. Vigneau, 2006) 
# Volume of discards is proportional to an auxiliary variable : Landings of the species (species to be specified with 'landSpp' parameter)
#-------------------------------------------------------------------------------

procRaise.landings <- function(csObject,                      #consolidated CS table
                               ceObject,                      #consolidated CE table (same stratification as csObjet)
                               clObject,                      #consolidated CL table (same stratification as csObjet)
                               dbeOutp,                       #'dbeOutput' object with descriptive fields
                               landSpp=as.character(NA),      #NA (ie landSpp=species),"all" (raising variable is complete landings), or a character vector with species name(s)
                               val="weight",                  #value to raise ("weight"or "number" or "nAtLength")
                               sampPar=TRUE,                  #'sampPar' checks if given species is considered as automatically sampled
                               ...) {

dbeOutp@catchCat <- toupper(dbeOutp@catchCat)                               #
csObject@sl$sort <- toupper(csObject@sl$sort)                                   # MM 29/04/2010
csObject@hl$sort <- toupper(csObject@hl$sort)                                   #
csObject@ca$sort <- toupper(csObject@ca$sort)                                   #

if (!val%in%c("weight","number","nAtLength")) {                                                                                                    #\\#
  stop("wrong 'val' parameter!!")                                                                                                                   #\\#
}

fraction <- dbeOutp@catchCat[1]

if (length(dbeOutp@catchCat)>1) {
  warning("only the first specified catch category will be considered!!")
}

if (!fraction%in%c("DIS","LAN")) {
  warning("wrong 'catchCat' slot!!")
}
  
if (dbeOutp@methodDesc!="analytical") {
  warnings("'dbeOutput' object doesn't match the method!! 'methodDesc' slot will be updated!")
  dbeOutp@methodDesc <- "analytitcal"}
  
species <- dbeOutp@species
if ("all"%in%species) species <- unique(as.character(csObject@sl$spp))

if (all(is.na(landSpp))) landSpp <- species
if ("all"%in%landSpp) landSpp <- species

#according to 'val' and available information, hauls are considered to be sampled or not.
#Since both fraction must be sampled in that case,...
indSamDis <- sampledFO(csObject,species=species,fraction=fraction,sampPar=sampPar)
if (val=="weight") indSamDis <- indSamDis$sampWt else indSamDis <- indSamDis$sampLg
indSamLan <- sampledFO(csObject,species=landSpp,fraction="LAN",sampPar=sampPar)
if (val=="weight") indSamLan <- indSamLan$sampWt else indSamLan <- indSamLan$sampLg
indSam <- (!is.na(indSamDis) & !is.na(indSamLan))   #only hauls where discards and landings have been sampled are kept for the calculation 

#sampled FOs are ...
samFO <- csObject@hh[indSam,c("PSUid","SSUid","time","space","technical")] ; samFO$ind <- 1   #specifies matrix dimensions
samTrip <- unique(samFO[,c("PSUid","time","space","technical","ind")])                                                                   #modif 29/01/2009
nSamp <- aggregate(samTrip$ind,list(technical=samTrip$technical,space=samTrip$space,time=samTrip$time),sum)   #number of samples (trips) #        
names(nSamp)[ncol(nSamp)] <- "value"                                                                                                 #
#so, values to be considered are ...
if (val=="weight") {
  VAL <- csObject@sl
  VAL$vol <- VAL$wt/1000         #weights in kg
  VAL$lenCls <- "all"                                                                                                                          #\\#
} else {
  VAL <- merge(slSex(csObject@sl,csObject@hl),csObject@sl[,c("PSUid","SSUid","TSUid","spp","sort","sex","wt","subSampWt")],all.x=TRUE,sort=FALSE)
  VAL$vol <- VAL$lenNum*VAL$wt/VAL$subSampWt
  if (val=="number") VAL$lenCls <- "all"                                                                                                           #\\#
}

LAN <- csObject@sl ; LAN$vol <- LAN$wt/1000 ; LAN$lenCls <- "all"  #modif MM : 01/03/2010

VAL <- VAL[VAL$spp%in%species,] ; VAL <- merge(VAL,samFO,all.x=TRUE)
VAL <- VAL[extCatchCat(VAL$sort)%in%fraction & !is.na(VAL$ind),]
if (nrow(VAL)==0) stop("no available discards sampling data for specified parameters!!")
#stratification field is built from 'time', 'space' and 'technical' fields for each df
VALstrat <- paste(VAL$time,VAL$space,VAL$technical,sep=":-:")
samFOstrat <- paste(samFO$time,samFO$space,samFO$technical,sep=":-:")          
HHstrat <- paste(csObject@hh$time,csObject@hh$space,csObject@hh$technical,sep=":-:")

#sampled landed weights for specified species
LAN <- LAN[LAN$spp%in%landSpp,] ; LAN <- merge(LAN,samFO,all.x=TRUE)
LAN <- LAN[extCatchCat(LAN$sort)%in%"LAN" & !is.na(LAN$ind),]
if (nrow(LAN)==0) stop("no available landings sampling data for specified parameters!!")
LANstrat <- paste(LAN$time,LAN$space,LAN$technical,sep=":-:")



#index of all sampled FOs
  ##FOind <- tapply(samFO$ind,list(samFO$PSUid,samFO$SSUid,samFOstrat),function(x) return(0))  
FOind <- catApply(samFO$ind,list(samFO$PSUid,samFO$SSUid,samFOstrat),function(x) return(0))

#------
# yij : volume in a haul j of a trip i
#------
if (any(is.na(VAL$vol))) warning("values from hauls defined as sampled hauls are missing!!")
  ##yijk <- tapply(VAL$vol,list(factor(VAL$PSUid,levels=levels(factor(samFO$PSUid))),  #all sampled trips and FOs should appear
  ##                           factor(VAL$SSUid,levels=levels(factor(samFO$SSUid))),
  ##                           factor(VALstrat,levels=levels(factor(samFOstrat))),
  ##                           VAL$lenCls),
  ##              sum,na.rm=TRUE)

yijk <- catApply(VAL$vol,list(as.character(VAL$PSUid),as.character(VAL$SSUid),as.character(VALstrat),VAL$lenCls),sum,na.rm=TRUE)  

  #sampled FOs that are not recorded in VAL must also appear in yij as 0 values, so... 
  ##yijk[is.na(yijk) & rep(!is.na(FOind),dim(yijk)[4])] <- 0  

dfTemp <- as.data.frame(t(FOind$ind))                                                        #
cross <- merge(dfTemp,unique(yijk$ind[4,]),all=TRUE)                                         #
yijk.new <- dbeReplic(yijk,as.matrix(t(cross)))                                              #
yijk.new$val[is.na(yijk.new$val)] <- 0                                                   #####

ref <- dbeAgg(yijk.new,c(1:3),sum,na.rm=TRUE)

#------
# xij : landed weight of specified species in a haul j of a trip i  (in kg)
#------
  ##xij <- tapply(LAN$wt/1000,list(factor(LAN$PSUid,levels=levels(factor(samFO$PSUid))),  
  ##                               factor(LAN$SSUid,levels=levels(factor(samFO$SSUid))),
  ##                               factor(LANstrat,levels=levels(factor(samFOstrat)))),
  ##              sum,na.rm=TRUE)
              
TT <- catApply(LAN$wt/1000,list(as.character(LAN$PSUid),as.character(LAN$SSUid),as.character(LANstrat)),sum,na.rm=TRUE)  
#xij <- dbeReplic(tt,yijk.new$ind[1:3,])  
              

  #sampled FOs are indexed in FOind, so...
  ##xij[is.na(xij) & !is.na(FOind)] <- 0        
TT <- list(val = TT$val[names(ref$val)], ind = ref$ind)   

#------
# mi : number of sampled hauls in a trip i
#------
  ##mi <- tapply(samFO$ind,list(samFO$PSUid,factor(samFOstrat,levels=dimnames(yijk)[[3]])),sum)
mi <- catApply(samFO$ind,list(as.character(samFO$PSUid),as.character(samFOstrat)),sum) 

#------
# Mi : total number of hauls in a trip i
#------
  ##Mi <- tapply(csObject@hh$SSUid,list(factor(csObject@hh$PSUid,levels=levels(factor(samFO$PSUid))),
  ##             factor(HHstrat,levels=dimnames(yijk)[[3]])),function(x) length(unique(x)))

Mi <- catApply(csObject@hh$SSUid,list(as.character(csObject@hh$PSUid),as.character(HHstrat)),function(x) length(unique(x)))$val[names(mi$val)]

#------
# n : number of sampled trips
#------
n <- tapply(samFO$PSUid,list(factor(samFOstrat,levels=unique(as.character(yijk.new$ind[3,])))),function(x) length(unique(x)))


#------
# N : total number of trips
#------
CEstrat <- paste(ceObject@ce$time,ceObject@ce$space,ceObject@ce$technical,sep=":-:")
if (any(is.na(ceObject@ce$trpNum))) warning("missing values for 'trpNum' field in ceObject!!")
  ##N <- tapply(ceObject@ce$trpNum,list(factor(CEstrat,levels=dimnames(yijk)[[3]])),sum,na.rm=TRUE)
N <- tapply(ceObject@ce$trpNum,list(factor(CEstrat,levels=unique(as.character(yijk.new$ind[3,])))),sum,na.rm=TRUE)

#------
# X : total stratified landed weights at population level for landSpp species (in kg)
#------
CLt <- clObject@cl[clObject@cl$taxon%in%landSpp,]
CLstrat <- paste(CLt$time,CLt$space,CLt$technical,sep=":-:")
if (any(is.na(CLt$landWt))) warning("missing values for 'landWt' field in clObject!!")                  
CLt$landMult[is.na(CLt$landMult)] <- 1                                                                                         #modif 11/12/2008
TotLand <- mapply(function(w,x,y,z) sum(c(w*x,y,z),na.rm=TRUE),CLt$landWt,CLt$landMult,CLt$unallocCatchWt,CLt$misRepCatchWt)   ## TotLand = OffLand*Multi + UnallocCat + MisallocCat 
  ##X <- tapply(TotLand,list(factor(CLstrat,levels=dimnames(yijk)[[3]])),sum,na.rm=TRUE)                                              
X <- tapply(TotLand,list(factor(CLstrat,levels=unique(as.character(yijk.new$ind[3,])))),sum,na.rm=TRUE)

#so, estimate of the total volume is...

  ##yiBar <- RowSum(yijk,c(1,3,4))/as.vector(mi) 
tt <- dbeAgg(yijk.new,c(1,3,4),sum,na.rm=TRUE)
yiBar <- list(val = tt$val/dbeReplic(mi,tt$ind[1:2,,drop=FALSE])$val, ind = tt$ind)

  ##xiBar <- RowSum(xij,c(1,3))/mi 
tt <- dbeAgg(TT,c(1,3),sum,na.rm=TRUE)
xiBar <- list(val = tt$val/mi$val[names(tt$val)], ind = tt$ind)
    
  ##Rhat <- RowSum(yiBar*as.vector(Mi),c(2,3))/as.vector(apply(Mi*xiBar,2,sum,na.rm=TRUE))        
tt <- dbeAgg(list(val = yiBar$val * dbeReplic(list(val = Mi, ind= mi$ind),yiBar$ind[1:2,,drop=FALSE])$val , ind = yiBar$ind),2:3,sum,na.rm=TRUE)
tt2 <- dbeAgg(list(val = xiBar$val * Mi[names(xiBar$val)] , ind = xiBar$ind),2,sum,na.rm=TRUE)
Rhat <- list(val = tt$val / tt2$val[tt$ind[1,]] , ind = tt$ind)
    
Rhat$val[is.nan(Rhat$val)] <- 0

yIII <- Rhat$val*X[Rhat$ind[1,]]                                                                    #if NAs, should be coming from X (unavailable population level data)

#and, its associated variance is...

  ##yiHat <- yiBar*as.vector(Mi) 
yiHat <- list(val = yiBar$val*dbeReplic(list(val=Mi,ind=mi$ind),yiBar$ind[1:2,,drop=FALSE])$val, ind= yiBar$ind)

  ##xiHat <- Mi*xiBar
xiHat <- xiBar$val * Mi[names(xiBar$val)]
  
  ##s2iPart1 <- yijk-rep(xij,dim(yijk)[4])*rep(Rhat,each=prod(dim(yijk)[1:2]))  #warning : length data in 'yijk and 'Rhat', but not in 'xij'
s2iPart1 <- list(val = yijk.new$val - dbeReplic(TT,yijk.new$ind[1:3,,drop=FALSE])$val * dbeReplic(Rhat,yijk.new$ind[3:4,,drop=FALSE])$val , ind = yijk.new$ind)

  ##s2iPart2 <- yiBar-rep(xiBar,dim(yijk)[4])*rep(Rhat,each=dim(xiBar)[1])
s2iPart2 <- list(val = yiBar$val - dbeReplic(xiBar,yiBar$ind[1:2,,drop=FALSE])$val * dbeReplic(Rhat,yiBar$ind[2:3,,drop=FALSE])$val , ind = yiBar$ind)

  ##s2i <- RowSum(aperm(aperm(s2iPart1,c(1,3,4,2))-rep(s2iPart2,dim(s2iPart1)[2]),c(1,4,2,3))^2,c(1,3,4))/as.vector(mi-1)
tt <- dbeAgg(list(val = (s2iPart1$val - dbeReplic(s2iPart2,s2iPart1$ind[c(1,3:4),,drop=FALSE])$val)^2 , ind = s2iPart1$ind),c(1,3,4), sum, na.rm=TRUE)
s2i <- list(val = tt$val / (dbeReplic(mi, tt$ind[1:2,,drop=FALSE])$val - 1) , ind = tt$ind)
  #Nan & Inf values in 's2i' must be replaced by 0 (mi=1 ==> var=0)
s2i$val[is.nan(s2i$val)] <- 0 ; s2i$val[is.infinite(s2i$val)] <- 0 
  
  #first part of the formula
  ##first <- RowSum((yiHat-rep(xiHat,dim(yijk)[4])*rep(Rhat,each=dim(xiHat)[1]))^2,c(2,3))*as.vector(N*N/(n*(n-1)))  
tt <- dbeAgg(list(val = (yiHat$val - dbeReplic(list(val = xiHat, ind = xiBar$ind),yiHat$ind[1:2,,drop=FALSE])$val * 
                          dbeReplic(Rhat,yiHat$ind[2:3,,drop=FALSE])$val)^2, ind = yiHat$ind),c(2,3),sum,na.rm=TRUE)
first <- tt$val * (N*N/(n*(n-1)))[tt$ind[1,]]

first[is.nan(first)] <- 0 ; first[is.infinite(first)] <- 0                  #not sure about that (variance inter trip, so n=1 ==> first=0 ????)
  
  #second part of the formula
  ##second <- RowSum(s2i*as.vector(Mi*(Mi-mi)/mi),c(2,3))*as.vector(N/n)                     
tt <- dbeAgg(list(val= s2i$val * dbeReplic(list(val = Mi*(Mi-mi$val)/mi$val, ind = mi$ind), s2i$ind[1:2,,drop=FALSE])$val , ind = s2i$ind) , 
                    2:3, sum, na.rm=TRUE) 
second <- tt$val * (N/n)[tt$ind[1,]]


#VyIII
VyIII <- first + second

          
#"dbeOutput" object is updated
est <- switch(val,
        weight="totalW",
        number="totalN",
        nAtLength="lenStruc")
vr <- switch(val,
        weight="totalWvar",
        number="totalNvar",
        nAtLength="lenVar")

lgTest <- FALSE ; if (val=="nAtLength") lgTest <- TRUE
        
if (val=="nAtLength") {                                                                                                                                 
nMEAS <- spdAgreg(list(value=VAL$lenNum),BY=list(time=VAL$time,space=VAL$space,technical=VAL$technical),sum,na.rm=TRUE)             #
nMEAS <- merge(nSamp[,c("time","space","technical")],nMEAS,all.x=TRUE,sort=TRUE) ; nMEAS$value[is.na(nMEAS$value)] <- 0
dbeOutp@nMeas$len <- nMEAS                                                                                                                                        #
dbeOutp@nSamp$len <- nSamp[,c("time","space","technical","value")]                  #modif 03/04/2009
}
slot(dbeOutp,est)$estim <- procRaise.format(yIII,lg=lgTest)
slot(dbeOutp,vr) <- procRaise.format(VyIII,lg=lgTest)

return(dbeOutp)
}








#-------------------------------------------------------------------------------
# New raising methodology based on fishing days at population level
#-------------------------------------------------------------------------------

procRaise.fd <- function(csObject,                      #consolidated CS table
                         ceObject,                      #consolidated CE table (same stratification as csObjet)
                         dbeOutp,                       #'dbeOutput' object with descriptive fields
                         val="weight",                  #value to raise ("weight"or "number" or "nAtLength")
                         sampPar=TRUE,                  #'sampPar' checks if given species is considered as automatically sampled
                         ...) {

dbeOutp@catchCat <- toupper(dbeOutp@catchCat)                               #
csObject@sl$sort <- toupper(csObject@sl$sort)                                   # MM 29/04/2010
csObject@hl$sort <- toupper(csObject@hl$sort)                                   #
csObject@ca$sort <- toupper(csObject@ca$sort)                                   #

if (!val%in%c("weight","number","nAtLength")) {                                                                                                    #\\#
  stop("wrong 'val' parameter!!")                                                                                                                   #\\#
}
    
fraction <- dbeOutp@catchCat[1]

if (length(dbeOutp@catchCat)>1) {
  warning("only the first specified catch category will be considered!!")
}

if (!fraction%in%c("DIS","LAN")) {
  warning("wrong 'catchCat' slot!!")
}
 
if (dbeOutp@methodDesc!="analytical") {
  warnings("'dbeOutput' object doesn't match the method!! 'methodDesc' slot will be updated!")
  dbeOutp@methodDesc <- "analytical"}
  
species <- dbeOutp@species
if ("all"%in%species) species <- unique(as.character(csObject@sl$spp))
                  
#according to 'val' and available information, hauls are considered to be sampled or not
indSam <- sampledFO(csObject,species=species,fraction=fraction,sampPar=sampPar)
if (val=="weight") indSam <- indSam$sampWt else indSam <- indSam$sampLg
#sampled FOs are ...
samFO <- csObject@hh[!is.na(indSam),c("PSUid","SSUid","time","space","technical","date")] ; samFO$ind <- 1   #specifies matrix dimensions
#unused levels are deleted
samFO <- model.frame(PSUid~SSUid+time+space+technical+date+ind,data=samFO,drop.unused.level=TRUE)
samDay <- unique(samFO[,c("PSUid","date","time","space","technical","ind")])                                                                   #modif 29/01/2009
nSamp <- aggregate(samDay$ind,list(technical=samDay$technical,space=samDay$space,time=samDay$time),sum)   #number of samples (fishing days)    #        
names(nSamp)[ncol(nSamp)] <- "value"                                                                                                 #
#so, values to be considered are ...
if (val=="weight") {
  VAL <- csObject@sl
  VAL$vol <- VAL$wt/1000             # weights in kg
  VAL$lenCls <- "all"                                                                                                                          #\\#
} else {
  VAL <- merge(slSex(csObject@sl,csObject@hl),csObject@sl[,c("PSUid","SSUid","TSUid","spp","sort","sex","wt","subSampWt")],all.x=TRUE,sort=FALSE)
  VAL$vol <- VAL$lenNum*VAL$wt/VAL$subSampWt
  if (val=="number") VAL$lenCls <- "all"                                                                                                           #\\#
}

VAL <- VAL[VAL$spp%in%species,] ; VAL <- merge(VAL,samFO,all.x=TRUE)
VAL <- VAL[extCatchCat(VAL$sort)%in%fraction & !is.na(VAL$ind),]
if (nrow(VAL)==0) stop("no available sampling data for specified parameters!!")
VALstrat <- paste(VAL$time,VAL$space,VAL$technical,sep=":-:")
samFOstrat <- paste(samFO$time,samFO$space,samFO$technical,sep=":-:")          
HHstrat <- paste(csObject@hh$time,csObject@hh$space,csObject@hh$technical,sep=":-:")

  ##FOind <- tapply(samFO$ind,list(samFO$PSUid,samFO$date,samFO$SSUid,samFOstrat),function(x) return(0))  #index of all sampled FOs
FOind <- catApply(samFO$ind,list(samFO$PSUid,samFO$date,samFO$SSUid,samFOstrat),function(x) return(0))

#------
# yikjl : volume in a haul j of a fishing day k, in a trip i for a class l
#------
if (any(is.na(VAL$vol))) warning("values from hauls defined as sampled hauls are missing!!")
  ##yikjl <- tapply(VAL$vol,list(factor(VAL$PSUid,levels=levels(factor(samFO$PSUid))),  #all sampled trip should appear
  ##                           factor(VAL$date,levels=levels(factor(samFO$date))),
  ##                           factor(VAL$SSUid,levels=levels(factor(samFO$SSUid))),
  ##                           factor(VALstrat,levels=levels(factor(samFOstrat))),
  ##                           VAL$lenCls),
  ##              sum,na.rm=TRUE)
              
yikjl <- catApply(VAL$vol,list(as.character(VAL$PSUid),as.character(VAL$date),as.character(VAL$SSUid),as.character(VALstrat),VAL$lenCls),sum,na.rm=TRUE)  

  #sampled FOs that are not recorded in VAL must also appear in yikj as 0 values
  ##yikjl[is.na(yikjl) & rep(!is.na(FOind),dim(yikjl)[5])] <- 0  

dfTemp <- as.data.frame(t(FOind$ind))                                                        #
cross <- merge(dfTemp,unique(yikjl$ind[5,]),all=TRUE)                                         #
yikjl.new <- dbeReplic(yikjl,as.matrix(t(cross)))                                              #
yikjl.new$val[is.na(yikjl.new$val)] <- 0                                                   #####

#------
# mik : number of sampled hauls in a fishing day k of a trip i
#------
  ##mik <- tapply(samFO$ind,list(samFO$PSUid,samFO$date,factor(samFOstrat,levels=dimnames(yikjl)[[4]])),sum)
mik <- catApply(samFO$ind,list(as.character(samFO$PSUid),as.character(samFO$date),as.character(samFOstrat)),sum) 

#------
# Mik : total number of hauls in a fishing day k of a trip i
#------
  ##Mik <- tapply(csObject@hh$SSUid,list(factor(csObject@hh$PSUid,levels=levels(factor(samFO$PSUid))),
  ##             factor(csObject@hh$date,levels=levels(factor(samFO$date))),
  ##             factor(HHstrat,levels=dimnames(yikjl)[[4]])),function(x) length(unique(x)))
Mik <- catApply(csObject@hh$SSUid,list(as.character(csObject@hh$PSUid),as.character(csObject@hh$date),as.character(HHstrat)),
                    function(x) length(unique(x)))$val[names(mik$val)]


#------
# di : number of sampled fishing day in a trip i
#------
  ##di <- tapply(samFO$date,list(samFO$PSUid,factor(samFOstrat,levels=dimnames(yikjl)[[4]])),function(x) length(unique(x)))
di <- catApply(samFO$date,list(as.character(samFO$PSUid),as.character(samFOstrat)),function(x) length(unique(x))) 

#------
# Di : total number of fishing day in a trip i
#------
  ##Di <- tapply(csObject@hh$date,list(factor(csObject@hh$PSUid,levels=levels(factor(samFO$PSUid))),
  ##             factor(HHstrat,levels=dimnames(yikjl)[[4]])),function(x) length(unique(x)))
Di <- catApply(csObject@hh$date,list(as.character(csObject@hh$PSUid),as.character(HHstrat)),function(x) length(unique(x)))$val[names(di$val)] 


#------
# n : number of sampled trips
#------
  ##n <- tapply(samFO$PSUid,list(factor(samFOstrat,levels=dimnames(yikjl)[[4]])),function(x) length(unique(x)))
n <- tapply(samFO$PSUid,list(factor(samFOstrat,levels=unique(as.character(yikjl.new$ind[4,])))),function(x) length(unique(x)))

#------
# D : total number of fishing days
#------
CEstrat <- paste(ceObject@ce$time,ceObject@ce$space,ceObject@ce$technical,sep=":-:")
if (any(is.na(ceObject@ce$daysAtSea))) warning("missing values for 'daysAtSea' field in ceObject!!")
  ##D <- tapply(ceObject@ce$daysAtSea,list(factor(CEstrat,levels=dimnames(yikjl)[[4]])),sum,na.rm=TRUE)
D <- tapply(ceObject@ce$daysAtSea,list(factor(CEstrat,levels=unique(as.character(yikjl.new$ind[4,])))),sum,na.rm=TRUE)

#so, estimate of the total volume is...

  ##yikBar <- RowSum(yikjl,c(1,2,4,5))/as.vector(mik) 
tt <- dbeAgg(yikjl.new,c(1,2,4,5),sum,na.rm=TRUE)
temp <- dbeReplic(mik,tt$ind[1:3,,drop=FALSE])$val
yikBar <- list(val = tt$val/temp, ind = tt$ind)

yikHat <- list(val = yikBar$val * Mik[names(temp)] ,ind =  yikBar$ind)

  ##yiBar <- RowSum(yikHat,c(1,3,4))/as.vector(di)
tt <- dbeAgg(yikHat,c(1,3,4),sum,na.rm=TRUE)
temp <- dbeReplic(di,tt$ind[1:2,,drop=FALSE])$val
yiBar <- list(val = tt$val/temp, ind = tt$ind)

  ##yBarBar <- RowSum(yiBar*as.vector(Di),c(2,3))/as.vector(apply(Di,2,sum,na.rm=TRUE))
tt <- dbeAgg(list(val = yiBar$val * dbeReplic(list(val = Di, ind = di$ind),yiBar$ind[1:2,,drop=FALSE])$val , ind = yiBar$ind), 2:3, sum, na.rm=TRUE)
tt2 <- dbeAgg(list(val = Di, ind = di$ind),2,sum,na.rm=TRUE)
yBarBar <- tt$val / tt2$val[tt$ind[1,]]

yIV <- yBarBar * D[tt$ind[1,]]         #if NAs, should be coming from D (unavailable population level data)


#and, its associated variance is...
  #first part of the formula of variance
  ##first <- RowSum((yiBar-rep(yBarBar,each=dim(yiBar)[1]))^2,c(2,3))*
  ##    as.vector(n*((D/apply(Di,2,sum,na.rm=TRUE))^2)*(1-apply(Di,2,sum,na.rm=TRUE)/D)/(n-1))
yBarBar <- list(val=yBarBar, ind = tt$ind)
tt <- dbeAgg(list(val = (yiBar$val - dbeReplic(yBarBar,yiBar$ind[2:3,,drop=FALSE])$val)^2 ,ind = yiBar$ind), c(2,3), sum, na.rm=TRUE)
ttt <- (n/(n-1))[names(D)] * ((D / tt2$val[names(D)])^2) * (1-tt2$val[names(D)]/D) 
first <- tt$val * ttt[tt$ind[1,]]  

  #Nan & Inf values in 'first' must be replaced by 0 (mi=1 ==> var=0)
first[is.nan(first)] <- 0 ; first[is.infinite(first)] <- 0 

  #second part of the formula of variance
  ##  second <- RowSum(RowSum(aperm(aperm(yikHat,c(1,3,4,2))-as.vector(yiBar),c(1,4,2,3))^2,c(1,3,4))*
  ##     as.vector(Di*(Di-di)/(di*(di-1))),c(2,3))*as.vector(D/apply(Di,2,sum,na.rm=TRUE))
tt1 <- dbeAgg(list(val = (yikHat$val - dbeReplic(yiBar,yikHat$ind[c(1,3:4),,drop=FALSE])$val)^2 ,ind = yikHat$ind), c(1,3,4), sum, na.rm=TRUE)
ttt1 <- dbeReplic(list(val = Di[names(di$val)]*(Di[names(di$val)]-di$val)/(di$val*(di$val-1)) , ind = di$ind), tt1$ind[1:2,,drop=FALSE]) 
tt <- dbeAgg(list(val = tt1$val * ttt1$val, ind = tt1$ind), 2:3, sum, na.rm=TRUE) 
second <- tt$val * (D / tt2$val[names(D)])[tt$ind[1,]] 

  #Nan & Inf values in 'second' must be replaced by 0 (mi=1 ==> var=0)
second[is.nan(second)] <- 0 ; second[is.infinite(second)] <- 0 
                                     
  #third part of the formula of variance
  ##stepp <- RowSum(aperm(aperm(yikjl,c(1,2,4,5,3))-as.vector(yikBar),c(1,2,5,3,4))^2,c(1,2,4,5))*as.vector(Mik*(Mik-mik)/(mik*(mik-1)))
  ##third <- RowSum(RowSum(stepp,c(1,3,4))*as.vector(Di/di),c(2,3))*as.vector(D/apply(Di,2,sum,na.rm=TRUE))                                           
  
stepp <- dbeAgg(list(val = (yikjl.new$val - dbeReplic(yikBar,yikjl.new$ind[c(1,2,4,5),,drop=FALSE])$val)^2 ,ind = yikjl.new$ind), c(1,2,4,5), sum, na.rm=TRUE)
stepp1 <- dbeReplic(list(val = Mik[names(mik$val)]*(Mik[names(mik$val)]-mik$val)/(mik$val*(mik$val-1)), ind = mik$ind), stepp$ind[1:3,,drop=FALSE])
third1 <- dbeAgg(list(val = stepp$val * stepp1$val, ind= stepp$ind), c(1,3,4), sum, na.rm=TRUE)   
third2 <- dbeAgg(
  list(val = third1$val * dbeReplic(list(val = Di[names(di$val)]/di$val, ind = di$ind) , third1$ind[1:2,,drop=FALSE])$val, ind = third1$ind),
  2:3, sum, na.rm=TRUE)  
third <- third2$val * (D / tt2$val[names(D)])[third2$ind[1,]]  
  
  #Nan & Inf values in 'third' must be replaced by 0 (mi=1 ==> var=0)
third[is.nan(third)] <- 0 ; third[is.infinite(third)] <- 0 

  
#VyIV
VyIV <- first + second + third

#"dbeOutput" object is updated
est <- switch(val,
        weight="totalW",
        number="totalN",
        nAtLength="lenStruc")
vr <- switch(val,
        weight="totalWvar",
        number="totalNvar",
        nAtLength="lenVar")

lgTest <- FALSE ; if (val=="nAtLength") lgTest <- TRUE
        
if (val=="nAtLength") {                                                                                                                                 
nMEAS <- spdAgreg(list(value=VAL$lenNum),BY=list(time=VAL$time,space=VAL$space,technical=VAL$technical),sum,na.rm=TRUE)             #
nMEAS <- merge(nSamp[,c("time","space","technical")],nMEAS,all.x=TRUE,sort=TRUE) ; nMEAS$value[is.na(nMEAS$value)] <- 0
dbeOutp@nMeas$len <- nMEAS                                                                                                                                        #
dbeOutp@nSamp$len <- nSamp[,c("time","space","technical","value")]                  #modif 03/04/2009
}
slot(dbeOutp,est)$estim <- procRaise.format(yIV,lg=lgTest)
slot(dbeOutp,vr) <- procRaise.format(VyIV,lg=lgTest)

return(dbeOutp)                        
}



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################










################################################################################
# Methods to be exported
################################################################################


setGeneric("totVolume", function(dbeOutput,
                                 csObject,
                                 ceObject,
                                 clObject,
                                 ...){
	standardGeneric("totVolume")}
)



setMethod("totVolume", signature(dbeOutput="dbeOutput",csObject="csDataCons",ceObject="ceDataCons",clObject="missing"), function(dbeOutput,
                                                                                                                                 csObject,
                                                                                                                                 ceObject,
                                                                                                                                 type="trip",   #or "fo", "fd", "time"
                                                                                                                                 #val="weight",  #or "number" or "nAtLength"
                                                                                                                                 sampPar=TRUE,
                                                                                                                                 incl.precision=TRUE,    ## added MM 26/07/2010
                                                                                                                                 probs=c(0.025,0.975),
                                                                                                                                 ...){
if (type=="landings") stop("'landings' type requires a cs, a ce and a cl object!!")
eval(parse('',text=paste("obj <- procRaise.",type,"(csObject,ceObject,dbeOutput,val=\"weight\",sampPar=sampPar)",sep=""))) 
eval(parse('',text=paste("obj <- procRaise.",type,"(csObject,ceObject,obj,val=\"number\",sampPar=sampPar)",sep=""))) 
eval(parse('',text=paste("obj <- procRaise.",type,"(csObject,ceObject,obj,val=\"nAtLength\",sampPar=sampPar)",sep=""))) 


if (incl.precision) {  

  if (!all(is.na(obj@lenStruc$estim)) & !all(is.na(obj@lenVar))) {
    obj <- dbeCalc(obj,type="CV",vrbl="l",replicates=FALSE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="l",probs=probs,replicates=FALSE,update=TRUE)
  }

  if (!all(is.na(obj@totalN$estim)) & !all(is.na(obj@totalNvar))) {
    obj <- dbeCalc(obj,type="CV",vrbl="n",replicates=FALSE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="n",probs=probs,replicates=FALSE,update=TRUE)
  }

  if (!all(is.na(obj@totalW$estim)) & !all(is.na(obj@totalWvar))) {
    obj <- dbeCalc(obj,type="CV",vrbl="w",replicates=FALSE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="w",probs=probs,replicates=FALSE,update=TRUE)
  }
}

return(obj)

})


setMethod("totVolume", signature(dbeOutput="dbeOutput",csObject="csDataCons",ceObject="ceDataCons",clObject="clDataCons"), function(dbeOutput,
                                                                                                                                    csObject,
                                                                                                                                    ceObject,
                                                                                                                                    clObject,
                                                                                                                                    landSpp=as.character(NA),
                                                                                                                                    #val="weight",  #or "number" or "nAtLength"
                                                                                                                                    sampPar=TRUE,
                                                                                                                                    incl.precision=TRUE,    ## added MM 26/07/2010
                                                                                                                                    probs=c(0.025,0.975),
                                                                                                                                    ...){
para <- match.call()
if (!is.null(para$type)) {
  if (para$type!="landings") warning("CL object as input!! Raising is made by total landings!!")} 
obj <- procRaise.landings(csObject,ceObject,clObject,dbeOutput,landSpp=landSpp,val="weight",sampPar=sampPar)  
obj <- procRaise.landings(csObject,ceObject,clObject,obj,landSpp=landSpp,val="number",sampPar=sampPar)  
obj <- procRaise.landings(csObject,ceObject,clObject,obj,landSpp=landSpp,val="nAtLength",sampPar=sampPar)  


if (incl.precision) {  

  if (!all(is.na(obj@lenStruc$estim)) & !all(is.na(obj@lenVar))) {
    obj <- dbeCalc(obj,type="CV",vrbl="l",replicates=FALSE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="l",probs=probs,replicates=FALSE,update=TRUE)
  }

  if (!all(is.na(obj@totalN$estim)) & !all(is.na(obj@totalNvar))) {
    obj <- dbeCalc(obj,type="CV",vrbl="n",replicates=FALSE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="n",probs=probs,replicates=FALSE,update=TRUE)
  }

  if (!all(is.na(obj@totalW$estim)) & !all(is.na(obj@totalWvar))) {
    obj <- dbeCalc(obj,type="CV",vrbl="w",replicates=FALSE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="w",probs=probs,replicates=FALSE,update=TRUE)
  }
}

return(obj)                                                                                                        

})










###############
###############
##  Example  ##
###############
###############


#library(COSTcore)
#
#source("C:/Documents and Settings/mmerzere/Bureau/0valuesIndex.r")

##consolidated datasets are built for testing 
#strDef <- strIni(timeStrata="quarter",techStrata="foCatEu5")
#csObject <- csDataCons(csDataVal(sole.cs),strDef)
#clObject <- clDataCons(clDataVal(sole.cl),strDef)
#ceObject <- ceDataCons(ceDataVal(sole.ce),strDef)
#
##random foNum are created in ceObject
#ceObject@ce$foNum <- sample(c(15:30),nrow(ceObject@ce),replace=TRUE)*ceObject@ce$trpNum           
#
##random foDur are created in ceObject
#ceObject@ce$foDur <- sample(c(60:180),nrow(ceObject@ce),replace=TRUE)*ceObject@ce$trpNum            
#
##random daysAtSea values are created in ceObject
#ceObject@ce$daysAtSea <- sample(c(1:5),nrow(ceObject@ce),replace=TRUE)*ceObject@ce$trpNum           
#
############################################
#
##dbeOutput initial object
#obj <- dbeObject(desc="My object",species="Solea solea",catchCat="DIS",strataDesc=strDef,methodDesc="analytical")
#obj
#
#raising by trip
#newObj11 <- totVolume(obj,csObject,ceObject)
##raising by haul
#newObj22 <- totVolume(obj,csObject,ceObject,type="fo")
##raising by fishing day
#newObj33 <- totVolume(obj,csObject,ceObject,type="fd")
##raising by fishing duration
#newObj44 <- totVolume(obj,csObject,ceObject,type="time")
##raising by total landings
#newObj55 <- totVolume(obj,csObject,ceObject,clObject)
#


