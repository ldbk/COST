### function that fills 'estimates' and 'variance' tables in a COSTdbe object from 'replicates' table ###
fillTabFromRep <- function(object){

  ##subfunction
  formEst <- function(tabRep,type="l",...){ #type = "l" (length) or "a" (age)
  tabRep$value[is.infinite(tabRep$value) | is.nan(tabRep$value)] <- NA
  if (type=="l") {
    tabRep$length <- factor(as.character(tabRep$length),levels=as.character(unique(tabRep$length)))
    tab <- with(tabRep,aggregate(value,list(length=length,technical=technical,space=space,time=time),...))
    names(tab)[ncol(tab)] <- "value"
  } else {
    tabRep$age <- factor(as.character(tabRep$age),levels=as.character(unique(tabRep$age)))
    tab <- with(tabRep,aggregate(value,list(age=age,technical=technical,space=space,time=time),...))
    names(tab)[ncol(tab)] <- "value"
  }
  tab <- tab[,names(tabRep)[-ncol(tabRep)]]
  return(tab)
  }

if (!all(is.na(object@lenStruc$rep))) {
  object@lenStruc$estim <- formEst(object@lenStruc$rep,type="l",mean,na.rm=TRUE)
  object@lenVar <- formEst(object@lenStruc$rep,type="l",var,na.rm=TRUE)
}

if (!all(is.na(object@ageStruc$rep))) {
  object@ageStruc$estim <- formEst(object@ageStruc$rep,type="a",mean,na.rm=TRUE)
  object@ageVar <- formEst(object@ageStruc$rep,type="a",var,na.rm=TRUE)
}

return(object)
}

################################################################################
################################################################################
################################################################################
################################################################################


############################
#### 'mbe2dbe' function ####
############################


mbe2dbe <- function(mbeoutput,species=as.character(NA)) {

if (nrow(mbeoutput$cov)>1) {
time <- space <- technical <- "all"
} else {
#stratification definition
time <- paste(mbeoutput$cov$year,mbeoutput$cov$seas,sep=" - ")
space <- mbeoutput$cov$area
technical <- mbeoutput$cov$gear
}

#mbeoutput's dimnames
dimnames(mbeoutput$totcatch.land) <- list(round(exp(mbeoutput$l.int)),mbeoutput$avec,1:(dim(mbeoutput$totcatch.land)[3]))
dimnames(mbeoutput$mean.lga.land) <- dimnames(mbeoutput$mean.wga.land) <- list(mbeoutput$avec,1:(dim(mbeoutput$totcatch.land)[3]))
if(!is.null(mbeoutput$totcatch.disc)){dimnames(mbeoutput$totcatch.disc) <-dimnames(mbeoutput$totcatch.land)
dimnames(mbeoutput$mean.lga.disc) <- dimnames(mbeoutput$mean.wga.disc) <-
  list(mbeoutput$avec,1:(dim(mbeoutput$totcatch.land)[3]))}
                 

#dbeOutput objects are created
dbeTotcatch.land <- dbeTotcatch.disc <- dbeMean.lga.land <- dbeMean.wga.land <- dbeMean.lga.disc <- dbeMean.wga.disc <-new("dbeOutput",species=species)

#'param' slot is filled
dbeTotcatch.land@param <- dbeTotcatch.disc@param <- "N"
dbeMean.lga.land@param <- dbeMean.lga.disc@param <- "length"
dbeMean.wga.land@param <- dbeMean.wga.disc@param <- "weight"

#'catchCat' slot is filled
dbeTotcatch.land@catchCat <- dbeMean.lga.land@catchCat <- dbeMean.wga.land@catchCat <- "LAN"
dbeTotcatch.disc@catchCat <- dbeMean.lga.disc@catchCat <- dbeMean.wga.disc@catchCat <- "DIS"

#totcatch.land
atLgth <- apply(mbeoutput$totcatch.land,c(1,3),sum,na.rm=TRUE)
atAge <- apply(mbeoutput$totcatch.land,2:3,sum,na.rm=TRUE)
expLgth <- expand.grid(dimnames(atLgth)) ; expAge <- expand.grid(dimnames(atAge))

dbeTotcatch.land@lenStruc$rep <- data.frame(cbind(time=time,space=space,technical=technical,length=as.character(expLgth[,1])),value=as.vector(atLgth),iter=expLgth[,2])
dbeTotcatch.land@ageStruc$rep <- data.frame(cbind(time=time,space=space,technical=technical,age=as.character(expAge[,1])),value=as.vector(atAge),iter=expAge[,2])
dbeTotcatch.land=fillTabFromRep(dbeTotcatch.land)

#totcatch.disc
dbeTotcatch.disc<-NULL
if(!is.null(mbeoutput$totcatch.disc)){
atLgth <- apply(mbeoutput$totcatch.disc,c(1,3),sum,na.rm=TRUE)
atAge <- apply(mbeoutput$totcatch.disc,2:3,sum,na.rm=TRUE)
expLgth <- expand.grid(dimnames(atLgth)) ; expAge <- expand.grid(dimnames(atAge))
dbeTotcatch.disc@lenStruc$rep <- data.frame(cbind(time=time,space=space,technical=technical,length=as.character(expLgth[,1])),value=as.vector(atLgth),iter=expLgth[,2])
dbeTotcatch.disc@ageStruc$rep <- data.frame(cbind(time=time,space=space,technical=technical,age=as.character(expAge[,1])),value=as.vector(atAge),iter=expAge[,2])
dbeTotcatch.disc<-fillTabFromRep(dbeTotcatch.disc)
}

#mean.lga.land
expAge <- expand.grid(dimnames(mbeoutput$mean.lga.land))
dbeMean.lga.land@ageStruc$rep <- data.frame(cbind(time=time,space=space,technical=technical,age=as.character(expAge[,1])),value=as.vector(mbeoutput$mean.lga.land),iter=expAge[,2])
dbeMean.lga.land=fillTabFromRep(dbeMean.lga.land)

#mean.wga.land
expAge <- expand.grid(dimnames(mbeoutput$mean.wga.land))
dbeMean.wga.land@ageStruc$rep <- data.frame(cbind(time=time,space=space,technical=technical,age=as.character(expAge[,1])),value=as.vector(mbeoutput$mean.wga.land),iter=expAge[,2])
dbeMean.wga.land=fillTabFromRep(dbeMean.wga.land)

#mean.lga.disc
dbeMean.lga.disc<-NULL
if(!is.null(mbeoutput$mean.lga.disc)){
expAge <- expand.grid(dimnames(mbeoutput$mean.lga.disc))
dbeMean.lga.disc@ageStruc$rep <- data.frame(cbind(time=time,space=space,technical=technical,age=as.character(expAge[,1])),value=as.vector(mbeoutput$mean.lga.disc),iter=expAge[,2])
dbeMean.lga.disc=fillTabFromRep(dbeMean.lga.disc)}

#mean.wga.disc
dbeMean.wga.disc<-NULL
if(!is.null(mbeoutput$mean.wga.disc)){
expAge <- expand.grid(dimnames(mbeoutput$mean.wga.disc))
dbeMean.wga.disc@ageStruc$rep <- data.frame(cbind(time=time,space=space,technical=technical,age=as.character(expAge[,1])),value=as.vector(mbeoutput$mean.wga.disc),iter=expAge[,2])
dbeMean.wga.disc=fillTabFromRep(dbeMean.wga.disc)}

return(list(dbeTotcatch.land=dbeTotcatch.land,dbeTotcatch.disc=dbeTotcatch.disc,dbeMean.lga.land=dbeMean.lga.land,
            dbeMean.wga.land=dbeMean.wga.land,dbeMean.lga.disc=dbeMean.lga.disc,dbeMean.wga.disc=dbeMean.wga.disc))
}

