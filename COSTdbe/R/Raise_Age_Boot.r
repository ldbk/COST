
## FUNCTION STILL IN DEVELOPMENT,
#need to check iters that don't give a particular age-length combination are assigned 0 for calculating mean and var across iter

#library(COSTcore) #1.3-3
#library(COSTdbe)  #0.1-9

#spdAgreg <- COSTcore:::spdAgreg
#resample <- COSTdbe:::resample
#As.num <- COSTcore:::As.num

#data(sole)
#strD <- strIni(timeStrata="quarter",techStrata="commCat")
#csObject <- csDataCons(csDataVal(subset(sole.cs,sampType%in%c("M","V"))),strD)
#clObject <- clDataCons(clDataVal(sole.cl),strD)
#dbeOutput <- dbeObject(species="Solea solea",catchCat="LAN",strataDesc=strD)
#sex <- as.character(NA)
#
#sol.dbe.an <- RaiseLgth (dbeOutput = dbeOutput, csObject = csObject, clObject = clObject)
#sol.dbe.boot <- RaiseLgthBoot (dbeOutput = dbeOutput, csObject = csObject, clObject = clObject, B=10)

#sol.dbe.an <- RaiseAge (csObject = csObject, dbeOutput = sol.dbe.an, type="fixed")
#set.seed(113)
#sol.dbe.boot1 <- RaiseAgeBoot (csObject = csObject, dbeOutput = sol.dbe.boot, type="fixed", bootMethod="samples")
#set.seed(113)
#sol.dbe.boot2 <- RaiseAgeBoot (csObject = csObject, dbeOutput = sol.dbe.boot, type="fixed", bootMethod="otoliths")
# comparison
#merge (sol.dbe.boot1@ageStruc$estim, sol.dbe.boot2@ageStruc$estim, by=c("time", "space", "technical", "age"), suffixes=c(".sam",".oto"), all=T)


################################################################################
################################################################################
################################################################################
################################################################################



Raise_Age_Boot <- function(csObject,dbeOutput,type="p",sex=as.character(NA), bootMethod = "samples", fillGaps=FALSE, p=10, trace=FALSE){
#type= "fixed" or "prop" or "ages"
#bootMethod="samples" or "otoliths"

if (!bootMethod %in% c("samples","otoliths")) stop ('bootMethod must be "samples" or "otoliths"')

sp <- dbeOutput@species
ca <- ca(csObject)
#ca table is subset
ca <- ca[ca$spp%in%sp,]   
if (nrow(ca)==0) stop("no CA data for specified species in input object!!")

#number of age samples (PSUid/SSUid) is calculated at this stage (before subsetting on 'sex')               #
Unit <- paste(ca$PSUid,ca$SSUid,sep=":-:")                                                                  #
nSAMP <- spdAgreg(list(value=Unit),BY=list(time=ca$time,space=ca$space),function(x) length(unique(x)))      #
dbeOutput@nSamp$age <- nSAMP                                                                                #  ADDED : MM 02/04/2009

if (!(all(is.na(sex)))) {ca <- ca[ca$sex%in%sex,]
                         if (nrow(ca)==0) stop("no CA data for specified sex in input object!!")            #                                                                               #
}                                                                                                           #

#number of fish measured in HL                                                                              #
nMEAS <- spdAgreg(list(value=ca$age),BY=list(time=ca$time,space=ca$space),function(x) sum(!is.na(x)))       #     
dbeOutput@nMeas$age <- nMEAS                                                                                #

#numbers at length all replicates
if (all(is.na(dbeOutput@lenStruc$rep))) stop("estimates for length structure are missing in 'dbeOutput' object!!")

# Change desc slot to bootstrap samples or otoliths
if (dbeOutput@methodDesc != paste("bootstrap", bootMethod)) {
    print(paste ("dbeOutput object methodDesc slot has been changed from ", dbeOutput@methodDesc, " to bootstrap ", bootMethod, sep=""))
    dbeOutput@methodDesc <- paste("bootstrap", bootMethod)
    }

Ldfall <- dbeOutput@lenStruc$rep
# set number of age iterations to match number used for length distribution
B <- length(unique(Ldfall$iter[Ldfall$iter != 0]))

CA.orig <- ca

if (bootMethod == "samples") {
  CA.orig$Unit <- paste(CA.orig$PSUid,CA.orig$SSUid,sep=":-:")

  # PSUids for each strata combination
  CASTR <- paste(ca$time,ca$space,sep=":-:")
  UnitSTR = data.frame (Unit, CASTR, stringsAsFactors = F)

  ageids <- unique( UnitSTR )
  ageids <- ageids[order(ageids$CASTR),] # ageids and nSAMP should now have STR in same order
  uStrat <- paste (nSAMP$time,nSAMP$space,sep=":-:")
  #identify start and end positions for each strata
  start.pos = c(1, (cumsum(nSAMP$value)[-length(nSAMP$value)]) +1 )
  end.pos = cumsum(nSAMP$value)

  # sample new set of PSUid for each strata combination - stratified bootstrap resampling - may be able to use boot function instead
  bootAgeid = matrix (NA, nrow = dim(ageids)[1], ncol=B+1)
  dimnames(bootAgeid) = list(NULL, c("orig", paste("iter.",1:B,sep="")))
  # original sample ids in first column
  bootAgeid[,1] = ageids$Unit

  # put resampled ids for each strata into vector in relevant places,
  # assigning to all columns using size = nSAMP$value[i] * B, instead of using loop by iteration 1 to B
  # order of nSAMP$value and uStrat needs to match
  for ( i in 1:length(uStrat) ){
    bootAgeid [ start.pos[i]:end.pos[i], -1] = resample (ageids$Unit [ ageids$CASTR == uStrat[i] ], size = nSAMP$value[i] * B, replace=T )
  }
} else {
  # i.e. if bootMethod is otoliths
  #numbers at length, original data
  Ldf <- Ldfall[Ldfall$iter == 0,]
  N <- tapply(Ldf$value,list(length=Ldf$length,time=Ldf$time,space=Ldf$space,technical=Ldf$technical),sum,na.rm=TRUE)

  #creating the stratified ALK with levels from N
  ALK.orig <- tapply(CA.orig$age,list(length=factor(CA.orig$lenCls,levels=dimnames(N)[[1]]),age=CA.orig$age,
                          time=factor(CA.orig$time,levels=dimnames(N)[[2]]),space=factor(CA.orig$space,levels=dimnames(N)[[3]])),length)
  ALK.orig[is.na(ALK.orig)] <- 0

  resampleALKrow <- function(x) { sumx = sum(x)
                        if(sumx > 1) rmultinom(1,size=sumx, prob=x) else x  }
}

####### START OF BOOTSTRAP LOOP ################

ac.list = vector("list", B+1)
print ("iter")
# i=1 uses original data, its output is labelled iter=0
# printing iteration number every 50 iterations
for (i in 1:(B+1) ){
if(identical ((i-1)/50, (i-1)%/%50))print(i-1)

#numbers at length
Ldf <- Ldfall[Ldfall$iter == (i-1),]
N <- tapply(Ldf$value,list(length=Ldf$length,time=Ldf$time,space=Ldf$space,technical=Ldf$technical),sum,na.rm=TRUE)

if (bootMethod == "samples") {
  ca = merge(data.frame(Unit = bootAgeid[,i]), CA.orig, by="Unit")
  
  if (fillGaps) {                                                                                                                  #
    cs_cons_temp <- new("csDataCons",tr=csObject@tr,hh=csObject@hh,sl=csObject@sl,hl=csObject@hl,ca=ca[,-match("Unit",names(ca))]) # MM 18/11/2011
    cs_cons_temp <- fillALKmult(cs_cons_temp,spp=sp,p=p,trace=trace)                                                               #
    ca <- ca(cs_cons_temp)                                                                                                         #
  }                                                                                                                                #
  
  ALK <- tapply(ca$age,list(length=factor(ca$lenCls,levels=dimnames(N)[[1]]),age=ca$age,
                          time=factor(ca$time,levels=dimnames(N)[[2]]),space=factor(ca$space,levels=dimnames(N)[[3]])),length)
  ALK[is.na(ALK)] <- 0
} else {
  #creating the stratified ALK with levels from N (duplication for technical strata)
  ALK <- aperm (apply (ALK.orig, c(1,3,4), resampleALKrow), perm=c(2,1,3,4))
  dimnames(ALK) <- dimnames(ALK.orig)
  }


## THIS SECTION IS COPIED FROM VesselRaiseBoot as a reminder to do something about gaps in alks
# to use it will need to setup alkgaps.list and alkgaps.counter and change alk to ALK
## Fill in missing rows of alk  (Will be quicker to only run this if there are gaps that need filling)
# (all gaps of length <= value are filled with the sum of surrounding filled classes)

# need to have functions in alkGaps.r available
# Disadvantage is that it creates 'virtual' otoliths

#gaps.out = gapsRm(alk,type="fillMiss",value=2,preview=FALSE,postview=FALSE)
#alk = gaps.out$alk
# records with otoliths added
#if ( dim(gaps.out$addIndTab)[1]>0 ){
#alkgaps.list[[i]] = cbind(gaps.out$addIndTab, iter=(i-1))
#alkgaps.counter = alkgaps.counter + 1 }


  # --> duplication
ll <- dimnames(ALK) ; ll[["technical"]] <- dimnames(N)[[4]]
ALK <- array(rep(as.vector(ALK),dim(N)[4]),dim=c(dim(ALK),dim(N)[4]),dimnames=ll)

#nj : number of sampled individuals of length j
  nj <- tapply(ca$lenCls,list(length=factor(ca$lenCls,levels=dimnames(N)[[1]]),time=factor(ca$time,levels=dimnames(N)[[2]]),
                              space=factor(ca$space,levels=dimnames(N)[[3]])),function(x) sum(!is.na(x)))
  nj[is.na(nj)] <- 0
  # --> duplication
  ll2 <- dimnames(nj) ; ll2[["technical"]] <- dimnames(N)[[4]]
  nj <- array(rep(as.vector(nj),dim(N)[4]),dim=c(dim(nj),dim(N)[4]),dimnames=ll2)

#Nl : number of length-sampled individuals
	Nl <- apply(nj,2:4,sum)
#ns : number of samples to be aged
	ns <- apply(ALK,3:5,sum)
#nl : number of sampled length categories
  nl <- apply(nj,2:4,function(x) sum(x>0))
#Q'ij
	Qij <- aperm(aperm(ALK,c(1,3,4,5,2))/as.vector(apply(ALK,c(1,3:5),sum)),c(1,5,2:4))
#njStar
	if (type=="p") {
    njStar <- ns/nl
  } else {
    njStar <- nj*rep(as.vector(ns/Nl),each=dim(nj)[1])}
#lj : proportion of length j in the population
	lj <- N/rep(as.vector(apply(N,2:4,sum,na.rm=TRUE)),each=dim(N)[1])
  lj[is.na(lj)] <- 0

#'pi' calculation
#------------
Pi.hat <- apply(aperm(aperm(Qij,c(1,3,4,5,2))*as.vector(lj),c(1,5,2,3,4)),2:5,sum,na.rm=TRUE)
#test on Pi.hat (missing length class in ca)
#if (!all(apply(Pi.hat,2:4,sum,na.rm=TRUE)==1)) warning("it seems that some length classes from 'dbeOutput@lenStruc' slot are not in 'ca' table")


#Estimates of total numbers at age
  #total numbers
D.hat <- apply(N,2:4,sum,na.rm=TRUE)
  #total numbers at age
D_i <- Pi.hat*rep(D.hat,each=dim(Pi.hat)[1])


#results are inserted in dbeOutput object
#####################
  #D_i
df.D_i <- cbind(expand.grid(dimnames(D_i)),value=as.vector(D_i))

df.D_i <- df.D_i[order(df.D_i$time,df.D_i$space,df.D_i$technical,df.D_i$age),] ; rownames(df.D_i) <- 1:nrow(df.D_i)
ac.list[[i]] <- df.D_i[,names(dbeOutput@ageStruc$estim)] # changed for boot

} # End of bootstrap loop
print(i-1)
print("iterations complete")

# Age structure
#convert list of length distributions into data.frame matching dbeOutput format
rep.iter = unlist (lapply(ac.list, FUN = function(x) {dim(x)[1]}) )

ac.df = dbeOutput@ageStruc$rep = data.frame  (time =  unlist(lapply(ac.list, FUN = function(x) {x[,"time"]})),
                                space =  unlist(lapply(ac.list, FUN = function(x) {x[,"space"]})),
                                technical = unlist(lapply(ac.list, FUN = function(x) {x[,"technical"]})),
                                age =  unlist(lapply(ac.list, FUN = function(x) {x[,"age"]})),
                                value =  unlist(lapply(ac.list, FUN = function(x) {x[,"value"]})),
                                iter =  rep(0:B, times = rep.iter)   )

ac.df = ac.df[ac.df$iter > 0,]
ac.mean = spdAgreg (list (value=ac.df$value), BY = list(time=ac.df$time, space=ac.df$space, technical=ac.df$technical, age=ac.df$age), mean)
ac.mean$age = As.num(ac.mean$age)
ac.mean = ac.mean [order(ac.mean$time, ac.mean$space, ac.mean$technical, ac.mean$age),]
dimnames(ac.mean)[[1]] = 1:(dim(ac.mean)[1])

ac.var = spdAgreg (list (value=ac.df$value), BY = list(time=ac.df$time, space=ac.df$space, technical=ac.df$technical, age=ac.df$age), var)
ac.var$age = As.num(ac.var$age)
ac.var = ac.var [order(ac.var$time, ac.var$space, ac.var$technical, ac.var$age),]
dimnames(ac.var)[[1]] = 1:(dim(ac.var)[1])

dbeOutput@ageStruc$estim = ac.mean
dbeOutput@ageVar = ac.var

return(dbeOutput)

}



###################
# Exported method #
###################



setGeneric("RaiseAgeBoot", function(dbeOutput,
                                 csObject,
                                 type="p",
                                 sex=as.character(NA),
                                 bootMethod = "samples",
                                 incl.precision=TRUE,
                                 probs=c(0.025,0.975),
                                 fillGaps=FALSE,
                                 p=10,
                                 trace=FALSE,
                                 ...){
	standardGeneric("RaiseAgeBoot")}
)


setMethod("RaiseAgeBoot", signature(dbeOutput="dbeOutput",csObject="csDataCons"), function(dbeOutput,
                                                                                       csObject,
                                                                                       type="p",
                                                                                       sex=as.character(NA),
                                                                                       bootMethod = "samples",
                                                                                       incl.precision=TRUE,
                                                                                       probs=c(0.025,0.975),
                                                                                       fillGaps=FALSE,
                                                                                       p=10,
                                                                                       trace=FALSE,
                                                                                       ...){

if (incl.precision) {                                                                                             
                                                                                                     
  obj <- Raise_Age_Boot(csObject=csObject,dbeOutput=dbeOutput,type=type,sex=sex, bootMethod=bootMethod, fillGaps=fillGaps, p=p, trace=trace)
  
  if (!all(is.na(obj@ageStruc$rep))) {
    obj <- dbeCalc(obj,type="CV",vrbl="a",replicates=TRUE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="a",probs=probs,replicates=TRUE,update=TRUE)
  }
  
  return(obj)

} else {

  Raise_Age_Boot(csObject=csObject,dbeOutput=dbeOutput,type=type,sex=sex, bootMethod=bootMethod, fillGaps=fillGaps, p=p, trace=trace)

}
})





