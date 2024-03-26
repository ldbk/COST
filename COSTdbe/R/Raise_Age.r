
#Raising procedure of numbers at age for stratified simple random sampling (Marcel Machiels) 

setGeneric("N_at_Age_direct", function(dbeOutput,    #dbeOutput object
                               objectcs,             #csDataCons object
                               objectcl,             #clDataCons object
                               sex=as.character(NA),
                               ...){

  standardGeneric("N_at_Age_direct")

})


setMethod("N_at_Age_direct", signature(dbeOutput="dbeOutput",objectcs="csDataCons",objectcl="clDataCons"), function(dbeOutput,
                                                                                                                    objectcs,
                                                                                                                    objectcl,
                                                                                                                    sex=as.character(NA),
                                                                                                                    ...){
sp <- dbeOutput@species
ca <- ca(objectcs) ; cl <- cl(objectcl)
eval(parse('',text=paste("ca <- subset(ca,spp%in%",deparse(sp),")",sep=""))) 
eval(parse('',text=paste("cl <- subset(cl,taxon%in%",deparse(sp),")",sep=""))) 

if (!(all(is.na(sex)))) {ca <- ca[ca$sex%in%sex,]                                                            
                         if (nrow(ca)==0) stop("no CA data for specified sex in input object!!")            #                                                                               #
}                                                                                                           #


#subfunction to duplicate information from ca for each technical strata
dupTech <- function(mat,matTech) {
  ll <- dimnames(mat) ; ll[["technical"]] <- dimnames(matTech)[[3]]
  array(rep(as.vector(mat),dim(matTech)[3]),dim=c(dim(mat),dim(matTech)[3]),dimnames=ll)
}

# ********** CALCULATE WEIGHT PARAMETERS;

#total landed weights per strata
cl$landMult[is.na(cl$landMult)] <- 1
#TotLand = OffLand*Multi + UnallocCat + MisallocCat
totLand <- mapply(function(w,x,y,z) sum(c(w*x,y,z),na.rm=TRUE),cl$landWt,cl$landMult,cl$unallocCatchWt,cl$misRepCatchWt)
W.fix_C <- tapply(totLand*1000,list(time=cl$time,space=cl$space,technical=cl$technical),sum,na.rm=TRUE)      



w.bar_c <- tapply(ca$indWt,list(time=factor(ca$time,levels=dimnames(W.fix_C)[[1]]),
                                space=factor(ca$space,levels=dimnames(W.fix_C)[[2]])),mean,na.rm=TRUE)   
w.bar_c <- dupTech(w.bar_c,W.fix_C)

err <- tapply(ca$indWt,list(time=factor(ca$time,levels=dimnames(W.fix_C)[[1]]),
                            space=factor(ca$space,levels=dimnames(W.fix_C)[[2]])),sd,na.rm=TRUE)   
n <- tapply(ca$indWt,list(time=factor(ca$time,levels=dimnames(W.fix_C)[[1]]),
                          space=factor(ca$space,levels=dimnames(W.fix_C)[[2]])),length)
w.err_c <- err/sqrt(n)         
w.err_c <- dupTech(w.err_c,W.fix_C)



#********** CALCULATE FREQUENCY PARAMETERS;

factAge <- factor(ca$age,levels=seq(min(ca$age,na.rm=TRUE),max(ca$age,na.rm=TRUE),by=1))
Freq <- tapply(ca$indWt,list(age=factAge,
                             time=factor(ca$time,levels=dimnames(W.fix_C)[[1]]),
                             space=factor(ca$space,levels=dimnames(W.fix_C)[[2]])),length)
Freq[is.na(Freq)] <- 0                                                  # sampled number of fish per age group per stratum
Freq <- dupTech(Freq,W.fix_C)



Freqt <- tapply(ca$indWt,list(time=factor(ca$time,levels=dimnames(W.fix_C)[[1]]),
                              space=factor(ca$space,levels=dimnames(W.fix_C)[[2]])),length)
Freqt[is.na(Freqt)] <- 0                                                # total number of sampled fish per stratum
Freqt <- dupTech(Freqt,W.fix_C)


                 
#********** CALCULATE phat_kc and perr_kc and further RESULTS;

#phat_kc: the estimated age probability distribution per stratum
#perr_kc: the approximated standard error of the estimated phat_kc per stratum
#N_k:     the estimated number at age per stratum

phat_kc <- Freq/rep(Freqt,each=dim(Freq)[1]) #sweep(Freq,2:length(dim(Freq)),Freqt,"/")
perr_kc <- sqrt(phat_kc*(1-phat_kc)/rep(n,each=dim(phat_kc)[1])) #sqrt(sweep((sweep(Freq,2:length(dim(Freq)),Freqt,"/") * (1- sweep(Freq,2:length(dim(Freq)),Freqt,"/") )),2:length(dim(Freq)),Freqt,"/"))
N_k <- phat_kc*rep(W.fix_C/w.bar_c,each=dim(phat_kc)[1])  # round(sweep( phat_kc, 2:length(dim(Freq)),(W.fix_C)/w.bar_c , "*"))
D1_k <- phat_kc*rep(-W.fix_C/(w.bar_c^2),each=dim(phat_kc)[1])  # (-W.fix_C[]/(w.bar_c^2))*(phat_kc[])
D2_k <- W.fix_C/w.bar_c # <-W.fix_C[]/w.bar_c[]
dEEd <- (D1_k^2)*rep(w.err_c^2,each=dim(D1_k)[1]) + (perr_kc^2)*rep(D2_k^2,each=dim(perr_kc)[1])
#E_a <- sqrt(dEEd)

df.N_i <- cbind(expand.grid(dimnames(N_k)),value=as.vector(N_k))
df.VarN_i <- cbind(expand.grid(dimnames(dEEd)),value=as.vector(dEEd))

df.VarN_i <- df.VarN_i[!is.na(df.VarN_i$val),] ; df.N_i <- df.N_i[!is.na(df.N_i$val),] 
df.VarN_i <- df.VarN_i[df.VarN_i$val>0,] ; df.N_i <- df.N_i[df.N_i$val>0,]
  
  #N_i
df.N_i <- df.N_i[order(df.N_i$time,df.N_i$space,df.N_i$technical,df.N_i$age),] ; rownames(df.N_i) <- 1:nrow(df.N_i)
dbeOutput@ageStruc$estim <- df.N_i[,names(dbeOutput@ageStruc$estim)]
  
  #VarN_j
df.VarN_i <- df.VarN_i[order(df.VarN_i$time,df.VarN_i$space,df.VarN_i$technical,df.VarN_i$age),] ; rownames(df.VarN_i) <- 1:nrow(df.VarN_i)
dbeOutput@ageVar <- df.VarN_i[,names(dbeOutput@ageVar)]

#dbeOutput@ageVar<-ageVar
#-------------------------------------------------------------------------------
#result is returned
#-------------------------------------------------------------------------------

return(dbeOutput)

})

##################################################################
##################################################################
###################    Raise_Age method    #######################
##################################################################
##################################################################



Raise_Age <- function(csObject,dbeOutput,type="p",sex=as.character(NA)){#type= "p" or "fixedK", "propK" or "agesK" 

sp <- dbeOutput@species
ca <- ca(csObject)
#ca table is subset
ca <- ca[ca$spp%in%sp,]   
if (nrow(ca)==0) stop("no CA data for specified species in input object!!")

#number of age samples (PSUid/SSUid) is calculated at this stage (before subsetting on 'sex')               #
Unit <- paste(ca$PSUid,ca$SSUid,sep=":-:")                                                                  #
nSAMP <- spdAgreg(list(value=Unit),BY=list(time=ca$time,space=ca$space),function(x) length(unique(x)))      #
dbeOutput@nSamp$age <- nSAMP                                                                                #  ADDED : MM 02/04/2009
                                                                                                            #
if (!(all(is.na(sex)))) {ca <- ca[ca$sex%in%sex,]                                                            
                         if (nrow(ca)==0) stop("no CA data for specified sex in input object!!")            #                                                                               #
}                                                                                                           #

#number of fish measured in CA (virtual individuals excluded)
CAreal <- ca[ca$fishId>0,]                                                                              #
nMEAS <- spdAgreg(list(value=CAreal$age),BY=list(time=CAreal$time,space=CAreal$space),function(x) sum(!is.na(x)))       #     
dbeOutput@nMeas$age <- nMEAS                                                                                #

#numbers at length
Ldf <- dbeOutput@lenStruc$estim
N <- tapply(Ldf$value,list(length=Ldf$length,time=Ldf$time,space=Ldf$space,technical=Ldf$technical),sum,na.rm=TRUE)

#creating the stratified ALK with levels from N (duplication for technical strata)
ALK <- tapply(ca$age,list(length=factor(ca$lenCls,levels=dimnames(N)[[1]]),age=ca$age,
                          time=factor(ca$time,levels=dimnames(N)[[2]]),space=factor(ca$space,levels=dimnames(N)[[3]])),length)
ALK[is.na(ALK)] <- 0
  # --> duplication
ll <- dimnames(ALK) ; ll[["technical"]] <- dimnames(N)[[4]]
ALK <- array(rep(as.vector(ALK),dim(N)[4]),dim=c(dim(ALK),dim(N)[4]),dimnames=ll)

  if (all(is.na(dbeOutput@lenStruc$estim))) stop("estimates for length structure are missing in 'dbeOutput' object!!")
#  if (all(is.na(dbeOutput@lenVar))) stop("variance for length structure is missing in 'dbeOutput' object!!")

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
	if (type=="fixedK") {
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
if (!all(round(apply(Pi.hat,2:4,sum,na.rm=TRUE),6)==1)) warning("some length classes from 'dbeOutput@lenStruc' slot are not in 'ca' table")

#Estimates of total numbers at age
  #total numbers
D.hat <- apply(N,2:4,sum,na.rm=TRUE)
  #total numbers at age
D_i <- Pi.hat*rep(D.hat,each=dim(Pi.hat)[1])



########
## Variance calculation : depends on 'type' parameter
########

VarDj <- dbeOutput@lenVar

if (type%in%c("fixedK","propK","agesK")) {   #Kimura's method  --> VarPi and total numbers

  #Var.pi calculation
  #----------------
    if (type=="agesK") {
  	 a1 <- Pi.hat*(1-Pi.hat)
  	 VarPi <- a1/rep(ns,each=dim(a1)[1])
    } else {
      if (type=="fixedK") {
	      b1 <- apply(aperm(aperm(Qij*(1-Qij),c(1,3,4,5,2))*as.vector(lj*(1-lj)),c(1,5,2,3,4)),2:5,sum,na.rm=TRUE)
        b2 <- apply(aperm(aperm(Qij*(1-Qij),c(1,3,4,5,2))*as.vector(lj*lj),c(1,5,2,3,4)),2:5,sum,na.rm=TRUE)
        b3 <- apply(aperm(aperm(Qij*Qij,c(1,3,4,5,2))*as.vector(lj),c(1,5,2,3,4)),2:5,sum,na.rm=TRUE)-Pi.hat^2
        VarPi <- b1/rep(Nl*njStar,each=dim(b1)[1]) + b2/rep(njStar,each=dim(b2)[1]) + b3/rep(Nl,each=dim(b3)[1])
      } else {   #i.e if (type=="propK")
        c1 <- apply(aperm(aperm(Qij*(1-Qij),c(1,3,4,5,2))*as.vector(lj),c(1,5,2,3,4)),2:5,sum,na.rm=TRUE)
        c2 <- apply(aperm(aperm(Qij*Qij,c(1,3,4,5,2))*as.vector(lj),c(1,5,2,3,4)),2:5,sum,na.rm=TRUE)-Pi.hat^2
        VarPi <- c1/rep(ns,each=dim(c1)[1]) + c2/rep(Nl,each=dim(c2)[1])
  }}

  #Estimates of variance at age
  V1 <- VarPi*rep(D.hat*D.hat,each=dim(VarPi)[1])               #doesn't need VarDj information
  if (!all(is.na(VarDj))) {

    #Var(sum(D_j)) = sum(Var(D_j))
    VarD <- tapply(VarDj$value,list(time=factor(VarDj$time,levels=dimnames(N)[[2]]),space=factor(VarDj$space,levels=dimnames(N)[[3]]),
                                technical=factor(VarDj$technical,levels=dimnames(N)[[4]])),sum,na.rm=TRUE)

    V2 <- Pi.hat*Pi.hat*rep(VarD,each=dim(Pi.hat)[1])
    V3 <- VarPi*rep(VarD,each=dim(VarPi)[1])
    
  } else {
  
    V2 <- V3 <- V1}

VarD_i <- V1+V2+V3

} else {                #default method : VarQij  nd total numbers at length 

  VarQij <- aperm(Qij*(1-Qij),c(1,3,4,5,2))/as.vector(nj)
  #Estimates of variance at age
  V1 <- aperm(VarQij*as.vector(N*N),c(1,5,2,3,4))               #doesn't need VarDj information

  if (!all(is.na(VarDj))) {

   VarNj <- tapply(VarDj$value,list(length=VarDj$length,time=VarDj$time,space=VarDj$space,technical=VarDj$technical),sum,na.rm=TRUE)
   V2 <- aperm(aperm(Qij*Qij,c(1,3,4,5,2))*as.vector(VarNj),c(1,5,2,3,4))
   V3 <- aperm(VarQij*as.vector(VarNj),c(1,5,2,3,4))
   
  } else {
  
   V2 <- V3 <- V1}
   
VarD_i <- apply(V1+V2+V3,2:5,sum,na.rm=TRUE)

}




#results are inserted in dbeOutput object (in 'ageVar' slot only if 'lenVar' slot is filled 
#####################
  #D_i & VarD_i
df.D_i <- cbind(expand.grid(dimnames(D_i)),value=as.vector(D_i))
df.VarD_i <- cbind(expand.grid(dimnames(VarD_i)),value=as.vector(VarD_i))

df.VarD_i <- df.VarD_i[!is.na(df.D_i$val),] ; df.D_i <- df.D_i[!is.na(df.D_i$val),] 
df.VarD_i <- df.VarD_i[df.D_i$val>0,] ; df.D_i <- df.D_i[df.D_i$val>0,]
  
  #D_i
df.D_i <- df.D_i[order(df.D_i$time,df.D_i$space,df.D_i$technical,df.D_i$age),] ; rownames(df.D_i) <- 1:nrow(df.D_i)
dbeOutput@ageStruc$estim <- df.D_i[,names(dbeOutput@ageStruc$estim)]
  
  #VarD_j
df.VarD_i <- df.VarD_i[order(df.VarD_i$time,df.VarD_i$space,df.VarD_i$technical,df.VarD_i$age),] ; rownames(df.VarD_i) <- 1:nrow(df.VarD_i)

if (!all(is.na(VarDj))) dbeOutput@ageVar <- df.VarD_i[,names(dbeOutput@ageVar)]

return(dbeOutput)

}



###################
# Exported method #
###################



setGeneric("RaiseAge", function(dbeOutput,
                                 csObject,
                                 clObject,              #only used if type="direct" (stratified simple random sampling) 
                                 ...){
	standardGeneric("RaiseAge")}
)



setMethod("RaiseAge", signature(dbeOutput="dbeOutput",csObject="csDataCons",clObject="missing"), function(dbeOutput,
                                                                                                          csObject,
                                                                                                          type="p",  #type= "p" or "fixedK", "propK" or "agesK" or "direct"
                                                                                                          sex=as.character(NA),
                                                                                                          incl.precision=TRUE,   ## added MM 26/07/2010
                                                                                                          probs=c(0.025,0.975),
                                                                                                          ...){
if (type=="direct") stop("'clObject' parameter is missing!!") 

if (incl.precision) {

  obj <- Raise_Age(csObject,dbeOutput,type=type,sex=sex)
  
  if (!all(is.na(obj@ageStruc$estim)) & !all(is.na(obj@ageVar))) {
    obj <- dbeCalc(obj,type="CV",vrbl="a",replicates=FALSE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="a",probs=probs,replicates=FALSE,update=TRUE)
  }
  
  return(obj)

} else {
                                                                                                         
  Raise_Age(csObject,dbeOutput,type=type,sex=sex)

}

})


setMethod("RaiseAge", signature(dbeOutput="dbeOutput",csObject="csDataCons",clObject="clDataCons"), function(dbeOutput,
                                                                                                             csObject,
                                                                                                             clObject,      #only used if type="direct" (stratified simple random sampling)
                                                                                                             type="p",  #type= "p" or "fixedK", "propK" or "agesK" or "direct"
                                                                                                             sex=as.character(NA),
                                                                                                             incl.precision=TRUE,   ## added MM 26/07/2010
                                                                                                             probs=c(0.025,0.975),
                                                                                                             ...){
if (incl.precision) {

  if (type=="direct") {
    obj <- N_at_Age_direct(dbeOutput,csObject,clObject,sex=sex)
  } else {                                                                                                             
    obj <- Raise_Age(csObject,dbeOutput,type=type,sex=sex)
  }
  
  if (!all(is.na(obj@ageStruc$estim)) & !all(is.na(obj@ageVar))) {
    obj <- dbeCalc(obj,type="CV",vrbl="a",replicates=FALSE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="a",probs=probs,replicates=FALSE,update=TRUE)
  }
  
  return(obj)

} else {                                                                                                               

  if (type=="direct") {
    N_at_Age_direct(dbeOutput,csObject,clObject,sex=sex)
  } else {                                                                                                             
    Raise_Age(csObject,dbeOutput,type=type,sex=sex)
  }

}

})





#obj1 <- RaiseAge(dbeOutput,csObject,type="p")
#obj2 <- RaiseAge(dbeOutput,csObject,type="fixedK")
#obj3 <- RaiseAge(dbeOutput,csObject,type="propK")
#obj4 <- RaiseAge(dbeOutput,csObject,type="agesK")
#obj5 <- RaiseAge(dbeOutput,csObject,type="direct")
#obj6 <- RaiseAge(dbeOutput,csObject,clObject,type="p")
#obj7 <- RaiseAge(dbeOutput,csObject,clObject,type="fixedK")
#obj8 <- RaiseAge(dbeOutput,csObject,clObject,type="propK")
#obj9 <- RaiseAge(dbeOutput,csObject,clObject,type="agesK")
#obj10 <- RaiseAge(dbeOutput,csObject,clObject,type="direct")
#





