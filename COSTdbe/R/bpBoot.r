#===============================================================================
# Name           : bpBoot
# Author         : Paz Sampedro (IEO, SPAIN)
# Date (dd/mm/yy): 12/12/2008 10:42:06
# Version        : v0.5 
#
# Aim            : Bootstrap estimates of empirical weight-at-length/age,        
#                  maturity-at-length/age, sex-ratio-at-length/age and their     
#                  associated variances      
#===============================================================================

# Load library     
#library(boot)

# Create generic function "bpBoot"
setGeneric("bpBoot", function(dbeOutput, # dbeOutput object
                              object,    # csDatacons object
                              mat.scale=list(immature=c(0,1),mature=c(2:8)), # Codes of maturity scale
                                                                             #  for imature and mature
                              sample.boot=FALSE, # If FALSE, resampling unit= individual fish
                                                 #    TRUE, resampling unit=sample 
                              nboot=1000, # Number of bootstrap replicates
                             ...){
     standardGeneric("bpBoot")
     },package="COSTdbe")

# Create and save method from the generic function "bpBoot"
setMethod("bpBoot", signature(dbeOutput="dbeOutput", object="csDataCons"),function(dbeOutput,
                                                                  object,
                                                                  mat.scale=list(immature=c(0,1),mature=c(2:8)),
                                                                  sample.boot=FALSE,
                                                                  nboot=1000,
                                                                  ...){
#! Select and format data 
As.num <- function(x) as.numeric(as.character(x))
species <- dbeOutput@species
if (all(is.na(species))) stop("no species in 'dbeOutput' object!!")  
if ("all"%in%species) species <- unique(as.character(c(object@ca$spp,object@hl$spp)))

# Restriction of ca and hl table to the specified species
ca <- object@ca[object@ca$spp%in%species,]
hl <- object@hl[object@hl$spp%in%species,]
if (nrow(ca)==0) stop("no biological parameters for this species in consolidated object!!")
if (nrow(hl)==0) stop("no hl information!!")
  
# Stratification levels. Levels without data are removed
timeStrata <- spaceStrata <- techStrata <- TRUE
if (all(is.na(ca$time))) {timeStrata <- FALSE ; ca$time <- factor("all")}
if (all(is.na(ca$space))) {spaceStrata <- FALSE ; ca$space <- factor("all")}
if (all(is.na(hl$technical))) {techStrata <- FALSE ; hl$technical <- factor("all")}

# Add technical stratification in a for the homogeneity of dimensions, ca is duplicated for each hl$technical field
CA <- do.call("rbind",lapply( levels(hl$technical),function(x) {df <- ca ; df$technical <- x ; return(df)}))
if (!is.null(CA)) ca <- CA                          
 
# Recode specified biological data. New column is created: "bio"
type <- dbeOutput@param

# Recode user maturity scale to 0 for immatures and 1 for matures
recode <- function (data,immature, mature) 
 {
  data <- as.character (data)
  immature <- as.character(immature)
  mature <- as.character(mature)
  data[data%in%immature] <- 0
  data[data%in%mature] <- 1    
  data
 }

bio <- switch(type,
            # Split maturity stage in immature (value=0) and mature (value=1)
              # User defines immature and mature in mat.scale argument
            maturity =  as.numeric(recode(ca$matStage, mat.scale$immature, mat.scale$mature)),
            # Rename Females=1 and Males=0 (sexratio=F/F+M)
            sex = As.num(factor(ca$sex,levels=c("F","M"),labels=c("1","0"))),
            weight = As.num(ca$indWt),
            length = As.num(ca$lenCls)         #MM 27/07/2010
            )
                        
if (is.null(bio)) stop(paste ("no" ,type,"data in", deparse(substitute(object))))

# Add the column "bio" to ca
ca$bio <- bio

# Creation of data.frame with numbers of measured fish by length class and strata
# numbers of measured fish per crossed strata (length-time-space-technical)
NL <- tapply(hl$lenNum,list(paste(hl$lenCls,hl$time,hl$space,hl$technical,sep=":")),sum,na.rm=TRUE)
# and we build the data.frame from NL
df <- as.data.frame(t(do.call("cbind",lapply(names(NL),function(x) strsplit(x,":")[[1]]))))
df$nL <- NL ; names(df) <- c("lenCls","time","space","technical","nL")
ca <- merge(ca,df,all.x=TRUE) ; ca$nL[is.na(ca$nL)] <- 0
ca <- as.data.frame (ca)

# Number of samples in ca
samples.bio <- aggregate(ca$trpCode, list(ca$space,ca$time,ca$technical,ca$trpCode),length)
samples.bio <- tapply(samples.bio$Group.4,list(samples.bio$Group.2, samples.bio$Group.1,samples.bio$Group.3),length)
time <- rep(rownames(samples.bio),ncol(samples.bio)*dim(samples.bio)[3])
space <- rep(colnames(samples.bio),nrow(samples.bio)*dim(samples.bio)[3])
technical <- rep (unique(ca$technical),ncol(samples.bio)*nrow(samples.bio) )
samples.bio <- data.frame(time,sort(space),technical,as.vector(samples.bio))
names (samples.bio) <- c("time", "space", "technical", "value")
samples.bio <- samples.bio[!is.na(samples.bio$value),] 
dbeOutput@nSamp$len <- data.frame(time=samples.bio$time,space=samples.bio$space,technical=samples.bio$technical,value=samples.bio$value)            ######### MM 03/04/2009

# Number of bio data in ca
n.bio <- tapply (ca$bio, list(paste(ca$time, ca$space,ca$technical, sep=":")), length)
n.bio2 <- as.data.frame(t(do.call("cbind",lapply(names(n.bio),function(x) strsplit(x,":")[[1]]))))
n.bio2$value <- n.bio
names (n.bio2) <- c("time", "space", "technical", "value")
number.bio <- n.bio2[order(n.bio2$space,n.bio2$time),]
dbeOutput@nMeas$len <- data.frame(time=number.bio$time,space=number.bio$space,technical=number.bio$technical,value=number.bio$value)                ######### MM 03/04/2009
rownames(dbeOutput@nMeas$len) <- NULL                                                                                                               ######### MM 03/04/2009
#! Bootstrap function

bio.fun <- function(data,i,ind=NULL) 
  { 
  # Resampled data 
  if (is.null(ind)) # Resampling unit=individual fish
  { 
    # Strata by length 
    len.strata <- as.data.frame(sort(unique(paste(data$lenCls,data$time,data$space,data$technical, sep=":"))))
    names(len.strata) <- "strata"
    # Select resampled rows
    ca <- data[i,]
  }
  else  # Resampling unit=sample
  {
    # Strata by length 
    len.strata <- as.data.frame(sort(unique(paste(ind$lenCls,ind$time,ind$space,ind$technical, sep=":"))))
    names(len.strata) <- "strata"
    # Select resampled "samples"
    data <- data [i,]
    # Extract individuals of resampled "samples"
    j2 <- c()
 
    for(i in data$trp)
    {  
      j <- as.numeric(rownames(ind[ind$trpCode==i,]))#data$trp[i],]))   modif MM 28/02/2011
      j2<- c(j2,j)
      j2
    }
    ca <- ind[j2,]
 	}
  # Creation of data.frame with numbers sub-sampled by length class and strata
  # numbers of measured fish per crossed strata
  ML <- tapply(1:nrow(ca),list(paste(ca$lenCls,ca$time,ca$space,ca$technical,sep=":")),length)
  # and we build the data.frame from ML
  dF <- as.data.frame(t(do.call("cbind",lapply(names(ML),function(x) strsplit(x,":")[[1]]))))
  dF$mL <- ML ; names(dF) <- c("lenCls","time","space","technical","mL")
  # dF data are merged to ca
  ca <- merge(ca,dF,all.x=TRUE)

  # Calculation of mean, var at length, and adjusted mean & var at age (stratified)
  MeanAtL <- tapply(ca$bio,list(paste(ca$lenCls,ca$time,ca$space,ca$technical,sep=":")),mean,na.rm=T) 
  MeanAtL <- data.frame(names(MeanAtL),MeanAtL)
  names(MeanAtL) <- c("strata", "value")   
  MeanAtL <- merge(len.strata,MeanAtL,all.x=T)
  if (is.null(ind)) ind <- data
  age <- matrix(sort(unique (ind$age)))
  if (nrow(age)==0) 
  {
   results <- as.vector(As.num(MeanAtL[,2]))
   names (results) <- as.vector (paste ("lenCls", MeanAtL[,1],sep=":"))      
  } 
  else #! Calculations by age
  {      
    # Strata by age in ca
    ind <- ind[!is.na(ind$age),]
    age.strata <- as.data.frame(sort(unique(paste(ind$age,ind$time,ind$space,ind$technical, sep=":"))))
    names(age.strata) <- "strata"   
    ca <- ca[!is.na(ca$age),] # Delete rows without age data
    # Creation of data.frame with numbers sub-sampled by age and strata
    # numbers of measured fish per crossed strata
    MA <- tapply(1:nrow(ca),list(paste(ca$age,ca$time,ca$space,ca$technical, sep=":")),length)
    # and we build the data.frame from MA
    DF <- as.data.frame(t(do.call("cbind",lapply(names(MA),function(x) strsplit(x,":")[[1]]))))
    DF$mA <- MA ; names(DF) <- c("age","time","space","technical","mA")
    # DF data are merged to ca
    ca <- merge(ca,DF,all.x=TRUE)
    # Insertion of r field (r=nL/mL)
    ca$r <- ca$nL/ca$mL                                      
    # Sum r of A is put in ca
    rSumA <- tapply(ca$r,list(paste(ca$age,ca$time,ca$space,ca$technical,sep=":")),sum,na.rm=TRUE)
    # and we build the data.frame from rSumA
    DFA <- as.data.frame(t(do.call("cbind",lapply(names(rSumA),function(x) strsplit(x,":")[[1]]))))
    DFA$rSumA <- rSumA ; names(DFA) <- c("age","time","space","technical","rSumA")
    # and merging is operated
    ca <- merge(ca,DFA,all.x=TRUE)
    # Calculation of statistical weights w
    ca$w <- ca$mA*ca$r/ca$rSumA
    # Results
    # Mean at age if adjust to hl=FALSE
    # MeanAtA <- tapply(ca$bio,list(paste(ca$age,ca$time,ca$space,ca$technical,sep=":")),mean,na.rm=TRUE)
    # Mean at age if adjust to hl=TRUE
    MeanAtA <- tapply(ca$w*ca$bio,list(paste(ca$age,ca$time,ca$space,ca$technical,sep=":")),sum,na.rm=TRUE)/tapply(ca$w,list(paste(ca$age,ca$time,ca$space,ca$technical,sep=":")),sum,na.rm=TRUE)
    MeanAtA <- data.frame(names(MeanAtA),MeanAtA)
    names(MeanAtA) <- c("strata", "value")    
    MeanAtA <- merge (age.strata,MeanAtA,all.x=T)
    results <- as.vector(c(As.num(MeanAtL[,2]),As.num(MeanAtA[,2])))
    names (results) <- c(as.vector (paste ("lenCls", MeanAtL[,1], sep=":")),as.vector(paste("age",MeanAtA[,1],sep=":")))
  }
  results            
 }

# Age data for this stock ?
age <- matrix(sort(unique(ca$age)))
if (nrow(age)==0) {print (paste ("No AGE data in", deparse(substitute(object)))) }

#! Apply Bootstrap

if (sample.boot==FALSE) # Resampling unit= individual fish
 {
 ca$bootstr <- as.factor(paste(ca$lenCls,ca$time,ca$space,ca$technical,sep=":"))
 l <- length(levels(ca$bootstr))
 levels (ca$bootstr) <- 1:l
 bio.boot <- boot(ca,bio.fun,strata=ca$bootstr,R=nboot)
 } else  # Resampling unit= sample
 {

 # Define stratification variable: time, space and technical
 ca$bootstr <- as.factor(paste(ca$time,ca$space,ca$technical,sep=":"))
 # Trips to resample with stratification variable 
 trpCode <- data.frame(trp=ca$trpCode,bootstr=ca$bootstr)
 trp <- unique(trpCode)
 trp <- trp[order(as.numeric(as.character(trp$trp))),]
 bio.boot <- boot(trp,bio.fun,strata=as.factor(trp$bootstr),R=nboot,ind=ca)
 }
  
#! Format bootstrap results

# Add names to boot replicates
colnames(bio.boot$t) <- names(bio.boot$t0)

# Bootstrap variance estimates 

# Function var for R-2.8.0
# bio.boot$t <- bio.boot$t [,!apply(is.na(bio.boot$t),2,all)]
var.boot <- apply(bio.boot$t,2,var,use="pairwise.complete.obs")
# Variance values NA are transformed to 0
var.boot[is.na(var.boot)] <- 0 
# Data.frame with original estimates and bootstrap variance
results <- as.data.frame (cbind(estimate=bio.boot$t0,variance=var.boot))
# Split by strata
res <- as.data.frame(t(do.call("cbind",lapply(rownames(results),function(x) strsplit(x,":")[[1]]))))
names(res) <- c("class", "value", "time", "space", "technical")
# nboot estimates
iter2 <- data.frame()
iter <- data.frame()
for (i in 1:nboot)
{
 iter2 <- cbind (res, as.vector(bio.boot$t[i,]), as.vector(rep(i,length(as.vector(bio.boot$t[i,])))))
 iter <- rbind (iter,iter2)
 }
names (iter) <- c(names(res), "value.iter", "iter")
iter <- iter[!is.na(iter$value.iter),]
res$estimate <- bio.boot$t0
res$variance  <- var.boot
res$class <- as.vector(as.character(res$class))
res$value <- as.numeric(as.character(res$value))
# Select results by length
res.length <- res[res$class=="lenCls",]
iter.length <- iter[iter$class=="lenCls",]
# Order by technical, space, time and length
res.length <- with(res.length,res.length[order(res.length$technical,res.length$space,res.length$time,res.length$value),])
iter.length <- with(iter.length,iter.length[order(iter.length$technical,iter.length$space,iter.length$time,iter.length$value,iter.length$iter),])
# Trasform results by length to dbeOutput format 
dbeOutput@lenStruc$estim <- data.frame(time=res.length$time,space=res.length$space,technical=res.length$technical,length=res.length$value,value=as.numeric(res.length$estimate))
dbeOutput@lenStruc$rep <- data.frame(time=iter.length$time,space=iter.length$space,technical=iter.length$technical,length=iter.length$value,value=as.numeric(iter.length$value.iter),iter=iter.length$iter)
dbeOutput@lenVar <- data.frame(time=res.length$time,space=res.length$space,technical=res.length$technical,length=res.length$value,value=as.numeric(res.length$variance))
dbeOutput@lenNum$ci <- dbeCalc(dbeOutput,type="CI", vrbl="l",probs=c(0.025,0.975),replicates=T,update=F)
dbeOutput@lenNum$cv <- dbeCalc(dbeOutput,type="CV", vrbl="l",probs=c(0.025,0.975),replicates=T,update=F)$DF
dbeOutput@lenNum$DCRcvIndicator <- dbeCalc(dbeOutput,type="CV", vrbl="l",probs=c(0.025,0.975),replicates=T,update=F)$dcrInd

## Results by age 
if (nrow(age)!=0)
  {
  res.age <- res[res$class=="age"&!is.na(res$estimate),]
  iter.age <- iter[iter$class=="age"&!is.na(iter$value.iter),]
  res.age <- with(res.age, res.age[order(res.age$technical,res.age$space,res.age$time,res.age$value),])
  iter.age <- with(iter.age, iter.age[order(iter.age$technical,iter.age$space,iter.age$time,iter.age$value,iter.age$iter),])
  dbeOutput@ageStruc$estim <- data.frame(time=res.age$time,space=res.age$space,technical=res.age$technical,age=res.age$value,value=as.numeric(res.age$estimate)) 
  dbeOutput@ageStruc$rep <- data.frame(time=iter.age$time,space=iter.age$space,technical=iter.age$technical,age=iter.age$value,value=as.numeric(iter.age$value.iter),iter=iter.age$iter) 
  dbeOutput@ageVar <- data.frame(time=res.age$time,space=res.age$space,technical=res.age$technical,age=res.age$value,value=as.numeric(res.age$variance)) 
  dbeOutput@ageNum$ci <- dbeCalc(dbeOutput,type="CI", vrbl="a",probs=c(0.025,0.975),replicates=T,update=F)
  dbeOutput@ageNum$cv <- dbeCalc(dbeOutput,type="CV", vrbl="a",probs=c(0.025,0.975),replicates=T,update=F)$DF                            ######### MM 03/04/2009
  dbeOutput@ageNum$DCRcvIndicator <- dbeCalc(dbeOutput,type="CV", vrbl="a",probs=c(0.025,0.975),replicates=T,update=F)$dcrInd
  }
if (!dbeOutput@methodDesc%in%"bootstrap") warnings("'methodDesc' slot in 'dbeOutput' object will be updated!!")
dbeOutput@methodDesc <- "bootstrap"


return(dbeOutput)
  
})

