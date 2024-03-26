# Raise_Lgth_Boot

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
#sol.dbe.an <- RaiseLgth (dbeOutput, csObject, clObject)
#sol.dbe.boot <- RaiseLgthBoot (dbeOutput, csObject, clObject, B=3)
#sol.dbe.boot <- RaiseLgthBoot (dbeOutput, csObject, clObject, B=100) # took about 90s on my laptop

# iter = 0 should be analytical estimates
#tmpboot <- sol.dbe.boot@lenStruc$rep
#tmpboot <- tmpboot[tmpboot$iter==0,]
#tmpan <-sol.dbe.an@lenStruc$estim
#tmpm <- merge(tmpan, tmpboot, by=c("time", "space", "technical", "length"), all=T, suffixes = c(".an",".boot"))
#plot(tmpm$value.an, tmpm$value.boot)  # values match
#tmpm [1:20,] # although tmpboot has estimates of zero for lengths not in tmpan

#set.seed(191)
#sol.dbe.boot1 <- RaiseLgthBoot (dbeOutput, csObject, clObject, B=3)
# edited code
#set.seed(191)
#sol.dbe.boot2 <- RaiseLgthBoot (dbeOutput, csObject, clObject, B=3)

#all.equal(sol.dbe.boot1, sol.dbe.boot2)

################################################################################
################################################################################
################################################################################
################################################################################

RowSum <- function(X,MARGIN) {
d <- dim(X)
#array is permuted --> MARGIN dimensions are shifted to first rows
newD <- c(MARGIN,c(1:length(d))[-MARGIN])
newX <- aperm(X,newD)
#sum is made over columns
rowSums(newX,na.rm=TRUE,dims=length(MARGIN))
}


################################################################################
################################################################################

#
#sex=as.character(NA)
#sampPar=TRUE
#B <- 10
#sp <- dbeOutput@species
#taxon <- sp
#spp <- sp
#
## extCatchCat
#extCatchCat <- function(x) {
#sapply(x, function(x) substring(as.character(x),1,3))
#}
#
## spdAgreg
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
## As.num
#As.num <- function(x) as.numeric(as.character(x))
#
## Need sample function that returns one value if give data with one value x,
## rather than returning sample of vector 1:x , see help file for sample
#resample <- function(x, size, replace,...)
#  if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
#  } else sample(x, size, replace,...)
#
#
#
#
##method for handle 'sex' conditional key field in SL : an new 'sex' field is inserted in HL, matching SL sex field, and true HL sex field is moved down and renamed as 'lsex'
#slSex <- function(slTab,hlTab) {
#if (names(slTab)[1]!=names(hlTab)[1]) stop("tables must be consistent!!")
#ind <- NULL
#if (names(slTab)[1]=="sampType") ind <- 1:14 
#if (names(slTab)[1]=="PSUid") ind <- 1:15
#if (is.null(ind)) stop("wrong input tables!!") 
#slTab <- slTab[,ind] ; slTab$lsex <- slTab$sex ; hlTab$N <- 1:nrow(hlTab)
#hlTab <- merge(hlTab,slTab,all.x=TRUE) ; hlTab <- hlTab[order(hlTab$N),-match("N",names(hlTab))]
#sex <- as.character(hlTab$sex) 
#hlTab$sex <- as.character(hlTab$lsex)
#hlTab$lsex <- sex
#rownames(hlTab) <- 1:nrow(hlTab)
#return(hlTab)
#}
#
#
##fonction qui concatène les dimensions avant d'appliquer un tapply sur LA dimension
##retourne aussi en sortie les éléments séparés constituant les en-têtes (en prévision de calculs matriciels) -> matrice avec une ligne par dim 
#catApply <- function(X, INDEX, FUN = NULL, ..., simplify = TRUE,collapse=":@&@&@:"){
#
##on commence par concaténer les éléments de la liste
#catIndex <- apply(matrix(unlist(lapply(INDEX,as.character)),nrow=length(INDEX[[1]])),1, function(x) paste(x,collapse=collapse,sep=""))
#
#return1 <- tapply(X,list(catIndex),FUN=FUN,simplify=simplify,...)
#
#return2 <- sapply(names(return1),function(x) strsplit(x,collapse)[[1]])
#if (is.null(nrow(return2))) return2 <- t(as.matrix(return2))
#
#return(list(val=return1, ind=return2))
#}
#
#
#
#
##fonction de réplication d'un vecteur résultat de 'catApply' via la mise en parallèle des indices '$ind' de ce vecteur avec les indices du vecteur modèle
##peut aussi fonctionner juste pour ordonner un vecteur indicé conformément à un autre
#dbeReplic <- function(catAppObj, indMat, collapse=":@&@&@:"){
#
#if (!is.null(nrow(indMat))) {
#  indRef <- apply(indMat,2, function(x) paste(x,collapse=collapse,sep=""))
#} else {
#  indRef <- as.character(indMat)
#}
#res <- catAppObj$val[indRef]
#names(res) <- indRef #il faut gérer les en-têtes non trouvées
#return(list(val = res, ind = indMat))
#
#}
#
#
##fonction d'agrégation d'un vecteur résultat de 'catApply' conservant les dimensions choisies
#dbeAgg <- function(catAppObj, ind , FUN = NULL, ..., collapse=":@&@&@:"){
#
#return(catApply(catAppObj$val,lapply(ind,function(x) catAppObj$ind[x,]), FUN=FUN, ..., collapse=collapse))
#
#}
#


################################################################################
################################################################################



Raise_Lgth_Boot <- function(dbeOutput, csObject, clObject,spp,taxon,sex=as.character(NA),sampPar=TRUE,B){

dbeOutput@catchCat <- toupper(dbeOutput@catchCat)                               #
csObject@sl$sort <- toupper(csObject@sl$sort)                                   # MM 29/04/2010
csObject@hl$sort <- toupper(csObject@hl$sort)                                   #
csObject@ca$sort <- toupper(csObject@ca$sort)                                   #

sp <- dbeOutput@species

if (missing(taxon)) taxon <- sp
if (missing(spp)) spp <- sp  

eval(parse('',text=paste("csObject <- subsetSpp(csObject,spp%in%",deparse(spp),")",sep=""))) 
ccat <- dbeOutput@catchCat  
#'catchCat' slot must be "LAN"                                                    #
if (!all(is.na(ccat)) & (all(ccat%in%"LAN") | missing(clObject))) {                                                        #    
  csObject@sl <- csObject@sl[extCatchCat(csObject@sl$sort)%in%ccat,]            #
  csObject@hl <- csObject@hl[extCatchCat(csObject@hl$sort)%in%ccat,]            #
} else {
stop("wrong 'catchCat' slot in dbe object!")}
if (nrow(csObject@hl)==0) stop("no sampled landings for specified species!!")                                                                               #
#as sort is a factor,...                                                        #
csObject@sl$sort <- as.character(csObject@sl$sort)                              # 05/05/2009 MM
csObject@hl$sort <- as.character(csObject@hl$sort)                              #

#number of length samples (length sample= PSUid/SSUid/TSUid in HL if strategy="cc", and PSUid/SSUid in HL if strategy="metier") is calculated at this stage (before subsetting on 'sp' and 'sex')
cc <- is.na(dbeOutput@strataDesc@techStrata)==FALSE & dbeOutput@strataDesc@techStrata=="commCat"                                                                    #
if (cc) Unit <- paste(csObject@hl$PSUid,csObject@hl$SSUid,csObject@hl$TSUid,sep=":-:") else Unit <- paste(csObject@hl$PSUid,csObject@hl$SSUid,sep=":-:")            #
nSAMP <- spdAgreg(list(value=Unit),BY=list(time=csObject@hl$time,space=csObject@hl$space,technical=csObject@hl$technical),function(x) length(unique(x)))            #
dbeOutput@nSamp$len <- nSAMP                                                                                                                                        #  ADDED : MM 02/04/2009


# weight of the species/taxa/sex in the sampled box
# this weight is to be calculated by summing the sampled weight by species/taxa/sex before subsetting
# and added as a third weight variable

indNewWt <- TRUE
if (is.na(sex) & all(taxon%in%sp)) indNewWt <- FALSE
if (indNewWt) {

  NW <- spdAgreg(list(newWt=csObject@sl$subSampWt),BY=list(PSUid=csObject@sl$PSUid,SSUid=csObject@sl$SSUid,TSUid=csObject@sl$TSUid,
                                                         time=csObject@sl$time,space=csObject@sl$space,
                                                         technical=csObject@sl$technical,sort=csObject@sl$sort),sum,na.rm=TRUE)
                  
  mergeSL <- merge(csObject@sl,NW,all.x=TRUE,sort=FALSE)
  ssw <- csObject@sl$subSampWt
  csObject@sl$subSampWt <- mergeSL$newWt
  csObject@sl$lenCode <- ssw
} else {
  csObject@sl$lenCode <- csObject@sl$subSampWt
}

#subsetting "clObject"                                                                                    #
x <- clObject ; cl <- cl(x)                                                                               #
clObject <- new("clDataCons", desc=x@desc, cl=cl[cl$taxon%in%taxon,])                                     #
  
#we calculate the number of samples by strata now
ind <- sampledFO(csObject,species=spp,fraction=ccat,sampPar=sampPar)$sampLg
Hl <- cbind(csObject@hh[,c("PSUid","time","space","technical")],ind=ind)   #sampled FO by strata
 
#subsetting "csObject" for nMEAS calculation
temp <- NULL                                                                                                                                                                                             #
eval(parse('',text=paste("temp <- subsetSpp(csObject,spp%in%",deparse(sp),")",sep="")))               
if (!is.na(sex)) eval(parse('',text=paste("temp <- subsetSpp(temp,sex%in%",deparse(sex),")",sep="")))  #to keep all 'tr' and 'hh' information, we use "subsetSpp" method                                                                 

#number of fish measured in HL (for species specified in dbeOutput object)                                                                                          # ADDED : MM 02/04/2009                                           #
nMEAS <- spdAgreg(list(value=temp@hl$lenNum),BY=list(time=temp@hl$time,space=temp@hl$space,technical=temp@hl$technical),sum,na.rm=TRUE)             #
dbeOutput@nMeas$len <- nMEAS                                                                                                                                        #
rm(temp)

HH <- csObject@hh
SL <- csObject@sl ; SL$TSUid <- factor(as.character(SL$TSUid),exclude=NULL)                            #MM 24/04/2009
HL <- csObject@hl ; HL$TSUid <- factor(as.character(HL$TSUid),exclude=NULL)                            #MM 24/04/2009
#creating PSTUid field, concatenation of PSUid, SSUid, TSUid
SL$PSTUid <- apply(SL[,c("PSUid","SSUid","TSUid")],1,function(x) paste(x,collapse=":-:"))
HL$PSTUid <- apply(HL[,c("PSUid","SSUid","TSUid")],1,function(x) paste(x,collapse=":-:"))
#creating STR field, concatenation of time, space, technical
HH$STR <- apply(HH[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))
SL$STR <- apply(SL[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))
HL$STR <- apply(HL[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))
Hl$STR <- apply(Hl[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))

if (nrow(SL)==0) stop("the specified parameters resulted in an empty table!!")

# store original factor levels to use with bootstrap samples
levLenCls <- levels(factor(HL$lenCls))
levSTR <- levels(factor(SL$STR))
levSort <- levels(factor(SL$sort))
levPSUid <- levels(factor(SL$PSUid))
levSSUid <- levels(factor(SL$SSUid))
levTSUid <- levels(SL$TSUid)                           #MM 24/04/2009

#total landed weights per strata
clObject@cl$landMult[is.na(clObject@cl$landMult)] <- 1
#TotLand = OffLand*Multi + UnallocCat + MisallocCat
totLand <- mapply(function(w,x,y,z) sum(c(w*x,y,z),na.rm=TRUE),clObject@cl$landWt,clObject@cl$landMult,clObject@cl$unallocCatchWt,clObject@cl$misRepCatchWt)
totLandings <- spdAgreg(list(W=totLand),BY=list(time=clObject@cl$time,space=clObject@cl$space,technical=clObject@cl$technical),sum,na.rm=TRUE)

  ##W <- tapply(totLandings$W*1000,list(factor(apply(totLandings[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:")),levels=levSTR)),sum,na.rm=TRUE)
W <- catApply(totLandings$W*1000,list(apply(totLandings[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))),
        sum,na.rm=TRUE)

## New code not in Raise_Lgth ##

if ( is.na(dbeOutput@methodDesc)) {
  dbeOutput@methodDesc <- "bootstrap"
  print("dbeOutput methodDesc slot has been set to 'bootstrap'")}

if ( dbeOutput@methodDesc != "bootstrap" ) {
  warning(paste ("dbeOutput methodDesc slot has been changed from", dbeOutput@methodDesc, "to 'bootstrap'"), call. = F)
  dbeOutput@methodDesc <- "bootstrap" }

#n : number of sampled PSUid per strata
n <- tapply(HL$PSUid,list(STR=factor(HL$STR,levels=levels(factor(SL$STR)))),function(x) length(unique(x)))

n <- n[!is.na(n)]     #added MM 12/04/2010

# PSUids for each strata combination
lenids <- unique( HL [,c( "PSUid", "STR")] )
lenids <- lenids[order(lenids$STR),] # lenids and n should now have STR in same order
uStrat <- names(n)
#identify start and end positions for each strata
start.pos = c(1, (cumsum(n)[-length(n)]) +1 )
end.pos = cumsum(n)

# sample new set of PSUid for each strata combination - stratified bootstrap resampling - may be able to use boot function instead
bootLenPSUid = matrix (NA, nrow = dim(lenids)[1], ncol=B+1)
dimnames(bootLenPSUid) = list(NULL, c("orig", paste("iter.",1:B,sep="")))
# original sample ids in first column
bootLenPSUid[,1] = lenids$PSUid

# put resampled ids for each strata into vector in relevant places,
# assigning to all columns using size = n[i] * B, instead of using loop by iteration 1 to B
for ( i in 1:length(uStrat) ){
# order of nLen.vec and uStrat needs to match
  bootLenPSUid [ start.pos[i]:end.pos[i], -1] = resample (lenids$PSUid [ lenids$STR == uStrat[i] ], size = n[i] * B, replace=T )
  }

HH.orig <- HH  #added MM 29/07/2009
Hl.orig <- Hl  #added MM 29/07/2009
SL.orig <- SL
HL.orig <- HL
####### START OF BOOTSTRAP LOOP #################
ld.list = vector("list", B+1)

print ("iter")
# i=1 uses original data, its output is labelled iter=0
# printing iteration number every 50 iterations
for (i in 1:(B+1) ){

if(identical ((i-1)/50, (i-1)%/%50))print(i-1)


HH = merge(data.frame(PSUid = bootLenPSUid[,i]), HH.orig, by="PSUid") #added MM 29/07/2009
Hl = merge(data.frame(PSUid = bootLenPSUid[,i]), Hl.orig, by="PSUid") #

SL = merge(data.frame(PSUid = bootLenPSUid[,i]), SL.orig, by="PSUid")
  # new SL won't have same number of rows as original SL if some PSUid were repeated because of sort variable.
HL = merge(data.frame(PSUid = bootLenPSUid[,i]), HL.orig, by="PSUid")


## Code as in Raise_Lgth , except for levels=

#weight of the level
#wl <- tapply(SL$wt,list(STR=SL$STR,sort=SL$sort,TSUid=SL$TSUid,SSUid=SL$SSUid,PSUid=SL$PSUid),sum,na.rm=TRUE)   #reference for factor levels
  ##wl <- tapply(SL$wt,list(STR=factor(SL$STR,levels=levSTR),
  ##                                                    sort=factor(SL$sort,levels=levSort),
  ##                                                    TSUid=factor(SL$TSUid,levels=levTSUid,exclude=NULL),    #MM 24/04/2009
  ##                                                    SSUid=factor(SL$SSUid,levels=levSSUid),
  ##                                                    PSUid=factor(SL$PSUid,levels=levPSUid)),sum,na.rm=TRUE)   #reference for factor levels
wl <- catApply(SL$wt,list(STR=SL$STR,sort=SL$sort,TSUid=SL$TSUid,SSUid=SL$SSUid,PSUid=SL$PSUid),sum,na.rm=TRUE)  #reference for factor levels

#sampled weight
#ws <- tapply(SL$subSampWt,list(STR=SL$STR,sort=SL$sort,TSUid=SL$TSUid,SSUid=SL$SSUid,PSUid=SL$PSUid),sum,na.rm=TRUE)
  ##ws <- tapply(SL$subSampWt,list(STR=factor(SL$STR,levels=levSTR),
  ##                                                    sort=factor(SL$sort,levels=levSort),
  ##                                                    TSUid=factor(SL$TSUid,levels=levTSUid,exclude=NULL),     #MM 24/04/2009
  ##                                                    SSUid=factor(SL$SSUid,levels=levSSUid),
  ##                                                    PSUid=factor(SL$PSUid,levels=levPSUid)),sum,na.rm=TRUE)
ws <- catApply(SL$subSampWt,list(STR=SL$STR,sort=SL$sort,TSUid=SL$TSUid,SSUid=SL$SSUid,PSUid=SL$PSUid),sum,na.rm=TRUE)


#subsetting "csObject" on taxa/sex                                                                #added MM 29/07/2009                                                                                                                              #
eval(parse('',text=paste("SL <- subset(SL,spp%in%",deparse(sp),")",sep="")))                      #
if (!is.na(sex)) eval(parse('',text=paste("SL <- subset(SL,sex%in%",deparse(sex),")",sep="")))    #

                                                              

#weight of the species/taxa/sex
#wt  <- tapply(as.numeric(as.character(SL$lenCode)),list(STR=SL$STR,sort=SL$sort,TSUid=SL$TSUid,SSUid=SL$SSUid,PSUid=SL$PSUid),sum,na.rm=TRUE)
  ##wt  <- tapply(as.numeric(as.character(SL$lenCode)),list(STR=factor(SL$STR,levels=levSTR),
  ##                                                    sort=factor(SL$sort,levels=levSort),
  ##                                                    TSUid=factor(SL$TSUid,levels=levTSUid,exclude=NULL),      #MM 24/04/2009
  ##                                                    SSUid=factor(SL$SSUid,levels=levSSUid),
  ##                                                    PSUid=factor(SL$PSUid,levels=levPSUid)),sum,na.rm=TRUE)
wt  <- catApply(SL$subSampWt,list(as.character(SL$STR),as.character(SL$sort),as.character(SL$TSUid),as.character(SL$SSUid),
                                  as.character(SL$PSUid)),sum,na.rm=TRUE)    

wt <- dbeReplic(wt,wl$ind)
#number of fish in the sample by length
  ##d_j <- tapply(HL$lenNum,list(STR=factor(HL$STR,levels=levSTR),
  ##                              sort=factor(HL$sort,levels=levSort),
  ##                              TSUid=factor(HL$TSUid,levels=levTSUid,exclude=NULL),                            #MM 24/04/2009
  ##                              SSUid=factor(HL$SSUid,levels=levSSUid),
  ##                              PSUid=factor(HL$PSUid,levels=levPSUid),
  ##                              lenCls=factor(HL$lenCls,levels=levLenCls)),sum,na.rm=TRUE)
d_j  <- catApply(HL$lenNum,list(as.character(HL$STR),as.character(HL$sort),as.character(HL$TSUid),as.character(HL$SSUid),
                                  as.character(HL$PSUid),HL$lenCls),sum,na.rm=TRUE)    


#TSUid stage
  ##w_tsu <- RowSum(wt*wl/ws,c(1,3:5))
  ##wl_tsu <- RowSum(wl,c(1,3:5))
w_tsu <- dbeAgg(list(val = wt$val * wl$val / ws$val, ind = wl$ind), c(1,3:5),sum,na.rm=TRUE)
wl_tsu <- dbeAgg(wl,c(1,3:5),sum,na.rm=TRUE)


#SSUid stage
  ##expr <- d_j*(as.vector(wl/ws))
  ##sum.d_jssu <- RowSum(expr,c(1,5,6))
  ##
  ##sum.w_ssu <- RowSum(w_tsu,c(1,4))
  ##sum.wl_ssu <- RowSum(wl_tsu,c(1,4))
expr <- list(val = d_j$val * dbeReplic(list(val = wl$val/ws$val, ind = wl$ind), d_j$ind[1:5,])$val , ind = d_j$ind)
sum.d_jssu <- dbeAgg(expr,c(1,5,6),sum,na.rm=TRUE)
sum.w_ssu <- dbeAgg(w_tsu,c(1,4),sum,na.rm=TRUE)
sum.wl_ssu <- dbeAgg(wl_tsu,c(1,4),sum,na.rm=TRUE)


#number of SSUid (total and sampled)
  ##Mi <- tapply(HH$SSUid,list(STR=factor(HH$STR,levels=levSTR),
  ##                                      PSUid=factor(HH$PSUid,levels=levPSUid)),function(x) length(unique(x)))
Mi <- catApply(HH$SSUid,list(as.character(HH$STR),as.character(HH$PSUid)),function(x) length(unique(x)))
Mi_a <- dbeReplic(Mi,sum.d_jssu$ind[1:2,])  #on met à la dimension de 'sum.d_jssu'
Mi_b <- dbeReplic(Mi,sum.w_ssu$ind[1:2,])  #on met à la dimension de 'sum.w_ssu'


Hl <- Hl[!is.na(Hl$ind),]

  ##mi <- tapply(Hl$ind,list(STR=factor(Hl$STR,levels=levSTR),
  ##                           PSUid=factor(Hl$PSUid,levels=levPSUid)),function(x) length(unique(x)))
mi <- catApply(Hl$ind,list(STR=as.character(Hl$STR),as.character(Hl$PSUid)),length)
mi_a <- dbeReplic(mi,sum.d_jssu$ind[1:2,])
mi_b <- dbeReplic(mi,sum.w_ssu$ind[1:2,])


#PSUid stage                           
  ##d_jpsu <- sum.d_jssu*as.vector(Mi/mi)
  ##w_psu <- sum.w_ssu*Mi/mi
  ##wl_psu <- sum.wl_ssu*Mi/mi
  ##
  ##sum.d_jpsu <- RowSum(d_jpsu,c(1,3))
  ##sum.w_psu <- rowSums(w_psu,na.rm=TRUE)
  ##sum.wl_psu <- rowSums(wl_psu,na.rm=TRUE)
  ##
  ##D_j <- sum.d_jpsu*as.vector(W/sum.wl_psu)
  ##WHat <-  sum.w_psu*W/sum.wl_psu

d_jpsu <- list(val = sum.d_jssu$val * Mi_a$val / mi_a$val, ind = sum.d_jssu$ind)
w_psu <- list(val = sum.w_ssu$val * Mi_b$val / mi_b$val, ind = sum.w_ssu$ind)
wl_psu <- list(val = sum.wl_ssu$val * Mi_b$val / mi_b$val, ind = sum.wl_ssu$ind)

sum.d_jpsu <- dbeAgg(d_jpsu,c(1,3),sum,na.rm=TRUE)
sum.w_psu <- dbeAgg(w_psu,1,sum,na.rm=TRUE)
sum.wl_psu <- dbeAgg(wl_psu,1,sum,na.rm=TRUE)
                                 
#on doit maintenant penser à insérer les 0-values, càd les combinaisons strate/psuid/length non échantillonnées, 
# mais dont l'occurence strate/length a été échantillonnée, dans d_jpsu (croisement des en-têtes de 'sum.d_jpsu' et de 'sum.w_ssu')

  #on commence par croiser les deux paires de dimensions (STR/PSUid et STR/Lgth)             #####
dfTemp <- as.data.frame(t(sum.d_jpsu$ind)) ; names(dfTemp) <- c("V1","V3")                       #
cross <- merge(as.data.frame(t(sum.w_ssu$ind)),dfTemp,all=TRUE)                                  #
  #ensuite, on étend le vecteur input selon cette nouvelle matrice                               #
d_jpsu.new <- dbeReplic(d_jpsu,as.matrix(t(cross)))                                              #
  #il ne reste plus qu'à considérer les 0-values                                                 #
d_jpsu.new$val[is.na(d_jpsu.new$val)] <- 0                                                   #####

#on remet au format
W_a <- dbeReplic(W,sum.d_jpsu$ind[1,])
sum.wl_psu_a <- dbeReplic(sum.wl_psu,sum.d_jpsu$ind[1,])
W_b <- dbeReplic(W,sum.w_psu$ind[1,])

D_j <- list(val = sum.d_jpsu$val * W_a$val / sum.wl_psu_a$val, ind = sum.d_jpsu$ind)

WHat <- list(val = sum.w_psu$val * W_b$val / sum.wl_psu$val, ind = sum.w_psu$ind)


                                                                                                    
 # reformat output, D_j
  ##df.D_j <- cbind(expand.grid(dimnames(D_j)),value=as.vector(D_j))
df.D_j <- as.data.frame(cbind(do.call("rbind",lapply(names(D_j$val),function(x) strsplit(x,":@&@&@:")[[1]]))))
  df.D_j$value=D_j$val

#df.D_j <- df.D_j[!is.na(df.D_j$val),]
#df.D_j <- df.D_j[df.D_j$val>0,]

df.D_j <- cbind(df.D_j,do.call("rbind",lapply(as.character(df.D_j$V1),function(x) strsplit(x,":-:")[[1]])))
names(df.D_j) <- c("STR","length","value","time","space","technical")
df.D_j <- df.D_j[order(df.D_j$time,df.D_j$space,df.D_j$technical,df.D_j$length),] ; rownames(df.D_j) <- 1:nrow(df.D_j)
ld.list[[i]] <- df.D_j[,names(dbeOutput@lenStruc$estim)] # changed for boot

} # End of bootstrap loop
print(i-1)
print("iterations complete")

# Outputs to dbeOutput
df.WHat <- cbind(value=WHat$val/1000,as.data.frame(do.call("rbind",lapply(names(WHat$val),function(x) strsplit(x,":-:")[[1]]))))    #weight in kg : MM 07/04/2009
names(df.WHat) <- c("value","time","space","technical")
df.WHat <- df.WHat[order(df.WHat$time,df.WHat$space,df.WHat$technical),] ; rownames(df.WHat) <- 1:nrow(df.WHat)
dbeOutput@totalW$estim <- df.WHat[,names(dbeOutput@totalW$estim)]

# Length structure
#convert list of length distributions into data.frame matching dbeOutput format
rep.iter = unlist (lapply(ld.list, FUN = function(x) {dim(x)[1]}) )

ld.df =  data.frame  (time =  unlist(lapply(ld.list, FUN = function(x) {x[,"time"]})),
                                space =  unlist(lapply(ld.list, FUN = function(x) {x[,"space"]})),
                                technical = unlist(lapply(ld.list, FUN = function(x) {x[,"technical"]})),
                                length =  unlist(lapply(ld.list, FUN = function(x) {x[,"length"]})),
                                value =  unlist(lapply(ld.list, FUN = function(x) {x[,"value"]})),
                                iter =  rep(0:B, times = rep.iter)  )

# order rows
ld.df$iter <- As.num (ld.df$iter)
ld.df = ld.df [order(ld.df$iter, ld.df$time, ld.df$space, ld.df$technical),]
dimnames(ld.df)[[1]] = 1:(dim(ld.df)[1])
dbeOutput@lenStruc$rep = ld.df


# sum over length classes to give estimate of total N for each iteration
ld.sumiter = spdAgreg (list (value=ld.df$value), BY = list(time=ld.df$time, space=ld.df$space, technical=ld.df$technical, iter=ld.df$iter), sum)
# reorder columns
ld.sumiter = ld.sumiter[, c("time","space","technical","value","iter")]
# order rows
ld.sumiter$iter <- As.num (ld.sumiter$iter)
ld.sumiter = ld.sumiter [order(ld.sumiter$iter, ld.sumiter$time, ld.sumiter$space, ld.sumiter$technical),]
dimnames(ld.sumiter)[[1]] = 1:(dim(ld.sumiter)[1])

dbeOutput@totalN$rep = ld.sumiter

# drop original estimates from ld.sumiter to calculate bootstrap mean & var
ld.sumiter$iter = As.num(ld.sumiter$iter)
ld.sumiter = ld.sumiter[ld.sumiter$iter > 0,]
# mean across iterations of total N
dbeOutput@totalN$estim = spdAgreg (list (value=ld.sumiter$value), BY = list(time=ld.sumiter$time, space=ld.sumiter$space, technical=ld.sumiter$technical), mean)
# variance across iterations of total N
dbeOutput@totalNvar = spdAgreg (list (value=ld.sumiter$value), BY = list(time=ld.sumiter$time, space=ld.sumiter$space, technical=ld.sumiter$technical), var)

# drop original estimates from ld.df to calculate bootstrap mean & var
ld.df = ld.df[ld.df$iter > 0,]
#0-values are not inserted in ld.df, but must be considered in mean and variance calculation, so... 
ld.mean = spdAgreg (list (value=ld.df$value), BY = list(time=ld.df$time, space=ld.df$space, technical=ld.df$technical, length=ld.df$length),
  function(x) mean(c(x,rep(0,length=B-length(x)))))
ld.mean$length = As.num(ld.mean$length)
# should be in correct order, but sort anyway
ld.mean = ld.mean [order(ld.mean$time, ld.mean$space, ld.mean$technical, ld.mean$length),]
dimnames(ld.mean)[[1]] = 1:(dim(ld.mean)[1])

ld.var = spdAgreg (list (value=ld.df$value), BY = list(time=ld.df$time, space=ld.df$space, technical=ld.df$technical, length=ld.df$length),
   function(x) var(c(x,rep(0,length=B-length(x)))))
ld.var$length = As.num(ld.var$length)
ld.var = ld.var [order(ld.var$time, ld.var$space, ld.var$technical, ld.var$length),]
dimnames(ld.var)[[1]] = 1:(dim(ld.var)[1])

dbeOutput@lenStruc$estim = ld.mean
dbeOutput@lenVar = ld.var

return(dbeOutput)

}



###################
# Exported method #
###################



setGeneric("RaiseLgthBoot", function(dbeOutput,
                                 csObject,
                                 clObject,
                                 spp,
                                 taxon,
                                 sex=as.character(NA),  
                                 sampPar=TRUE,
                                 B,
                                 incl.precision=TRUE,
                                 probs=c(0.025,0.975),
                                 ...){
	standardGeneric("RaiseLgthBoot")}
)



setMethod("RaiseLgthBoot", signature(dbeOutput="dbeOutput",csObject="csDataCons",clObject="clDataCons"), function(dbeOutput,
                                                                                                              csObject,
                                                                                                              clObject,
                                                                                                              spp,
                                                                                                              taxon,
                                                                                                              sex=as.character(NA),
                                                                                                              sampPar=TRUE,     
                                                                                                              B,
                                                                                                              incl.precision=TRUE,
                                                                                                              probs=c(0.025,0.975),
                                                                                                              ...){

if (incl.precision) {  

  obj <- Raise_Lgth_Boot(dbeOutput = dbeOutput, csObject = csObject, clObject = clObject, spp=spp,taxon=taxon,sex=sex,sampPar=sampPar,B=B)

  if (!all(is.na(obj@lenStruc$rep))) {
    
    obj <- dbeCalc(obj,type="CV",vrbl="l",replicates=TRUE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="l",probs=probs,replicates=TRUE,update=TRUE)
  }

  if (!all(is.na(obj@totalN$rep))) {
    obj <- dbeCalc(obj,type="CV",vrbl="n",replicates=TRUE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="n",probs=probs,replicates=TRUE,update=TRUE)
  }

  if (!all(is.na(obj@totalW$rep))) {
    obj <- dbeCalc(obj,type="CV",vrbl="w",replicates=TRUE,update=TRUE)
    obj <- dbeCalc(obj,type="CI",vrbl="w",probs=probs,replicates=TRUE,update=TRUE)
  }

  return(obj)

} else {
                                                                                                           
  Raise_Lgth_Boot(dbeOutput = dbeOutput, csObject = csObject, clObject = clObject, spp=spp,taxon=taxon,sex=sex,sampPar=sampPar,B=B)

}

})






