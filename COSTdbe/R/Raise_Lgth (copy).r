#data(sole)
#strD <- strIni(timeStrata="quarter",techStrata="commCat")
#csObject <- csDataCons(csDataVal(subset(sole.cs,sampType%in%c("M","V"))),strD)
#clObject <- clDataCons(clDataVal(sole.cl),strD)
#dbeOutput <- dbeObject(species="Solea solea",catchCat="LAN",strataDesc=strD)
#sex <- as.character(NA)
#
#
#spdAgreg <- COSTcore:::spdAgreg
#
################################################################################
################################################################################
################################################################################
################################################################################


#fonction qui concatène les dimensions avant d'appliquer un tapply sur LA dimension
#retourne aussi en sortie les éléments séparés constituant les en-têtes (en prévision de calculs matriciels) -> matrice avec une ligne par dim 
catApply <- function(X, INDEX, FUN = NULL, ..., simplify = TRUE,collapse=":@&@&@:"){

#on commence par concaténer les éléments de la liste
catIndex <- apply(matrix(unlist(lapply(INDEX,as.character)),nrow=length(INDEX[[1]])),1, function(x) paste(x,collapse=collapse,sep=""))

return1 <- tapply(X,list(catIndex),FUN=FUN,simplify=simplify,...)

return2 <- sapply(names(return1),function(x) strsplit(x,collapse)[[1]])
if (is.null(nrow(return2))) return2 <- t(as.matrix(return2))

return(list(val=return1, ind=return2))
}




#fonction de réplication d'un vecteur résultat de 'catApply' via la mise en parallèle des indices '$ind' de ce vecteur avec les indices du vecteur modèle
#peut aussi fonctionner juste pour ordonner un vecteur indicé conformément à un autre
dbeReplic <- function(catAppObj, indMat, collapse=":@&@&@:"){

if (!is.null(nrow(indMat))) {
  indRef <- apply(indMat,2, function(x) paste(x,collapse=collapse,sep=""))
} else {
  indRef <- as.character(indMat)
}
res <- catAppObj$val[indRef]
names(res) <- indRef #il faut gérer les en-têtes non trouvées
return(list(val = res, ind = indMat))

}


#fonction d'agrégation d'un vecteur résultat de 'catApply' conservant les dimensions choisies
dbeAgg <- function(catAppObj, ind , FUN = NULL, ..., collapse=":@&@&@:"){

return(catApply(catAppObj$val,lapply(ind,function(x) catAppObj$ind[x,]), FUN=FUN, ..., collapse=collapse))

}


          
RowSum <- function(X,MARGIN) {
d <- dim(X)
#array is permuted --> MARGIN dimensions are shifted to first rows
newD <- c(MARGIN,c(1:length(d))[-MARGIN])
newX <- aperm(X,newD)
#sum is made over columns
rowSums(newX,na.rm=TRUE,dims=length(MARGIN))
}


             
Raise_Lgth <- function(csObject,clObject,dbeOutput,spp,taxon,sex=as.character(NA),sampPar=TRUE){

dbeOutput@catchCat <- toupper(dbeOutput@catchCat)                               #
csObject@sl$sort <- toupper(csObject@sl$sort)                                   # MM 29/04/2010
csObject@hl$sort <- toupper(csObject@hl$sort)                                   #
csObject@ca$sort <- toupper(csObject@ca$sort)                                   #

sp <- dbeOutput@species

if (missing(taxon)) taxon <- sp
if (missing(spp)) spp <- sp

if (length(spp)>1 & !is.na(sex)) stop("wrong spp and/or sex parameters : this case can't be considered!!")
if (length(spp)>1 & length(sp)>1) stop("wrong spp and/or dbeOutput@species values : this case can't be considered!!")

eval(parse('',text=paste("csObject <- subsetSpp(csObject,spp%in%",deparse(spp),")",sep="")))

ccat <- dbeOutput@catchCat
#'catchCat' slot must be "LAN"
if (!all(is.na(ccat)) & (all(toupper(ccat)%in%"LAN") | missing(clObject))) {
  csObject@sl <- csObject@sl[extCatchCat(csObject@sl$sort)%in%ccat,]
  csObject@hl <- csObject@hl[extCatchCat(csObject@hl$sort)%in%ccat,]
} else {
stop("wrong 'catchCat' slot in dbe object!")}
if (nrow(csObject@hl)==0) stop("no sampled landings for specified species!!")
#as sort is a factor,...                                                        #
csObject@sl$sort <- as.character(csObject@sl$sort)                              # 05/05/2009 MM
csObject@hl$sort <- as.character(csObject@hl$sort)                              #
#in TSUid field, all values are considered (NAs)
csObject@sl$TSUid <- factor(as.character(csObject@sl$TSUid),exclude=NULL)
csObject@hl$TSUid <- factor(as.character(csObject@hl$TSUid),exclude=NULL)

#number of length samples (length sample= PSUid/SSUid/TSUid in HL if strategy="cc", and PSUid/SSUid in HL if strategy="metier") is calculated at this stage (before subsetting on 'sp' and 'sex')
cc <- is.na(dbeOutput@strataDesc@techStrata)==FALSE & dbeOutput@strataDesc@techStrata=="commCat"                                                                    #
if (cc) Unit <- paste(csObject@hl$PSUid,csObject@hl$SSUid,csObject@hl$TSUid,sep=":-:") else Unit <- paste(csObject@hl$PSUid,csObject@hl$SSUid,sep=":-:")            #
nSAMP <- spdAgreg(list(value=Unit),BY=list(time=csObject@hl$time,space=csObject@hl$space,technical=csObject@hl$technical),function(x) length(unique(x)))            #
dbeOutput@nSamp$len <- nSAMP                                                                                                                                        #  ADDED : MM 02/04/2009

#we calculate the number of samples by strata now
ind <- sampledFO(csObject,species=spp,fraction=ccat,sampPar=sampPar)$sampLg
Hl <- cbind(csObject@hh[,c("PSUid","SSUid","time","space","technical")],ind=ind)

#on insère les indicateurs d'échantillonnage
IndSL <- merge(csObject@sl,Hl,all.x=TRUE,sort=FALSE)                            #
csObject@sl$wt <- csObject@sl$wt*IndSL$ind                                      # MM modif 28/04/2010
csObject@sl$subSampWt <- csObject@sl$subSampWt*IndSL$ind                        #

#some calculations must be made BEFORE subsetting

#SL_STR <- do.call(`paste`, c(csObject@sl[,c("time","space","technical")],list(sep=":-:")))   #modif MM 6/6/2011  ; concatenation problems with 'apply' and 'paste' (spaces)

SL_STR <- apply(csObject@sl[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))


#weight of the level
  ##wl <- tapply(csObject@sl$wt,list(STR=SL_STR,sort=csObject@sl$sort,TSUid=csObject@sl$TSUid,SSUid=csObject@sl$SSUid,
  ##PSUid=csObject@sl$PSUid),sum,na.rm=TRUE)   #reference for factor levels   <-----
wl <- catApply(csObject@sl$wt,list(STR=SL_STR,sort=csObject@sl$sort,TSUid=csObject@sl$TSUid,SSUid=csObject@sl$SSUid,PSUid=csObject@sl$PSUid),sum,na.rm=TRUE)  #reference for factor levels

#sampled weight
  ##ws <- tapply(csObject@sl$subSampWt,list(STR=SL_STR,sort=csObject@sl$sort,TSUid=csObject@sl$TSUid,SSUid=csObject@sl$SSUid,
  ##PSUid=csObject@sl$PSUid),sum,na.rm=TRUE)      <-----
ws <- catApply(csObject@sl$subSampWt,list(STR=SL_STR,sort=csObject@sl$sort,TSUid=csObject@sl$TSUid,SSUid=csObject@sl$SSUid,PSUid=csObject@sl$PSUid),sum,na.rm=TRUE)

#subsetting "csObject"                                                                                    #                                                                                                          #
eval(parse('',text=paste("csObject <- subsetSpp(csObject,spp%in%",deparse(sp),")",sep="")))               #
if (!is.na(sex)) eval(parse('',text=paste("csObject <- subsetSpp(csObject,sex%in%",deparse(sex),")",sep="")))  #to keep all 'tr' and 'hh' information, we use "subsetSpp" method                                                              #

#subsetting "clObject"                                                                                    #
if (!missing(clObject)){
  x <- clObject ; cl <- cl(x)                                                                               #
  clObject <- new("clDataCons", desc=x@desc, cl=cl[cl$taxon%in%taxon,])                                     #
}

#number of fish measured in HL (for species specified in dbeOutput object)                                                                                          # ADDED : MM 02/04/2009                                           #
nMEAS <- spdAgreg(list(value=csObject@hl$lenNum),BY=list(time=csObject@hl$time,space=csObject@hl$space,technical=csObject@hl$technical),sum,na.rm=TRUE)             #
dbeOutput@nMeas$len <- nMEAS                                                                                                                                        #


SL <- csObject@sl                            #MM 24/04/2009
HL <- csObject@hl                             #MM 24/04/2009
#creating STR field, concatenation of time, space, technical

#HHSTR <- do.call(`paste`, c(csObject@hh[,c("time","space","technical")],list(sep=":-:")))   #modif MM 6/6/2011
#SL$STR <- do.call(`paste`, c(SL[,c("time","space","technical")],list(sep=":-:")))           #modif MM 6/6/2011
#HL$STR <- do.call(`paste`, c(HL[,c("time","space","technical")],list(sep=":-:")))           #modif MM 6/6/2011
#Hl$STR <- do.call(`paste`, c(Hl[,c("time","space","technical")],list(sep=":-:")))           #modif MM 6/6/2011


HHSTR <- apply(csObject@hh[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))
SL$STR <- apply(SL[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))
HL$STR <- apply(HL[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))
Hl$STR <- apply(Hl[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))

if (nrow(SL)==0) stop("the specified parameters resulted in an empty table!!")

  ##nam <- dimnames(wl)   <-----
#nam <- do.call("rbind",lapply(names(wl),function(x) strsplit(x,":@&@&@:")[[1]]))

  
#weight of the species/taxa/sex
  ##wt  <- tapply(SL$subSampWt,list(STR=factor(as.character(SL$STR),levels=nam$STR),sort=factor(as.character(SL$sort),levels=nam$sort),
  ##                                 TSUid=factor(as.character(SL$TSUid),exclude=NULL,levels=nam$TSUid),
  ##                                 SSUid=factor(as.character(SL$SSUid),levels=nam$SSUid),
  ##                                 PSUid=factor(as.character(SL$PSUid),levels=nam$PSUid)),sum,na.rm=TRUE) <-----

wt  <- catApply(SL$subSampWt,list(as.character(SL$STR),as.character(SL$sort),as.character(SL$TSUid),as.character(SL$SSUid),
                                  as.character(SL$PSUid)),sum,na.rm=TRUE) 
wt <- dbeReplic(wt,wl$ind)   
#number of fish in the sample by length
  ##d_j <- tapply(HL$lenNum,list(STR=factor(HL$STR,levels=nam$STR),
  ##                              sort=factor(HL$sort,levels=nam$sort),
  ##                              TSUid=factor(HL$TSUid,levels=nam$TSUid,exclude=NULL),            #MM 24/04/2009
  ##                              SSUid=factor(HL$SSUid,levels=nam$SSUid),
  ##                              PSUid=factor(HL$PSUid,levels=nam$PSUid),
  ##                              lenCls=HL$lenCls),sum,na.rm=TRUE)

d_j  <- catApply(HL$lenNum,list(as.character(HL$STR),as.character(HL$sort),as.character(HL$TSUid),as.character(HL$SSUid),
                                  as.character(HL$PSUid),HL$lenCls),sum,na.rm=TRUE)    

#TSUid stage
  #system.time(d_jtsuT <- spdApply(d_j*(as.vector(wl/ws)),c(1,3:6),sum,na.rm=TRUE))
  ##w_tsu <- RowSum(wt*wl/ws,c(1,3:5))  <------ 
w_tsu <- dbeAgg(list(val = wt$val * wl$val / ws$val, ind = wl$ind), c(1,3:5),sum,na.rm=TRUE)
  
  ##wl_tsu <- RowSum(wl,c(1,3:5)) <------
wl_tsu <- dbeAgg(wl,c(1,3:5),sum,na.rm=TRUE)

#SSUid stage
  #sum.d_jssu <- apply(d_jtsu,c(1,4,5),sum,na.rm=TRUE)
  ##expr <- d_j*(as.vector(wl/ws)) <------
expr <- list(val = d_j$val * dbeReplic(list(val = wl$val/ws$val, ind = wl$ind), d_j$ind[1:5,,drop=FALSE])$val , ind = d_j$ind)

  ##sum.d_jssu <- RowSum(expr,c(1,5,6)) <------
sum.d_jssu <- dbeAgg(expr,c(1,5,6),sum,na.rm=TRUE)

  ##sum.w_ssu <- RowSum(w_tsu,c(1,4)) <------
sum.w_ssu <- dbeAgg(w_tsu,c(1,4),sum,na.rm=TRUE)

  ##sum.wl_ssu <- RowSum(wl_tsu,c(1,4)) <------
sum.wl_ssu <- dbeAgg(wl_tsu,c(1,4),sum,na.rm=TRUE)

#number of SSUid (total and sampled)
  ##Mi <- tapply(csObject@hh$SSUid,list(STR=factor(HHSTR,levels=nam$STR),
  ##                                      PSUid=factor(csObject@hh$PSUid,levels=nam$PSUid)),function(x) length(unique(x))) <------

Mi <- catApply(csObject@hh$SSUid,list(as.character(HHSTR),as.character(csObject@hh$PSUid)),function(x) length(unique(x)))
Mi_a <- dbeReplic(Mi,sum.d_jssu$ind[1:2,,drop=FALSE])  #on met à la dimension de 'sum.d_jssu'
Mi_b <- dbeReplic(Mi,sum.w_ssu$ind[1:2,,drop=FALSE])  #on met à la dimension de 'sum.w_ssu'

Hl <- Hl[!is.na(Hl$ind),]
  ##mi <- tapply(Hl$ind,list(STR=factor(Hl$STR,levels=nam$STR),
  ##                         PSUid=factor(Hl$PSUid,levels=nam$PSUid)),function(x) length(unique(x))) <------

mi <- catApply(Hl$ind,list(STR=as.character(Hl$STR),as.character(Hl$PSUid)),length)
mi_a <- dbeReplic(mi,sum.d_jssu$ind[1:2,,drop=FALSE])
mi_b <- dbeReplic(mi,sum.w_ssu$ind[1:2,,drop=FALSE])
#mi <- tapply(SL$SSUid,list(STR=factor(SL$STR,levels=nam$STR),
#                           PSUid=factor(SL$PSUid,levels=nam$PSUid)),function(x) length(unique(x)))

#PSUid stage
  ##d_jpsu <- sum.d_jssu*Mi/mi <------
d_jpsu <- list(val = sum.d_jssu$val * Mi_a$val / mi_a$val, ind = sum.d_jssu$ind)

  ##w_psu <- sum.w_ssu*Mi/mi <------
w_psu <- list(val = sum.w_ssu$val * Mi_b$val / mi_b$val, ind = sum.w_ssu$ind)

  ##wl_psu <- sum.wl_ssu*Mi/mi <------
wl_psu <- list(val = sum.wl_ssu$val * Mi_b$val / mi_b$val, ind = sum.wl_ssu$ind)

#sum.wt <- apply(wt,1,sum,na.rm=TRUE)
#sum.ws <- apply(ws,1,sum,na.rm=TRUE)

  ##sum.d_jpsu <- RowSum(d_jpsu,c(1,3))
  ##sum.w_psu <- rowSums(w_psu,na.rm=TRUE)
  ##sum.wl_psu <- rowSums(wl_psu,na.rm=TRUE)

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


if (!missing(clObject)) {
  #total landed weights per strata
  clObject@cl$landMult[is.na(clObject@cl$landMult)] <- 1
  #TotLand = OffLand*Multi + UnallocCat + MisallocCat
  totLand <- mapply(function(w,x,y,z) sum(c(w*x,y,z),na.rm=TRUE),clObject@cl$landWt,clObject@cl$landMult,clObject@cl$unallocCatchWt,clObject@cl$misRepCatchWt)
  totLandings <- spdAgreg(list(W=totLand),BY=list(time=clObject@cl$time,space=clObject@cl$space,technical=clObject@cl$technical),sum,na.rm=TRUE)

  ##W <- tapply(totLandings$W*1000,list(factor(apply(totLandings[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:")),
    ##levels=dimnames(d_j)[[1]])),sum,na.rm=TRUE) <------ + l.185

#W <- catApply(totLandings$W*1000,list(do.call(`paste`, c(totLandings[,c("time","space","technical")],list(sep=":-:")))),
#        sum,na.rm=TRUE)                                     #modif MM 6/6/2011


W <- catApply(totLandings$W*1000,list(apply(totLandings[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))),
        sum,na.rm=TRUE)


        
} else {
  
W <- sum.wl_psu

}

#on remet au format
W_a <- dbeReplic(W,sum.d_jpsu$ind[1,,drop=FALSE])
sum.wl_psu_a <- dbeReplic(sum.wl_psu,sum.d_jpsu$ind[1,,drop=FALSE])
W_b <- dbeReplic(W,sum.w_psu$ind[1,,drop=FALSE])

  ##D_j <- sum.d_jpsu*as.vector(W/sum.wl_psu) <------
D_j <- list(val = sum.d_jpsu$val * W_a$val / sum.wl_psu_a$val, ind = sum.d_jpsu$ind)

  ##WHat <-  sum.w_psu*W/sum.wl_psu <------
WHat <- list(val = sum.w_psu$val * W_b$val / sum.wl_psu$val, ind = sum.w_psu$ind)


## variance calculation based on 'Detecting sampling outliers and sampling heterogeneity when...' article (J. Vigneau & S. Mahévas)
######################

#n : number of sampled SSUid per strata
  ##n <- tapply(HL$PSUid,list(STR=factor(HL$STR,levels=levels(factor(SL$STR)))),function(x) length(unique(x)))

n <- tapply(Hl$ind,list(as.character(Hl$STR)),length)
n <- n[names(W_b$val)] ; names(n) <- names(W_b$val)

  ##first  <- W^2
  ##second <- (1-(sum.wl_psu/W))/(sum.wl_psu^2/n)
  ##  third.1  <- sum.d_jpsu/as.vector(sum.wl_psu)
  ##  third.2 <- aperm(array(rep(as.vector(third.1),dim(w_psu)[2]),dim=dim(d_jpsu)[c(1,3,2)]),c(1,3,2))
  ##  third.3 <- d_jpsu-third.2*as.vector(wl_psu)
  ##third <- apply(third.3^2,c(1,3),sum,na.rm=TRUE)/as.vector(n-1)
  ##VarD_j <- third*as.vector(first*second)
  ##VarD_j[is.nan(VarD_j)] <- 0
  ##VarD_j[is.infinite(VarD_j)] <- 0

first  <- (W_b$val)^2
if (!missing(clObject)) {

  second <- (1-(sum.wl_psu$val / W_b$val))/(sum.wl_psu$val^2/n)

} else {   #raising to trips
  
  second <- 1/(sum.wl_psu$val^2/n)

}
  third  <- list(val = sum.d_jpsu$val/dbeReplic(sum.wl_psu,sum.d_jpsu$ind[1,,drop=FALSE])$val , ind = sum.d_jpsu$ind)
#  third.2 <- aperm(array(rep(as.vector(third.1),dim(w_psu)[2]),dim=dim(d_jpsu)[c(1,3,2)]),c(1,3,2))
  third.2 <- d_jpsu.new$val - dbeReplic(third,d_jpsu.new$ind[c(1,3),,drop=FALSE])$val * dbeReplic(wl_psu,d_jpsu.new$ind[1:2,,drop=FALSE])$val  ##attention ici aux 0-values
  third.3 <- dbeAgg(list(val = third.2^2, ind = d_jpsu.new$ind),c(1,3),sum,na.rm=TRUE)
third <- third.3$val / (n-1)[third.3$ind[1,,drop=FALSE]]
VarD_j <- third * (first*second)[third.3$ind[1,,drop=FALSE]]
VarD_j[is.nan(VarD_j)] <- 0
VarD_j[is.infinite(VarD_j)] <- 0

#results are inserted in dbeOutput object
#####################
  #D_j & VarD_j
  ##df.D_j <- cbind(expand.grid(dimnames(D_j)),value=as.vector(D_j))
  ##df.VarD_j <- cbind(expand.grid(dimnames(VarD_j)),value=as.vector(VarD_j))
  
df.D_j <- as.data.frame(cbind(do.call("rbind",lapply(names(D_j$val),function(x) strsplit(x,":@&@&@:")[[1]]))))
  df.D_j$value=D_j$val
df.VarD_j <- as.data.frame(cbind(do.call("rbind",lapply(names(VarD_j),function(x) strsplit(x,":@&@&@:")[[1]]))))
  df.VarD_j$value <- VarD_j

df.VarD_j <- df.VarD_j[!is.na(df.VarD_j$value),] ; df.D_j <- df.D_j[!is.na(df.D_j$val),]
df.D_j <- df.D_j[df.D_j$val>0,] ; df.VarD_j <- merge(df.D_j[,1:2],df.VarD_j,sort=FALSE,all.x=TRUE)

  #D_j
df.D_j <- cbind(df.D_j,do.call("rbind",lapply(as.character(df.D_j[,1]),function(x) strsplit(x,":-:")[[1]])))
names(df.D_j) <- c("STR","length","value","time","space","technical")
df.D_j <- df.D_j[order(df.D_j$time,df.D_j$space,df.D_j$technical,df.D_j$length),] ; rownames(df.D_j) <- 1:nrow(df.D_j)
dbeOutput@lenStruc$estim <- df.D_j[,names(dbeOutput@lenStruc$estim)]


    #VarD_j
  df.VarD_j <- cbind(df.VarD_j,do.call("rbind",lapply(as.character(df.VarD_j[,1]),function(x) strsplit(x,":-:")[[1]])))
  names(df.VarD_j) <- c("STR","length","value","time","space","technical")
  df.VarD_j <- df.VarD_j[order(df.VarD_j$time,df.VarD_j$space,df.VarD_j$technical,df.VarD_j$length),] ; rownames(df.VarD_j) <- 1:nrow(df.VarD_j)
  dbeOutput@lenVar <- df.VarD_j[,names(dbeOutput@lenVar)]


  #WHat
df.WHat <- cbind(value=WHat$val/1000,as.data.frame(do.call("rbind",lapply(names(WHat$val),function(x) strsplit(x,":-:")[[1]]))))    #weight in kg : MM 07/04/2009
names(df.WHat) <- c("value","time","space","technical")
df.WHat <- df.WHat[order(df.WHat$time,df.WHat$space,df.WHat$technical),] ; rownames(df.WHat) <- 1:nrow(df.WHat)
df.WHat <- df.WHat[!is.na(df.WHat$value),]
rownames(df.WHat) <- 1:nrow(df.WHat)
dbeOutput@totalW$estim <- df.WHat[,names(dbeOutput@totalW$estim)]

  #totalN slot is filled with aggregated 'lenStruc$estim' table                                                                    #added MM 27/04/2009
totN <- with(dbeOutput@lenStruc$estim,aggregate(value,by=list(technical=technical,space=space,time=time),sum,na.rm=TRUE))          #
names(totN)[ncol(totN)] <- "value"                                                                                                 #
totN$value <- round(totN$value)  
totN <- totN[!is.na(totN$value),]  
rownames(totN) <- 1:nrow(totN)                                                                                                #
dbeOutput@totalN$estim <- totN[,names(dbeOutput@totalN$estim)]                                                                     #

  #totalNvar slot is filled with aggregated 'lenVar' table                                                                    #added MM 26/07/2010
totNvar <- with(dbeOutput@lenVar,aggregate(value,by=list(technical=technical,space=space,time=time),sum,na.rm=TRUE))          #
names(totNvar)[ncol(totNvar)] <- "value"                                                                                                 #
#totNvar$value <- round(totNvar$value)  
totNvar <- totNvar[!is.na(totNvar$value),]  
rownames(totNvar) <- 1:nrow(totNvar)                                                                                                #
dbeOutput@totalNvar <- totNvar[,names(dbeOutput@totalNvar)]                                                                     #

return(dbeOutput)

}

###################
# Exported method #
###################



setGeneric("RaiseLgth", function(dbeOutput,
                                 csObject,
                                 clObject,
                                 spp,
                                 taxon,
                                 sex=as.character(NA),
                                 sampPar=TRUE,
                                 incl.precision=TRUE,
                                 probs=c(0.025,0.975),
                                 ...){                                                         #'sampPar' parameter is left to be added
browser()
	standardGeneric("RaiseLgth")}
)



setMethod("RaiseLgth", signature(dbeOutput="dbeOutput",csObject="csDataCons",clObject="clDataCons"), function(dbeOutput,
                                                                                                              csObject,
                                                                                                              clObject,
                                                                                                              spp,
                                                                                                              taxon,
                                                                                                              sex=as.character(NA),
                                                                                                              sampPar=TRUE,
                                                                                                              incl.precision=TRUE,    ## added MM 26/07/2010
                                                                                                              probs=c(0.025,0.975),
                                                                                                              ...){

if (incl.precision) {  

  obj <- Raise_Lgth(csObject=csObject,clObject=clObject,dbeOutput=dbeOutput,spp=spp,taxon=taxon,sex=sex,sampPar=sampPar)

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

  return(obj)

} else {
                                                                                                           
  Raise_Lgth(csObject=csObject,clObject=clObject,dbeOutput=dbeOutput,spp=spp,taxon=taxon,sex=sex,sampPar=sampPar)

}

})




setMethod("RaiseLgth", signature(dbeOutput="dbeOutput",csObject="csDataCons",clObject="missing"), function(dbeOutput,
                                                                                                              csObject,
                                                                                                              spp,
                                                                                                              taxon,
                                                                                                              sex=as.character(NA),
                                                                                                              sampPar=TRUE,
                                                                                                              incl.precision=TRUE,    ## added MM 26/07/2010
                                                                                                              probs=c(0.025,0.975),
                                                                                                              ...){

if (incl.precision) {  

  obj <- Raise_Lgth(csObject=csObject,dbeOutput=dbeOutput,spp=spp,taxon=taxon,sex=sex,sampPar=sampPar)

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

  return(obj)

} else {
                                                                                                          
  Raise_Lgth(csObject=csObject,dbeOutput=dbeOutput,spp=spp,taxon=taxon,sex=sex,sampPar=sampPar)

}

})






             
