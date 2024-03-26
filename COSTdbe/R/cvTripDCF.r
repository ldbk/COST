setGeneric("cvTripDCF", function(csObject,
                                 spp,
                                 catchCat="LAN",
                                 sampPar=TRUE,
                                 ...){
	standardGeneric("cvTripDCF")}
)

setMethod("cvTripDCF", signature(csObject="csDataCons"), 
                        function(csObject,
                                 spp,
                                 catchCat="LAN",
                                 sampPar=TRUE,
                                 ...){


ccat <- toupper(catchCat)                               
csObject@sl$sort <- toupper(csObject@sl$sort)                                
csObject@hl$sort <- toupper(csObject@hl$sort)                                   
csObject@ca$sort <- toupper(csObject@ca$sort)                                   

eval(parse('',text=paste("csObject <- subsetSpp(csObject,spp%in%",deparse(spp),")",sep="")))

if (!all(is.na(ccat)) & (all(toupper(ccat)%in%c("LAN","DIS")))) {
  csObject@sl <- csObject@sl[extCatchCat(csObject@sl$sort)%in%ccat,]
  csObject@hl <- csObject@hl[extCatchCat(csObject@hl$sort)%in%ccat,]
} else {
stop("wrong 'catchCat' slot in dbe object!")}

if (nrow(csObject@hl)==0) stop("no sampled landings for specified species!!")

#as sort is a factor,...                                                        
csObject@sl$sort <- as.character(csObject@sl$sort)                             
csObject@hl$sort <- as.character(csObject@hl$sort)                              
#in TSUid field, all values are considered (NAs)
csObject@sl$TSUid <- factor(as.character(csObject@sl$TSUid),exclude=NULL)
csObject@hl$TSUid <- factor(as.character(csObject@hl$TSUid),exclude=NULL)

llSpp <- list() ; j <- 1

for (i in spp) {

ind <- sampledFO(csObject,species=i,fraction=ccat,sampPar=sampPar)$sampLg
Hl <- cbind(csObject@hh[,c("PSUid","SSUid","time","space","technical")],ind=ind)

#on insere les indicateurs d'echantillonnage
IndSL <- merge(csObject@sl,Hl,all.x=TRUE,sort=FALSE)                            
csObject@sl$wt <- csObject@sl$wt*IndSL$ind                                      
csObject@sl$subSampWt <- csObject@sl$subSampWt*IndSL$ind  
                      

#some calculations must be made BEFORE subsetting

SL_STR <- apply(csObject@sl[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))


#weight of the level
wl <- catApply(csObject@sl$wt,list(STR=SL_STR,sort=csObject@sl$sort,TSUid=csObject@sl$TSUid,SSUid=csObject@sl$SSUid,PSUid=csObject@sl$PSUid),sum,na.rm=TRUE)  #reference for factor levels

#sampled weight
ws <- catApply(csObject@sl$subSampWt,list(STR=SL_STR,sort=csObject@sl$sort,TSUid=csObject@sl$TSUid,SSUid=csObject@sl$SSUid,PSUid=csObject@sl$PSUid),sum,na.rm=TRUE)


SL <- csObject@sl                            
HL <- csObject@hl                         
#creating STR field, concatenation of time, space, technical
HHSTR <- apply(csObject@hh[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))
SL$STR <- apply(SL[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))
HL$STR <- apply(HL[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))
Hl$STR <- apply(Hl[,c("time","space","technical")],1,function(x) paste(x,collapse=":-:"))

if (nrow(SL)==0) stop("the specified parameters resulted in an empty table!!")

wt  <- catApply(SL$subSampWt,list(as.character(SL$STR),as.character(SL$sort),as.character(SL$TSUid),as.character(SL$SSUid),
                                  as.character(SL$PSUid)),sum,na.rm=TRUE) 
wt <- dbeReplic(wt,wl$ind)   

d_j  <- catApply(HL$lenNum,list(as.character(HL$STR),as.character(HL$sort),as.character(HL$TSUid),as.character(HL$SSUid),
                                  as.character(HL$PSUid),HL$lenCls),sum,na.rm=TRUE)    

#TSUid stage
w_tsu <- dbeAgg(list(val = wt$val * wl$val / ws$val, ind = wl$ind), c(1,3:5),sum,na.rm=TRUE)
  
  ##wl_tsu <- RowSum(wl,c(1,3:5)) <------
wl_tsu <- dbeAgg(wl,c(1,3:5),sum,na.rm=TRUE)

#SSUid stage
expr <- list(val = d_j$val * dbeReplic(list(val = wl$val/ws$val, ind = wl$ind), d_j$ind[1:5,])$val , ind = d_j$ind)

  ##sum.d_jssu <- RowSum(expr,c(1,5,6)) <------
sum.d_jssu <- dbeAgg(expr,c(1,5,6),sum,na.rm=TRUE)

  ##sum.w_ssu <- RowSum(w_tsu,c(1,4)) <------
sum.w_ssu <- dbeAgg(w_tsu,c(1,4),sum,na.rm=TRUE)

  ##sum.wl_ssu <- RowSum(wl_tsu,c(1,4)) <------
sum.wl_ssu <- dbeAgg(wl_tsu,c(1,4),sum,na.rm=TRUE)

#number of SSUid (total and sampled)
Mi <- catApply(csObject@hh$SSUid,list(as.character(HHSTR),as.character(csObject@hh$PSUid)),function(x) length(unique(x)))
Mi_a <- dbeReplic(Mi,sum.d_jssu$ind[1:2,])  #on met a la dimension de 'sum.d_jssu'
Mi_b <- dbeReplic(Mi,sum.w_ssu$ind[1:2,])  #on met a la dimension de 'sum.w_ssu'

Hl <- Hl[!is.na(Hl$ind),]

mi <- catApply(Hl$ind,list(STR=as.character(Hl$STR),as.character(Hl$PSUid)),length)
mi_a <- dbeReplic(mi,sum.d_jssu$ind[1:2,])
mi_b <- dbeReplic(mi,sum.w_ssu$ind[1:2,])

#PSUid stage
  ##d_jpsu <- sum.d_jssu*Mi/mi <------
d_jpsu <- list(val = sum.d_jssu$val * Mi_a$val / mi_a$val, ind = sum.d_jssu$ind)

  ##w_psu <- sum.w_ssu*Mi/mi <------
w_psu <- list(val = sum.w_ssu$val * Mi_b$val / mi_b$val, ind = sum.w_ssu$ind)

  ##wl_psu <- sum.wl_ssu*Mi/mi <------
wl_psu <- list(val = sum.wl_ssu$val * Mi_b$val / mi_b$val, ind = sum.wl_ssu$ind)


sum.d_jpsu <- dbeAgg(d_jpsu,c(1,3),sum,na.rm=TRUE)
                                  
#on doit maintenant penser a inserer les 0-values, cad les combinaisons strate/psuid/length non echantillonnees, 
# mais dont l'occurence strate/length a ete echantillonnee, dans d_jpsu (croisement des en-têtes de 'sum.d_jpsu' et de 'sum.w_ssu')

  #on commence par croiser les deux paires de dimensions (STR/PSUid et STR/Lgth)             #####
dfTemp <- as.data.frame(t(sum.d_jpsu$ind)) ; names(dfTemp) <- c("V1","V3")                       #
cross <- merge(as.data.frame(t(sum.w_ssu$ind)),dfTemp,all=TRUE)                                  #
  #ensuite, on etend le vecteur input selon cette nouvelle matrice                               #
d_jpsu.new <- dbeReplic(d_jpsu,as.matrix(t(cross)))                                              #
  #il ne reste plus qu'a considerer les 0-values                                                 #
d_jpsu.new$val[is.na(d_jpsu.new$val)] <- 0                                                   #####




#results are formatted
#####################
  
df.D_j <- as.data.frame(cbind(do.call("rbind",lapply(names(d_jpsu.new$val),function(x) strsplit(x,":@&@&@:")[[1]]))))
df.D_j$value <- d_jpsu.new$val

df.N <- with(df.D_j,aggregate(value,list(V1=V1,V2=V2),sum,na.rm=TRUE))

df.W <- as.data.frame(cbind(do.call("rbind",lapply(names(w_psu$val),function(x) strsplit(x,":@&@&@:")[[1]]))))
df.W$value <- w_psu$val

#CVs are calculated
df.D_jMean <- with(df.D_j,aggregate(value,list(V1=V1,V3=V3),mean,na.rm=TRUE)) ; names(df.D_jMean)[ncol(df.D_jMean)] <- "mean"
df.D_jSd <- with(df.D_j,aggregate(value,list(V1=V1,V3=V3),sd,na.rm=TRUE)) ;  names(df.D_jSd)[ncol(df.D_jSd)] <- "sd"
tabD <- merge(df.D_jMean,df.D_jSd) ; tabD$CV <- tabD$sd/tabD$mean

#
df.NMean <- with(df.N,aggregate(x,list(V1=V1),mean,na.rm=TRUE)) ; names(df.NMean)[ncol(df.NMean)] <- "mean"
df.NSd <- with(df.N,aggregate(x,list(V1=V1),sd,na.rm=TRUE)) ;  names(df.NSd)[ncol(df.NSd)] <- "sd"
tabN <- merge(df.NMean,df.NSd) ; tabN$CV <- tabN$sd/tabN$mean

#
df.WMean <- with(df.W,aggregate(value,list(V1=V1),mean,na.rm=TRUE)) ; names(df.WMean)[ncol(df.WMean)] <- "mean"
df.WSd <- with(df.W,aggregate(value,list(V1=V1),sd,na.rm=TRUE)) ;  names(df.WSd)[ncol(df.WSd)] <- "sd"
tabW <- merge(df.WMean,df.WSd) ; tabW$CV <- tabW$sd/tabW$mean

#
df.DCF <- with(tabD,aggregate(mean,list(V1=V1),sum,na.rm=TRUE))
df.DCF2 <- with(tabD,aggregate(mean*CV,list(V1=V1),sum,na.rm=TRUE))
df.DCF$x <- df.DCF2$x/df.DCF$x 



  #formatted tables
tabD <- cbind(do.call("rbind",lapply(as.character(tabD[,1]),function(x) strsplit(x,":-:")[[1]])),tabD[,2:ncol(tabD)])
names(tabD)[1:4] <- c("time","space","technical","length")

tabN <- cbind(do.call("rbind",lapply(as.character(tabN[,1]),function(x) strsplit(x,":-:")[[1]])),tabN[,2:ncol(tabN)])
names(tabN)[1:3] <- c("time","space","technical")

tabW <- cbind(do.call("rbind",lapply(as.character(tabW[,1]),function(x) strsplit(x,":-:")[[1]])),tabW[,2:ncol(tabW)])
names(tabW)[1:3] <- c("time","space","technical")

tabDCF <- cbind(do.call("rbind",lapply(as.character(df.DCF[,1]),function(x) strsplit(x,":-:")[[1]])),df.DCF[,2:ncol(df.DCF),drop=FALSE])
names(tabDCF) <- c("time","space","technical","cvDCF")

tabNbTrip <- with(Hl,aggregate(PSUid,list(time=time,space=space,technical=technical),function(x) length(unique(x))))
names(tabNbTrip)[ncol(tabNbTrip)] <- "nbTrips"

llSpp[[j]] <- list(nbT=tabNbTrip,D=tabD,N=tabN,W=tabW,DCF=tabDCF) ; j <- j+1
}


names(llSpp) <- spp

return(llSpp)


}
)

##example
#data(sole)
##stratification
#strD <- strIni(timeStrata="quarter",techStrata="commCat")
##only market sampling data and biological parameters are kept
#csObject <- csDataCons(csDataVal(subset(sole.cs,sampType%in%c("M","V"))),strD)
#
#gg <- cvTripDCF(csObject,"Solea solea")
#
