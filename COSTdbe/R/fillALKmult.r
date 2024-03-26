setGeneric("fillALKmult", function(object,
                                   spp,
                                   p=10,          #number of inviduals to include for filling the missing length class
                                   trace=TRUE,
                                   ...){
	standardGeneric("fillALKmult")}
)

setMethod("fillALKmult", signature(object="csDataCons"),function(object,
                                                                 spp,
                                                                 p=10,
                                                                 trace=TRUE,
                                                                 ...){

require(nnet)
HL <- object@hl
CA <- object@ca
#classes de tailles par strates dans HL toutes fractions comprises
missingLC <- merge(unique(HL[,c("time","space","lenCls")]),unique(CA[,c("time","space","lenCls","fishId")]),all.x=TRUE)
missingLC <- missingLC[is.na(missingLC$fishId),]

if (nrow(missingLC)>0) {

#filling gaps must be done for each stratum
strat <- unique(missingLC[,c("time","space")])

for (i in 1:nrow(strat)) {
  if (trace) print("Stratum :")
  if (trace) print(strat[i,,drop=TRUE])
  CAsub <- merge(strat[i,,drop=FALSE],CA)
  missSub <- merge(strat[i,,drop=FALSE],missingLC )
  if (nrow(CAsub)>1 & length(unique(CAsub$age))>2) {                                                          #MM modif 15/03/2011
    #multinomial model is computed on this dataset
    mult <- multinom(age~lenCls,data=CAsub,trace=trace)
    mlc <- sort(missSub$lenCls)
  
    nbAtAge <- round(p*predict(mult,data.frame(lenCls=mlc),type="probs"))
    if (is.null(nrow(nbAtAge))) nbAtAge <- t(as.matrix(nbAtAge))

    rownames(nbAtAge) <- mlc
    #--> table
    nbAtAgeT <- data.frame(expand.grid(dimnames(nbAtAge)),as.vector(nbAtAge))
    nbAtAgeT2 <- data.frame(lenCls=rep(nbAtAgeT[,1],nbAtAgeT[,3]), age=rep(nbAtAgeT[,2],nbAtAgeT[,3]))
    minId <- min(c(0,CA$fishId),na.rm=TRUE) ; nbAtAgeT2$fishId <- minId-1:nrow(nbAtAgeT2)
    root <- CAsub[1,] ; root$sex <- root$otoWt <- root$otoSide <- root$indWt <- root$matMeth <- root$matScale <- root$matStage <- NA
    virtual <- do.call("rbind",lapply(1:nrow(nbAtAgeT2),function(x) return(root)))
    virtual[,c("lenCls","age","fishId")] <- nbAtAgeT2
    virtual$lenCls <- as.numeric(as.character(virtual$lenCls)) ; virtual$age <- as.numeric(as.character(virtual$age))
  
    CA <- rbind(CA,virtual)  
  }                                                                             #MM modif 15/03/2011

 }
}

object@ca <- CA
return(object)

})