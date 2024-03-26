#===================================
#
# EJ, 24/09/2007
# csData-class
#
#===================================

#====================================================================
# Class definition and validity check
#====================================================================

setClass("csDataVal", contains="csData")

#====================================================================
# Class constructor
#====================================================================
setGeneric("csDataVal", function(object, ...){
	standardGeneric("csDataVal")
	}
)

setMethod("csDataVal", signature("csData"), function(object, ...){
	new("csDataVal", tr=tr(object), hh=hh(object), sl=sl(object), hl=hl(object), ca=ca(object), desc=desc(object))
})

setMethod("csDataVal", signature("missing"), function(desc="Unknown stock", ...){
	new("csDataVal", desc=desc)
})

#====================================================================
# create sampling unit identifiers required for creating csDataCons
#====================================================================

#setGeneric("createSUid", function(object, ...){
#	standardGeneric("createSUid")
#	}
#)
#
#setMethod("createSUid", "csDataVal", function(object, ...){
#
#	tr <- tr(object)
#	tr$PSUid <- 1:nrow(tr)
#	tr <- tr[,c(ncol(tr),1:(ncol(tr)-1))]
#	hh <- hh(object)
#	hh$SSUid <- 1:nrow(hh)
#	hh <- merge(hh, tr[,1:7], sort=FALSE)
#	hh <- hh[,c(ncol(hh), ncol(hh)-1,1:(ncol(hh)-2))]
#	sl <- sl(object)
#	sl$TSUid <- 1:nrow(sl)
#	sl <- merge(sl, hh[,1:9], sort=FALSE)
#	sl <- sl[,c(ncol(sl)-1, ncol(sl), ncol(sl)-2,1:(ncol(sl)-3))]
#	hl <- hl(object)
#	hl <- merge(hl, sl[,1:16], sort=FALSE)
#	hl <- hl[,c((ncol(sl)-2):ncol(sl), 1:(ncol(sl)-3))]
#	ca <- ca(object)
#	ca <- merge(ca, tr[,1:7])
#	ca <- merge(ca, hh[,1:9], sort=FALSE, all.x=TRUE)
#	ca2u <- ca$SSUid
#	mis <- is.na(ca2u)
#	ca2u[mis] <- max(hh$SSUid)+1:sum(mis)
#	ca$SSUid <- ca2u
#	ca <- data.frame(ca[,c(8,ncol(ca))], ca[,-c(8,ncol(ca))])
#	list(tr=tr, hh=hh, sl=sl, hl=hl, ca=ca)
#})
#


