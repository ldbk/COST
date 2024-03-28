#' Validity check function for \link{clDataCons-class} object
#'
#' @description Landing data class validity check 
#'
#' @param object a \link{clDataCons-class} object
#' @return a boolean (TRUE if the object is a \link{clDataCons-class} object, FALSE if not)
#'
#' @export
valclcData <- function(object){
	cl <- object@cl
	obj <- methods::new("clDataCons")
	cl0 <- obj@cl
	# check columns
	if(checkNms(cl, names(cl0))==FALSE) stop("Check slot candidate \"ce\" columns' size and names.")
	# Everything is fine
	return(TRUE)
}

#' @title clDataCons-class
#'
#' @description Consolidated landing object
#'
#' @slot desc Character chain of a descriptor
#' @slot cl Dataframe of xx columns
#'
#' @export
setClass("clDataCons", 
	representation(
		desc="character",
		cl="data.frame"
	),
	prototype(
		desc="my stock",
		cl=data.frame(
			landCtry=as.factor(NA), # PK
			vslFlgCtry=as.factor(NA), # PK
			time=as.factor(NA), # PK
			space=as.factor(NA), # PK 
			technical=as.factor(NA), # PK
			taxon=as.factor(NA), # PK 
			landCat=as.character(NA), # PK 
			commCatScl=as.character(NA), # PK
			commCat=as.character(NA), # PK
			unallocCatchWt=as.numeric(NA),
			misRepCatchWt=as.numeric(NA),
			landWt=as.numeric(NA),
			landMult=as.numeric(NA),
			landValue=as.numeric(NA))		
	),
	validity=valclcData
)

#' clDataCons constructors
#' @param object \link{clDataVal-class} object
#' @param objStrat stratification \link{strIni-class} object
#' @param desc character vector of stock information
#' @param \\dots parameters
#' @rdname clDataCons-constructors
#' @docType methods
#' @export
setGeneric("clDataCons", function(object,objStrat,...){
	standardGeneric("clDataCons")
	}
)

#' @rdname clDataCons-constructors
setMethod("clDataCons", signature("clDataVal","strIni"), function(object,
                                                                  objStrat,
                                                                  desc="Unknown stock",
                                                                  ...){  
timeStrata <- objStrat@timeStrata                  
spaceStrata <- objStrat@spaceStrata               
techStrata <- objStrat@techStrata                 
tpRec <- objStrat@tpRec                           
spRec <- objStrat@spRec                           
tcRec <- objStrat@tcRec                           
                                                  
CL <- object@cl                                   
Semester <- ceiling(CL$quarter/2)      
CL$month <- paste(as.character(CL$year),as.character(CL$quarter),as.character(CL$month),sep=" - ")
CL$quarter <- paste(as.character(CL$year),as.character(CL$quarter),sep=" - ")
CL$semester <- paste(as.character(CL$year),as.character(Semester),sep=" - ")

# Time stratification
if (is.na(timeStrata)) {
  CL$time <- "all"
  tpRec <- as.list(NA)
} else {
  CL$time <- CL[,timeStrata]}    

if (!is.na(tpRec[1])) CL <- recFun(CL,"time",tpRec)    
   
# Space stratification
if (is.na(spaceStrata)) {
  CL$space <- "all"
  spRec <- as.list(NA)
} else {
  CL$space <- CL[,spaceStrata]}    

if (!is.na(spRec[1])) CL <- recFun(CL,"space",spRec)    
        
# Technical stratification

if (is.na(techStrata)) {
  CL$technical <- "all"
  tcRec <- as.list(NA)
} else {
  CL$technical <- CL[,techStrata]}    

if (!is.na(tcRec[1])) CL <- recFun(CL,"technical",tcRec)    
   
# Creation of the CONSOLIDATED object

csc <- methods::new("clDataCons")
cl <- CL[,match(names(csc@cl),names(CL))]
rownames(cl) <- 1:nrow(cl)  
methods::new("clDataCons", desc=desc,cl=coerceCons(cl,csc@cl))
})
	
#' @rdname clDataCons-constructors
setMethod("clDataCons", signature("clDataVal","missing"), function(object,desc="Unknown stock", ...){
  clDataCons(object,strIni(),desc=desc,...)
})

#' @rdname clDataCons-constructors
setMethod("clDataCons", signature("missing","missing"), function(desc="Unknown stock", ...){
  methods::new("clDataCons", desc=desc)
})	

#' clDataCons methods
#' @param object,x,y \link{clDataCons-class} object
#' @param subset subset expression
#' @param \\dots parameters
#' @rdname clDataCons-methods
#' @docType methods
#' @export
setMethod("cl", signature("clDataCons"), function(object, ...){
	object@cl
	}
)

#' @rdname clDataCons-methods
setMethod("head", signature("clDataCons"), function(x, ...){
  object <- methods::new("clDataCons",desc=x@desc)
  object@cl <- head(x@cl)
  return(object)  
	}
)

#' @rdname clDataCons-methods
setMethod("summary", signature("clDataCons"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$cl <- summary(object@cl)
  return(ll)  
	}
)

#' @rdname clDataCons-methods
setMethod("dim", signature("clDataCons"), function(x){
	return(dim(x@cl))  
})

#' @rdname clDataCons-methods
setMethod("rbind2", signature(x="clDataCons", y="clDataCons"), function(x,y){
	df0 <- methods::rbind2(cl(x),cl(y))
	methods::new("clDataCons", cl=df0)
})

#' @rdname clDataCons-methods
setMethod("subset", signature(x="clDataCons"), function(x,subset,...){
	e <- substitute(subset)
	df0 <- cl(x)	
	r <- eval(e, df0, parent.frame(n=2))
	ff <- methods::new("clDataCons", desc=x@desc, cl=df0[r,])
})
