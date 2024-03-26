#' Validity check function for \link{ceDataCons-class} object
#'
#' @description Fishing effort data class validity check 
#'
#' @param object a \link{ceDataCons-class} object
#' @return a boolean (TRUE if the object is a \link{ceDataCons-class} object, FALSE if not)
#'
#' @export
valcecData <- function(object){

	ce <- object@ce

	# I will rely o the prototype to check col names and size. I'm not sure it's a good strategy !
	obj <- new("ceDataCons")
	ce0 <- obj@ce
	
	# check columns
	if(checkNms(ce, names(ce0))==FALSE) stop("Check slot candidate \"ce\" columns' size and names.")
	
	# check PK (ToDo)

	# Everything is fine
	return(TRUE)
}

#' @title ceDataCons-class
#'
#' @description Consolidated fishing effort object
#'
#' @slot desc Character chain of a descriptor
#' @slot ce Dataframe of 10 columns
#'
#' @export
setClass("ceDataCons",
	representation(
		desc="character",
		ce="data.frame"
	),
	prototype(
		desc="my stock",
		ce=data.frame(
			vslFlgCtry=as.factor(NA), # PK
			time=as.factor(NA), # PK
			space=as.factor(NA), # PK 
			technical=as.factor(NA), # PK
			trpNum=as.numeric(NA),
			foNum=as.numeric(NA),
			foDur=as.numeric(NA),
			effKwDays=as.numeric(NA),
			effGtDays=as.numeric(NA),
			daysAtSea=as.numeric(NA)
		)),
	validity=valcecData
)

#' ceDataCons constructors
#' @param object \link{ceDataVal-class} object
#' @param objStrat stratification \link{strIni-class} object
#' @param desc character vector of stock information 
#' @rdname ceDataCons-constructors
#' @docType methods
#' @export
setGeneric("ceDataCons", function(object, objStrat,...){
	standardGeneric("ceDataCons")
	}
)

#' @rdname ceDataCons-constructors
setMethod("ceDataCons", signature("ceDataVal","strIni"), function(object, objStrat, desc="Unknown stock", ...){  

	timeStrata <- objStrat@timeStrata 
	spaceStrata <- objStrat@spaceStrata
	techStrata <- objStrat@techStrata  
	tpRec <- objStrat@tpRec
	spRec <- objStrat@spRec
	tcRec <- objStrat@tcRec
	
	CE <- object@ce
	Semester <- ceiling(CE$quarter/2)      
	CE$month <- paste(as.character(CE$year), as.character(CE$quarter), as.character(CE$month), sep=" - ")
	CE$quarter <- paste(as.character(CE$year), as.character(CE$quarter), sep=" - ")
	CE$semester <- paste(as.character(CE$year), as.character(Semester), sep=" - ")
	
	# Time stratification
	if (is.na(timeStrata)) {
		CE$time <- "all" 
		tpRec <- as.list(NA)
	} else {
		CE$time <- CE[,timeStrata]} 
      
	if(!is.na(tpRec[1])) CE <- recFun(CE,"time",tpRec)      

	# Space stratification
	if (is.na(spaceStrata)) {
		CE$space <- "all" 
		spRec <- as.list(NA)
	} else {
		CE$space <- CE[,spaceStrata]} 
		  
	if (!is.na(spRec[1])) CE <- recFun(CE,"space",spRec)      

	# Technical stratification
	empty <- FALSE
	if (is.na(techStrata)) {
		CE$technical <- "all" 
		tcRec <- as.list(NA)
	} else {
		if (techStrata=="commCat"){
			warning("effort object does not match with market category sampling strategy.\n'commCat' information is unavailable. Output object will be empty!")  
			empty <- TRUE
		} else {
			CE$technical <- CE[,techStrata]}
	}

	if(empty){
		new("ceDataCons",desc=desc)
	} else {
		if (!is.na(tcRec[1])) CE <- recFun(CE,"technical",tcRec)      

 
	# Creation of the CONSOLIDATED object
		csc <- new("ceDataCons")
		ce <- CE[,match(names(csc@ce),names(CE))]
		rownames(ce) <- 1:nrow(ce)  
		new("ceDataCons",desc=desc,ce=coerceCons(ce,csc@ce))
	}
})

#' @rdname ceDataCons-constructors
setMethod("ceDataCons", signature("ceDataVal","missing"), function(object,desc="Unknown stock", ...){
	ceDataCons(object,strIni(),desc=desc,...)
})

#' @rdname ceDataCons-constructors
setMethod("ceDataCons", signature("missing","missing"), function(desc="Unknown stock", ...){
	new("ceDataCons", desc=desc)
})	

setMethod("ce", signature("ceDataCons"), function(object, ...){
	object@ce
	}
)

setMethod("desc", signature("ceDataCons"), function(object, ...){
	object@desc
	}
)

setMethod("head", signature("ceDataCons"), function(x, ...){
  object <- new("ceDataCons",desc=x@desc)
  object@ce <- head(x@ce)
  return(object)  
	}
)

setMethod("summary", signature("ceDataCons"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$ce <- summary(object@ce)
  return(ll)  
	}
)

setMethod("dim", signature("ceDataCons"), function(x){
	return(dim(x@ce))  
})

setMethod("rbind2", signature(x="ceDataCons", y="ceDataCons"), function(x,y){
	df0 <- rbind2(ce(x),ce(y))
	new("ceDataCons", ce=df0)
})

setMethod("subset", signature(x="ceDataCons"), function(x,subset,...){
	e <- substitute(subset)
	df0 <- ce(x)	
	r <- eval(e, df0, parent.frame(n=2))
	new("ceDataCons", desc=x@desc, ce=df0[r,])
})

