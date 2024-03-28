#' Validity check function for \link{ceData-class} object
#'
#' @description Fishing effort data class validity check 
#'
#' @param object a \link{ceData-class} object
#' @return a boolean (TRUE if the object is a \link{ceData-class} object, FALSE if not)
#'
#' @export
valceData <- function(object){
	ce <- object@ce
	obj <- methods::new("ceData")
	ce0 <- obj@ce
	# check columns
	if(checkNms(ce, names(ce0))==FALSE) stop("Check slot candidate \"ce\" columns' size and names.")
	# check PK
	if(checkpk(ce,"ce")==FALSE) stop("Primary key not unique in slot candidate \"ce\".")
	# check column types
	tys0 <- lapply(ce0,class)
	if(checkTys(ce, tys0)==FALSE) stop("Column types not correct in slot candidate \"ce\".")
	# Everything is fine
	return(TRUE)
}

#' @title ceData-class
#'
#' @description Fishing effort object
#' 
#' @slot desc Character chain of a descriptor
#' @slot ce Dataframe of 19 columns 
#'
#' @export
setClass("ceData",
	representation(
		desc="character",
		ce="data.frame"
	),
	prototype(
		desc="my stock",
		ce=data.frame(
			vslFlgCtry=as.character(NA), # PK
			year=as.numeric(NA), # PK
			quarter=as.numeric(NA), # PK 
			month=as.numeric(NA), # PK
			area=as.character(NA), # PK
			rect=as.character(NA), # PK 
			subRect=as.character(NA), #PK
			foCatNat=as.character(NA), # PK
			foCatEu5=as.character(NA), # PK
			foCatEu6=as.character(NA), # PK
			harbour=as.character(NA), #PK     
			vslLenCat=as.character(NA), #PK   
			trpNum=as.numeric(NA),
			foNum=as.numeric(NA),
			foDur=as.numeric(NA),
			effKwDays=as.numeric(NA),
			effGtDays=as.numeric(NA),
			daysAtSea=as.numeric(NA),
			stringsAsFactors=F)		
	),
	validity=valceData
)

#' ceData constructors
#' @param ce dataframe following the ce slot of a \link{ceData-class} object
#' @param desc descriptor
#' @param missing : create an empty \link{ceData-class} object
#' @param \\dots parameters
#' @rdname ceData-constructors
#' @docType methods
#' @export
setGeneric("ceData", function(ce, ...){
	standardGeneric("ceData")
	}
)

#' @rdname ceData-constructors
setMethod("ceData", signature("data.frame"), function(ce, desc="Unknown stock", ...){
	# create object and name columns properly 
	obj <- methods::new("ceData")
	ce0 <- obj@ce
	names(ce) <- names(ce0)
	# corce columns
	ce <- coerceDataFrameColumns(ce, ce0)
	# object
	methods::new("ceData", ce=ce, desc=desc)
})

#' @rdname ceData-constructors
setMethod("ceData", signature("missing"), function(desc="Unknown stock", ...){
	methods::new("ceData", desc=desc)
})


#' @title ceData-methods 
#' 
#' @description Methods for \link{ceData-class} object
#' @param object,x,y,ceData \link{ceData-class} object
#' @param subset an expression
#' @param \\dots parameters
#' 
#' @docType methods
#' @rdname ceData-methods
#' @export
setGeneric("ce", function(object, ...){
	standardGeneric("ce")
	}
)

#' @rdname ceData-methods
#' @export
setMethod("ce", signature("ceData"), function(object, ...){
	object@ce
	}
)

#' @rdname ceData-methods
#' @export
setMethod("head", signature("ceData"), function(x, ...){
  object <- methods::new("ceData",desc=x@desc)
  object@ce <- head(x@ce)
  return(object)  
	}
)

#' @rdname ceData-methods
#' @export
setMethod("tail", signature("ceData"), function(x, ...){
  object <- methods::new("ceData",desc=x@desc)
  object@ce <- tail(x@ce)
  return(object)  
	}
)

#' @rdname ceData-methods
#' @export
setMethod("summary", signature("ceData"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$ce <- summary(object@ce)
  return(ll)  
	}
)

#' @rdname ceData-methods
#' @export
setMethod("dim", signature("ceData"), function(x){
	return(dim(x@ce))  
})

#' @rdname ceData-methods
#' @export
setMethod("rbind2", signature(x="ceData", y="ceData"), function(x,y){
	df0 <- methods::rbind2(ce(x),ce(y))
	ceData(df0)

})

#' @rdname ceData-methods
#' @export
setMethod("subset", signature(x="ceData"), function(x,subset,...){
	is.Val <- class(x)=="ceDataVal"  
	e <- substitute(subset)
	df0 <- ce(x)	
	r <- eval(e, df0, parent.frame(n=2))
	res <- ceData(df0[r,])
	if (is.Val) res <- ceDataVal(res)
	return(res)
})
