#' Validity check function for \link{clData-class} object
#'
#' @description Landing data class validity check
#'
#' @param object a \link{clData-class} object
#' @return a boolean (TRUE if the object is a \link{clData-class} object, FALSE if not)
#'
#' @export
valclData <- function(object){
	cl <- object@cl
	obj <- methods::new("clData")
	cl0 <- obj@cl
	# check columns
	if(checkNms(cl, names(cl0))==FALSE) stop("Check slot candidate \"cl\" columns' size and names.")
	# check PK
	if(checkpk(cl,"cl")==FALSE) stop("Primary key not unique in slot candidate \"cl\".")
	# check column types
	tys0 <- lapply(cl0,class)
	if(checkTys(cl, tys0)==FALSE) stop("Column types not correct in slot candidate \"cl\".")
	# Everything is fine
	return(TRUE)
}

#' @title clData-class
#'
#' @description Landing object
#'
#' @slot desc Character chain of a descriptor
#' @slot cl Dataframe of  columns
#'
#' @export
setClass("clData",
	representation(
		desc="character",
		cl="data.frame"
	),
	prototype(
		desc="my stock",
		cl=data.frame(
			landCtry=as.character(NA), # PK
			vslFlgCtry=as.character(NA), # PK
			year=as.numeric(NA), # PK
			quarter=as.numeric(NA), # PK 
			month=as.numeric(NA), # PK
			area=as.character(NA), # PK
			rect=as.character(NA), # PK 
			subRect=as.character(NA), #PK
			taxon=as.character(NA), # PK 
			landCat=as.character(NA), # PK 
			commCatScl=as.character(NA), # PK
			commCat=as.character(NA), # PK
			foCatNat=as.character(NA), # PK
			foCatEu5=as.character(NA), # PK
			foCatEu6=as.character(NA), # PK
			harbour=as.character(NA), #PK     
			vslLenCat=as.character(NA), #PK   
			unallocCatchWt=as.numeric(NA),
			misRepCatchWt=as.numeric(NA),
			landWt=as.numeric(NA),
			landMult=as.numeric(NA),
			landValue=as.numeric(NA),
			stringsAsFactors=F)		
	),
	validity=valclData
)

#' clData constructors
#' @param cl dataframe following the cl slot of a \link{clData-class} object
#' @param desc descriptor
#' @param missing : create an empty \link{clData-class} object
#' @param \\dots parameters
#' @rdname clData-constructors
#' @docType methods
#' @export
setGeneric("clData", function(cl, ...){
	standardGeneric("clData")
	}
)

#' @rdname clData-constructors
setMethod("clData", signature("data.frame"), function(cl, desc="Unknown stock", ...){
	# create object and name columns properly 
	obj <- methods::new("clData")
	cl0 <- obj@cl
	names(cl) <- names(cl0)
	# corce columns
	cl <- coerceDataFrameColumns(cl, cl0)
	# object
	methods::new("clData", cl=cl, desc=desc)
})

#' @rdname clData-constructors
setMethod("clData", signature("missing"), function(desc="Unknown stock", ...){
	methods::new("clData", desc=desc)
})

#' @title clData-methods 
#' 
#' @description Methods for \link{clData-class} object
#' @param object,x,y,clData \link{clData-class} object
#' @param subset an expression
#' @param \\dots parameters
#' 
#' @docType methods
#' @rdname clData-methods
#' @export
setGeneric("cl", function(object, ...){
	standardGeneric("cl")
	}
)

#' @rdname clData-methods
setMethod("cl", signature("clData"), function(object, ...){
	object@cl
	}
)

#' @rdname clData-methods
#' @export
setMethod("head", signature("clData"), function(x, ...){
  object <- methods::new("clData",desc=x@desc)
  object@cl <- head(x@cl)
  return(object)  
	}
)

#' @rdname clData-methods
#' @export
setMethod("summary", signature("clData"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$cl <- summary(object@cl)
  return(ll)  
	}
)

#' @rdname clData-methods
setMethod("dim", signature("clData"), function(x){
	return(dim(x@cl))  
})

#' @rdname clData-methods
setMethod("rbind2", signature(x="clData", y="clData"), function(x,y){
	df0 <- methods::rbind2(cl(x),cl(y))
	clData(df0)

})

#' @rdname clData-methods
setMethod("subset", signature(x="clData"), function(x,subset,...){
		  is.Val <- class(x)=="clDataVal"
		  e <- substitute(subset)
		  df0 <- cl(x)	
		  r <- eval(e, df0, parent.frame(n=2))
		  res <- clData(df0[r,])
		  if (is.Val) res <- clDataVal(res)
		  return(res)
})
