#====================================================================
# Class definition and validity
#====================================================================

valclData <- function(object){

	cl <- object@cl

	# I will rely o the prototype to check col names and size. I'm not sure it's a good strategy !
	obj <- new("clData")
	cl0 <- obj@cl
	
	# check columns
	if(checkNms(cl, names(cl0))==FALSE) stop("Check slot candidate \"cl\" columns' size and names.")
	
	# check PK
	if(checkCLpk(cl)==FALSE) stop("Primary key not unique in slot candidate \"cl\".")

	# check column types
	tys0 <- lapply(cl0,class)
	if(checkTys(cl, tys0)==FALSE) stop("Column types not correct in slot candidate \"cl\".")

	# Everything is fine
	return(TRUE)
}

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
			harbour=as.character(NA), #PK     #modif MM 01/12/2008 (PK)
			vslLenCat=as.character(NA), #PK   #modif MM 01/12/2008
			unallocCatchWt=as.numeric(NA),
			misRepCatchWt=as.numeric(NA),
			landWt=as.numeric(NA),
			landMult=as.numeric(NA),
			landValue=as.numeric(NA),
			stringsAsFactors=F)		
	),
	validity=valclData
)

#====================================================================
# Class constructor
#====================================================================
setGeneric("clData", function(cl, ...){
	standardGeneric("clData")
	}
)

setMethod("clData", signature("data.frame"), function(cl, desc="Unknown stock", ...){
	# create object and name columns properly 
	obj <- new("clData")
	cl0 <- obj@cl
	names(cl) <- names(cl0)
	# corce columns
	cl <- coerceDataFrameColumns(cl, cl0)
	# object
	new("clData", cl=cl, desc=desc)
})

setMethod("clData", signature("matrix"), function(cl, desc="Unknown stock", ...){
	# coerce to dataframe
	cl <- as.data.frame(cl)
	# create object and name columns properly 
	clData(cl=cl, desc=desc)
})

setMethod("clData", signature("missing"), function(desc="Unknown stock", ...){
	new("clData", desc=desc)
})

#====================================================================
# Accessor functions
#====================================================================

setGeneric("cl", function(object, ...){
	standardGeneric("cl")
	}
)

setMethod("cl", signature("clData"), function(object, ...){
	object@cl
	}
)

setMethod("desc", signature("clData"), function(object, ...){
	object@desc
	}
)

#====================================================================
# 'Head' and 'Tail' functions  (setGeneric methods in 'csData-class.R')
#====================================================================


setMethod("head", signature("clData"), function(x, ...){
  object <- new("clData",desc=x@desc)
  object@cl <- head(x@cl)
  return(object)  
	}
)


#====================================================================
# 'summary' function
#====================================================================

setMethod("summary", signature("clData"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$cl <- summary(object@cl)
  return(ll)  
	}
)

#====================================================================
# 'dim' function
#====================================================================

setMethod("dim", signature("clData"), function(x){
	return(dim(x@cl))  
})

#====================================================================
# rbind
#====================================================================

setMethod("rbind2", signature(x="clData", y="clData"), function(x,y){
	df0 <- rbind2(cl(x),cl(y))
	clData(df0)

})

#====================================================================
# subset
#====================================================================

setMethod("subset", signature(x="clData"), function(x,subset,...){

is.Val <- class(x)=="clDataVal"
	e <- substitute(subset)
	df0 <- cl(x)	
	r <- eval(e, df0, parent.frame(n=2))
	res <- clData(df0[r,])
if (is.Val) res <- clDataVal(res)
return(res)
})

