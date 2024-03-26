#===================================
#
# EJ, 24/09/2007, 09/02/2009
# ceData-class
#
#===================================

#====================================================================
# Class definition and validity check
#====================================================================

valceData <- function(object){

	ce <- object@ce

	# I will rely o the prototype to check col names and size. I'm not sure it's a good strategy !
	obj <- new("ceData")
	ce0 <- obj@ce
	
	# check columns
	if(checkNms(ce, names(ce0))==FALSE) stop("Check slot candidate \"ce\" columns' size and names.")
	
	# check PK
	if(checkCEpk(ce)==FALSE) stop("Primary key not unique in slot candidate \"ce\".")

	# check column types
	tys0 <- lapply(ce0,class)
	if(checkTys(ce, tys0)==FALSE) stop("Column types not correct in slot candidate \"ce\".")

	# Everything is fine
	return(TRUE)
}

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
			harbour=as.character(NA), #PK     #modif MM 01/12/2008 (PK)
			vslLenCat=as.character(NA), #PK   #modif MM 01/12/2008
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

#====================================================================
# Class constructor
#====================================================================
setGeneric("ceData", function(ce, ...){
	standardGeneric("ceData")
	}
)

setMethod("ceData", signature("data.frame"), function(ce, desc="Unknown stock", check=FALSE, ...){
	# create object and name columns properly 
	obj <- new("ceData")
	ce0 <- obj@ce
	names(ce) <- names(ce0)
	# corce columns
	ce <- coerceDataFrameColumns(ce, ce0)
	#check
	if (check) check.fields(new("ceData", ce=ce, desc=desc))
	# object
	new("ceData", ce=ce, desc=desc)
})

setMethod("ceData", signature("matrix"), function(ce, desc="Unknown stock", check=FALSE, ...){
	# coerce to data.frame
	ce <- as.data.frame(ce)
	#check
	if (check) check.fields(ceData(ce, desc))
	# create object and name columns properly 
	ceData(ce, desc)
})

setMethod("ceData", signature("missing"), function(desc="Unknown stock", check=FALSE, ...){
	new("ceData", desc=desc)
})

#====================================================================
# IO constructor
#====================================================================

  #addition of a default format using colClasses to keep RECTANGLE and SUB_RECTANGLE as character                                                                      
setMethod("ceData", signature("character"), function(ce, desc="Unknown stock", check=FALSE,...){

	# read CSV files
	# ToDo
	ce <- read.csv(ce,colClasses=c(NA,NA,NA,NA,NA,NA,"character","character",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),...)

	# check names are correct
	checkCEnms(ce)

	# remove record "type" 
	ce <- ce[,-1]

	# create object and name columns properly 

    #check
	if (check) check.fields(ceData(ce=ce, desc=desc))

	ceData(ce=ce, desc=desc)
})


#====================================================================
# Accessor functions
#====================================================================

setGeneric("ce", function(object, ...){
	standardGeneric("ce")
	}
)

setMethod("ce", signature("ceData"), function(object, ...){
	object@ce
	}
)

setGeneric("desc", function(object, ...){
	standardGeneric("desc")
	}
)

setMethod("desc", signature("ceData"), function(object, ...){
	object@desc
	}
)

#====================================================================
# 'Head' and 'Tail' functions
#====================================================================

setMethod("head", signature("ceData"), function(x, ...){
  object <- new("ceData",desc=x@desc)
  object@ce <- head(x@ce)
  return(object)  
	}
)

setMethod("tail", signature("ceData"), function(x, ...){
  object <- new("ceData",desc=x@desc)
  object@ce <- tail(x@ce)
  return(object)  
	}
)

#====================================================================
# 'summary' function
#====================================================================

setMethod("summary", signature("ceData"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$ce <- summary(object@ce)
  return(ll)  
	}
)

#====================================================================
# 'dim' function
#====================================================================

setMethod("dim", signature("ceData"), function(x){
	return(dim(x@ce))  
})

#====================================================================
# rbind2
#====================================================================

setMethod("rbind2", signature(x="ceData", y="ceData"), function(x,y){
	df0 <- rbind2(ce(x),ce(y))
	ceData(df0)

})

#====================================================================
# subset
#====================================================================

setMethod("subset", signature(x="ceData"), function(x,subset,...){
	is.Val <- class(x)=="ceDataVal"  
	e <- substitute(subset)
	df0 <- ce(x)	
	r <- eval(e, df0, parent.frame(n=2))
	res <- ceData(df0[r,])
	if (is.Val) res <- ceDataVal(res)
	return(res)
})

#====================================================================
# replacement
#====================================================================

#setReplaceMethod("[", signature(x="ceData", i="ANY", j="ANY", value="ANY"), function(x, i, j=missing, value){
#	df0 <- ce(x)
#	df0[i,j] <- value
#	ceData(df0)
#})


