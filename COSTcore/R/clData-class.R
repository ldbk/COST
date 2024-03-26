#===================================
#
# EJ, 24/09/2007
# clData-class
#
#===================================

#====================================================================
# Class definition and validity check
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

setMethod("clData", signature("data.frame"), function(cl, desc="Unknown stock", check=FALSE, ...){
	# create object and name columns properly 
	obj <- new("clData")
	cl0 <- obj@cl
	names(cl) <- names(cl0)
	# corce columns
	cl <- coerceDataFrameColumns(cl, cl0)
	#check
	if (check) check.fields(new("clData", cl=cl, desc=desc))
  # object
	new("clData", cl=cl, desc=desc)
})

setMethod("clData", signature("matrix"), function(cl, desc="Unknown stock", check=FALSE, ...){
	# coerce to dataframe
	cl <- as.data.frame(cl)
	
  #check
	if (check) check.fields(clData(cl=cl, desc=desc))
	# create object and name columns properly 
	clData(cl=cl, desc=desc)
})

setMethod("clData", signature("missing"), function(desc="Unknown stock", check=FALSE, ...){
	new("clData", desc=desc)
})

#====================================================================
# IO constructor
#====================================================================

  #addition of a default format using colClasses to keep RECTANGLE and SUB_RECTANGLE as character 
setMethod("clData", signature("character"), function(cl, desc="Unknown stock", check=FALSE,...){

	# read CSV files
	# ToDo
	cl <- read.csv(cl,colClasses=c(NA,NA,NA,NA,NA,NA,NA,"character","character",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),...)

	# check names are correct
	checkCLnms(cl)

	# remove record type 
	cl <- cl[,-1]

  #check
	if (check) check.fields(clData(cl=cl, desc=desc))  
	# create object and name columns properly 
	clData(cl=cl, desc=desc)
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


setMethod("tail", signature("clData"), function(x, ...){
  object <- new("clData",desc=x@desc)
  object@cl <- tail(x@cl)
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
# 'is.' function
#====================================================================

# setGeneric("is.clData", function(object){
# 	standardGeneric("is.clData")
# })


# setMethod("is.clData","ANY", function(object){
# 	return(is(object)[1]=="clData")
# })

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

                                                                               




#====================================================================
# replacement
#====================================================================

#setReplaceMethod("[", signature(x="clData", i="ANY", j="ANY", value="ANY"), function(x, i, j, value){
#	df0 <- cl(x)
#	df0[i,j] <- value
#	clData(df0)
#})

