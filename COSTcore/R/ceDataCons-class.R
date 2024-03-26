#===================================
#
# EJ, 24/09/2007
# ceData-class
#
#===================================

#====================================================================
# Class definition and validity check
#====================================================================

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

setClass("ceDataCons",
	representation(
		desc="character",
		ce="data.frame"
	),
	prototype(
		desc="my stock",
		ce=data.frame(
			vslFlgCtry=as.factor(NA), # PK
# 			year=as.numeric(NA), # PK	=> time
# 			quarter=as.numeric(NA), # PK	=> time 
# 			month=as.numeric(NA), # PK	=> time
# 			area=as.character(NA), # PK	=> space
# 			rect=as.character(NA), # PK 	=> space
# 			foCatNat=as.character(NA), # PK	=> tech
# 			foCatEu5=as.character(NA), # PK	=> tech
# 			foCatEu6=as.character(NA), # PK	=> tech
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



#====================================================================
# Convert columns of a data.frame with respect to 'refObject' format
#====================================================================


setGeneric("coerceCons", function(object, refObject, ...){
	standardGeneric("coerceCons")
	}
)

setMethod("coerceCons", signature("data.frame", "data.frame"), function(object, refObject, ...){

	if(ncol(object)!=ncol(refObject)) stop("Both objects must have the same number of columns.\n")

	n <- ncol(object)
	for(i in 1:n){
		cls <- class(refObject[,i])
		v <- as.character(object[,i])
		eval(parse('',text=paste("v <- as.",cls,"(v)",sep="")))
		object[,i] <- v
	}
	object	
})



#############
# "strIni"  #   =stratification definition for Cons objects creation
#====================================================================
# Class definition
#====================================================================

setClass("strIni",representation(timeStrata="character",
                                 spaceStrata="character",
                                 techStrata="character",
                                 tpRec="list",
                                 spRec="list",
                                 tcRec="list"),
                    prototype(timeStrata=as.character(NA),
                              spaceStrata=as.character(NA),
                              techStrata=as.character(NA),
                              tpRec=as.list(NA),
                              spRec=as.list(NA),
                              tcRec=as.list(NA)))		

#====================================================================
# Class constructor
#====================================================================

strIni <- function(timeStrata=as.character(NA), spaceStrata=as.character(NA), techStrata=as.character(NA), tpRec=as.list(NA), spRec=as.list(NA), tcRec=as.list(NA)){   

	if (is.na(techStrata)==FALSE & techStrata%in%c("vslLen", "vslPwr", "vslSize")) 
		warning(paste("Check that original '", techStrata, "' field in 'tr' table has been categorized into a moderate number of levels. (see 'cut' function)", sep=""))   

	new("strIni",timeStrata=timeStrata, spaceStrata=spaceStrata, techStrata=techStrata, tpRec=tpRec, spRec=spRec, tcRec=tcRec)                      
}

##====================================================================
## Class constructor
##====================================================================
#setGeneric("ceDataCons", function(object, ...){
#	standardGeneric("ceDataCons")
#	}
#)
#
#setMethod("ceDataCons", signature("ceDataVal"), function(object, ...){
#
#	ce <- ce(object)
#
#	#------------------------------------------------------------------------------
#	# time
#	#------------------------------------------------------------------------------
#	ce$time <- paste(ce$year, paste("Q", ce$quarter, sep=""), sep=".")
#
#	#------------------------------------------------------------------------------
#	# tech
#	#------------------------------------------------------------------------------
#	ce$technical <- apply(ce[,c("foCatNat","foCatEu5","foCatEu6")], 1,paste, collapse=".") 
#	
#	#------------------------------------------------------------------------------
#	# space
#	#------------------------------------------------------------------------------
#	ce$space <- apply(ce[,c("area","rect")], 1,paste, collapse=".") 
#	
#	#------------------------------------------------------------------------------
#	# create csDataCons
#	#------------------------------------------------------------------------------
#	cec <- ceDataCons()
#	ce <- ce[,match(names(ce(cec)),names(ce))]
#	new("ceDataCons", ce=ce)
#})
#
#setMethod("ceDataCons", signature("missing"), function(desc="Unknown stock", ...){
#	new("ceDataCons", desc=desc)
#})

setGeneric("ceDataCons", function(object, objStrat,...){
	standardGeneric("ceDataCons")
	}
)

setMethod("ceDataCons", signature("ceDataVal","strIni"), function(object, objStrat, desc="Unknown stock", ...){  

	timeStrata <- objStrat@timeStrata 
	spaceStrata <- objStrat@spaceStrata
	techStrata <- objStrat@techStrata  
	tpRec <- objStrat@tpRec
	spRec <- objStrat@spRec
	tcRec <- objStrat@tcRec
	
	CE <- object@ce
	Semester <- ceiling(CE$quarter/2)      
                                                                                                  		# addition of quarter in 'month' information 
	CE$month <- paste(as.character(CE$year), as.character(CE$quarter), as.character(CE$month), sep=" - ")
	# addition of year information to "time" field                
	CE$quarter <- paste(as.character(CE$year), as.character(CE$quarter), sep=" - ")
	CE$semester <- paste(as.character(CE$year), as.character(Semester), sep=" - ")
	
	#-------------------------------------------------------------------------------
	# Addition of fields
	#-------------------------------------------------------------------------------

    #-------------------------------------------------------------------------------
    # based on the user specification and the post-stratification specified in strIni
    #-------------------------------------------------------------------------------

	#recoding procedure
	recFun <- function(df,field,rec) {
		Typ <- class(df[,field]) 
		fc <- factor(df[,field]) 
		#Lev <- levels(fc)[!levels(fc)%in%rec$from]
		#df[,field] <- factor(fc,levels=c(Lev,rec$from),labels=c(Lev,rec$to))
		df[,field] <- mapvalues(fc,from=rec$from,to=rec$to)
		eval(parse('',text=paste("df[,field] <- as.",Typ,"(as.character(df[,field]))",sep="")))
		return(df)
	}
  
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

 
	#-------------------------------------------------------------------------------
	# Creation of the CONSOLIDATED object
	#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # selection of the appropriate fields (selection of the new stratification fields instead of the original)
    #---------------------------------------------------------------------------

		csc <- new("ceDataCons")
		ce <- CE[,match(names(csc@ce),names(CE))]
		rownames(ce) <- 1:nrow(ce)  
		new("ceDataCons",desc=desc,ce=coerceCons(ce,csc@ce))
	}
})

setMethod("ceDataCons", signature("ceDataVal","missing"), function(object,desc="Unknown stock", ...){
	ceDataCons(object,strIni(),desc=desc,...)
})

setMethod("ceDataCons", signature("missing","missing"), function(desc="Unknown stock", ...){
	new("ceDataCons", desc=desc)
})	

#====================================================================
# Accessor functions
#====================================================================

setMethod("ce", signature("ceDataCons"), function(object, ...){
	object@ce
	}
)

setMethod("desc", signature("ceDataCons"), function(object, ...){
	object@desc
	}
)

#====================================================================
# 'Head' and 'Tail' functions
#====================================================================

setMethod("head", signature("ceDataCons"), function(x, ...){
  object <- new("ceDataCons",desc=x@desc)
  object@ce <- head(x@ce)
  return(object)  
	}
)

setMethod("tail", signature("ceDataCons"), function(x, ...){
  object <- new("ceDataCons",desc=x@desc)
  object@ce <- tail(x@ce)
  return(object)  
	}
)

#====================================================================
# 'summary' function
#====================================================================

setMethod("summary", signature("ceDataCons"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$ce <- summary(object@ce)
  return(ll)  
	}
)


#====================================================================
# 'dim' function
#====================================================================

setMethod("dim", signature("ceDataCons"), function(x){
	return(dim(x@ce))  
})

#====================================================================
# 'is.' function
#====================================================================

setGeneric("is.ceDataCons", function(object){
	standardGeneric("is.ceDataCons")
})


setMethod("is.ceDataCons","ANY", function(object){
	return(is(object)[1]=="ceDataCons")
})

#====================================================================
# rbind
#====================================================================

setMethod("rbind2", signature(x="ceDataCons", y="ceDataCons"), function(x,y){
	df0 <- rbind2(ce(x),ce(y))
	new("ceDataCons", ce=df0)
})

#====================================================================
# subset
#====================================================================

setMethod("subset", signature(x="ceDataCons"), function(x,subset,...){
	e <- substitute(subset)
	df0 <- ce(x)	
	r <- eval(e, df0, parent.frame(n=2))
	new("ceDataCons", desc=x@desc, ce=df0[r,])
})

