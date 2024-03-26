#===================================
#
# EJ, 24/09/2007
# clData-class
#
#===================================

#====================================================================
# Class definition and validity check
#====================================================================

valclcData <- function(object){

	cl <- object@cl

	# I will rely o the prototype to check col names and size. I'm not sure it's a good strategy !
	obj <- new("clDataCons")
	cl0 <- obj@cl
	
	# check columns
	if(checkNms(cl, names(cl0))==FALSE) stop("Check slot candidate \"ce\" columns' size and names.")
	
	# check PK (ToDo)

	# Everything is fine
	return(TRUE)
}

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

#====================================================================
# Class constructor
#====================================================================
setGeneric("clDataCons", function(object,objStrat,...){
	standardGeneric("clDataCons")
	}
)

#setMethod("clDataCons", signature("clDataVal"), function(object, ...){
#
#	cl <- cl(object)
#
#	#------------------------------------------------------------------------------
#	# time
#	#------------------------------------------------------------------------------
#	cl$time <- paste(cl$year, paste("Q", cl$quarter, sep=""), sep=".")
#
#	#------------------------------------------------------------------------------
#	# tech
#	#------------------------------------------------------------------------------
#	cl$technical <- apply(cl[,c("foCatNat","foCatEu5","foCatEu6")], 1,paste, collapse=".") 
#	
#	#------------------------------------------------------------------------------
#	# space
#	#------------------------------------------------------------------------------
#	cl$space <- apply(cl[,c("area","rect")], 1,paste, collapse=".") 
#	
#	#------------------------------------------------------------------------------
#	# create csDataCons
#	#------------------------------------------------------------------------------
#	clc <- clDataCons()
#	cl <- cl[,match(names(cl(clc)),names(cl))]
#	new("clDataCons", cl=cl)
#})
#
#setMethod("clDataCons", signature("missing"), function(desc="Unknown stock", ...){
#	new("clDataCons", desc=desc)
#})


setMethod("clDataCons", signature("clDataVal","strIni"), function(object,
                                                                  objStrat,
                                                                  desc="Unknown stock",
                                                                  ...){  

timeStrata <- objStrat@timeStrata                 # <<<- to make the code clearer, but maybe it's not the thing to do
spaceStrata <- objStrat@spaceStrata               #
techStrata <- objStrat@techStrata                 #
tpRec <- objStrat@tpRec                           #
spRec <- objStrat@spRec                           #
tcRec <- objStrat@tcRec                           #
                                                  #
CL <- object@cl                                   ####
Semester <- ceiling(CL$quarter/2)      
                                                                                                  #<<- 22/09/2008 update : addition of quarter in 'month' information 
CL$month <- paste(as.character(CL$year),as.character(CL$quarter),as.character(CL$month),sep=" - ")#<<- 22/07/2008 update : addition of year information to "time" field                
CL$quarter <- paste(as.character(CL$year),as.character(CL$quarter),sep=" - ")                     #
CL$semester <- paste(as.character(CL$year),as.character(Semester),sep=" - ")                      #

#-------------------------------------------------------------------------------
# Addition of fields
#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # based on the user specification and the post-stratification specified in the strIni object
    #---------------------------------------------------------------------------

#recoding procedure
recFun <- function(df,field,rec) {                  # <<<- there's surely a more simple way to do this
  Typ <- class(df[,field]) 
  fc <- factor(df[,field]) 
  #Lev <- levels(fc)[!levels(fc)%in%rec$from]
  #df[,field] <- factor(fc,levels=c(Lev,rec$from),labels=c(Lev,rec$to))
  df[,field] <- mapvalues(fc,from=rec$from,to=rec$to)
  eval(parse('',text=paste("df[,field] <- as.",Typ,"(as.character(df[,field]))",sep="")))
  return(df)
}
  
        #-------
        # Time stratification
        #-------

if (is.na(timeStrata)) {
  CL$time <- "all"
  tpRec <- as.list(NA)
} else {
  CL$time <- CL[,timeStrata]}    

if (!is.na(tpRec[1])) CL <- recFun(CL,"time",tpRec)    
   
        #-------
        # Space stratification
        #-------

if (is.na(spaceStrata)) {
  CL$space <- "all"
  spRec <- as.list(NA)
} else {
  CL$space <- CL[,spaceStrata]}    

if (!is.na(spRec[1])) CL <- recFun(CL,"space",spRec)    
        
        #-------
        # Technical stratification
        #-------

if (is.na(techStrata)) {
  CL$technical <- "all"
  tcRec <- as.list(NA)
} else {
  CL$technical <- CL[,techStrata]}    

if (!is.na(tcRec[1])) CL <- recFun(CL,"technical",tcRec)    
   
              
#-------------------------------------------------------------------------------
# Creation of the CONSOLIDATED object
#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # selection of the appropriate fields (selection of the new stratification fields instead of the original)
    #---------------------------------------------------------------------------

csc <- new("clDataCons")
cl <- CL[,match(names(csc@cl),names(CL))]
rownames(cl) <- 1:nrow(cl)  
new("clDataCons", desc=desc,cl=coerceCons(cl,csc@cl))
})
	





setMethod("clDataCons", signature("clDataVal","missing"), function(object,desc="Unknown stock", ...){

	clDataCons(object,strIni(),desc=desc,...)
})





setMethod("clDataCons", signature("missing","missing"), function(desc="Unknown stock", ...){

	new("clDataCons", desc=desc)
})	







#====================================================================
# Accessor functions
#====================================================================

setMethod("cl", signature("clDataCons"), function(object, ...){
	object@cl
	}
)

setMethod("desc", signature("clDataCons"), function(object, ...){
	object@desc
	}
)

#====================================================================
# 'Head' and 'Tail' functions  (setGeneric methods in 'csData-class.R')
#====================================================================


setMethod("head", signature("clDataCons"), function(x, ...){
  object <- new("clDataCons",desc=x@desc)
  object@cl <- head(x@cl)
  return(object)  
	}
)


setMethod("tail", signature("clDataCons"), function(x, ...){
  object <- new("clDataCons",desc=x@desc)
  object@cl <- tail(x@cl)
  return(object)  
	}
)

#====================================================================
# 'summary' function
#====================================================================

setMethod("summary", signature("clDataCons"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$cl <- summary(object@cl)
  return(ll)  
	}
)

#====================================================================
# 'dim' function
#====================================================================

setMethod("dim", signature("clDataCons"), function(x){
	return(dim(x@cl))  
})

#====================================================================
# 'is.' function
#====================================================================

setGeneric("is.clDataCons", function(object){
	standardGeneric("is.clDataCons")
})


setMethod("is.clDataCons","ANY", function(object){
	return(is(object)[1]=="clDataCons")
})

#====================================================================
# rbind
#====================================================================

setMethod("rbind2", signature(x="clDataCons", y="clDataCons"), function(x,y){
	df0 <- rbind2(cl(x),cl(y))
	new("clDataCons", cl=df0)
})

#====================================================================
# subset
#====================================================================

setMethod("subset", signature(x="clDataCons"), function(x,subset,...){
	e <- substitute(subset)
	df0 <- cl(x)	
	r <- eval(e, df0, parent.frame(n=2))
	ff <- new("clDataCons", desc=x@desc, cl=df0[r,])
})

