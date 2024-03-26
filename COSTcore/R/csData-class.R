#====================================================================
#
# EJ, 24/09/2007
# csData-class
#
# Data hierarchy
# tr --> hh --> sl --> hl
# tr --> ca
#  
#====================================================================

#====================================================================
# Abbreviates
#
# samp = sampling
# country = ctry
# flag =flg 
# vessel = vsl
# trip = trp
# number = num
# project = proj
# power = pwr
# fishing operation = fo
# method = meth
# depth = dep
# selectivity =sel
# device =dev
# commercial = comm
# weight = wt
# class = cls
# otolith = oto
#====================================================================

#====================================================================
# Class definition and validity check
#====================================================================

valcsData <- function(object){
	tr <- object@tr
	hh <- object@hh
	sl <- object@sl
	hl <- object@hl
	ca <- object@ca

	# I will rely o the prototype to check col names and size. I'm not sure it's a good strategy !
	obj <- new("csData")
	tr0 <- obj@tr
	hh0 <- obj@hh
	sl0 <- obj@sl
	hl0 <- obj@hl
	ca0 <- obj@ca
	
	# check columns
	if(checkNms(tr, names(tr0))==FALSE) stop("Check slot candidate \"tr\" columns' size and names.")
	if(checkNms(hh, names(hh0))==FALSE) stop("Check slot candidate \"hh\" columns' size and names.")
	if(checkNms(sl, names(sl0))==FALSE) stop("Check slot candidate \"sl\" columns' size and names.")
	if(checkNms(hl, names(hl0))==FALSE) stop("Check slot candidate \"hl\" columns' size and names.")
	if(checkNms(ca, names(ca0))==FALSE) stop("Check slot candidate \"ca\" columns' size and names.")
	
	# check PK
	if(checkTRpk(tr)==FALSE) stop("Primary key not unique in slot candidate \"tr\".")
	if(checkHHpk(hh)==FALSE) stop("Primary key not unique in slot candidate \"hh\".")
	if(checkSLpk(sl)==FALSE) stop("Primary key not unique in slot candidate \"sl\".")
	if(checkHLpk(hl)==FALSE) stop("Primary key not unique in slot candidate \"hl\".")
	if(checkCApk(ca)==FALSE) stop("Primary key not unique in slot candidate \"ca\".")

	# check column types
	tys0 <- lapply(tr0,class)
	if(checkTys(tr, tys0)==FALSE) stop("Column types not correct in slot candidate \"tr\".")
	tys0 <- lapply(hh0,class)
	if(checkTys(hh, tys0)==FALSE) stop("Column types not correct in slot candidate \"hh\".")
	tys0 <- lapply(sl0,class)
	if(checkTys(sl, tys0)==FALSE) stop("Column types not correct in slot candidate \"sl\".")
	tys0 <- lapply(hl0,class)
	if(checkTys(hl, tys0)==FALSE) stop("Column types not correct in slot candidate \"hl\".")
	tys0 <- lapply(ca0,class)
	if(checkTys(ca, tys0)==FALSE) stop("Column types not correct in slot candidate \"ca\".")

	# check data integrity
	if(checkDataIntegrity(tr[,1:6], hh[,1:6])==FALSE) stop("Data integrity problem in table \"hh\". Missing related records in \"tr\".")
	if(checkDataIntegrity(hh[,1:7], sl[,1:7])==FALSE) stop("Data integrity problem in table \"sl\". Missing related records in \"hh\".")
	if(checkDataIntegrity(sl[,1:14], slSex(sl,hl)[,1:14])==FALSE) stop("Data integrity problem in table \"hl\". Missing related records in \"sl\".")   #modif 08/12/08 'Sex' field not always a key field in SL
	if(checkDataIntegrity(tr[,1:6], ca[,1:6])==FALSE) stop("Data integrity problem in table \"ca\". Missing related records in \"tr\".")

	# Everything is fine
	return(TRUE)
}

setClass("csData",
	representation(
		desc="character",
		tr="data.frame",
		hh="data.frame",
		sl="data.frame",
		hl="data.frame",
		ca="data.frame"
	),
	prototype(
		desc="my stock",
		tr=data.frame(
			sampType=as.character(NA), # PK
			landCtry=as.character(NA), # PK
			vslFlgCtry=as.character(NA), # PK
			year=as.numeric(NA), # PK
			proj=as.character(NA), # PK
			trpCode=as.character(NA), # PK
#			harbour=as.character(NA),      #modif MM 01/12/2008
			vslLen=as.numeric(NA), 
			vslPwr=as.numeric(NA), 
			vslSize=as.numeric(NA), 
			vslType=as.character(NA), 
			harbour=as.character(NA),      #modif MM 01/12/2008
			foNum=as.numeric(NA), 
			daysAtSea=as.numeric(NA), 
			vslId=as.numeric(NA), 
			sampCtry=as.character(NA), 
			sampMeth=as.character(NA),
			stringsAsFactors=F),
		hh=data.frame(
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			year=as.numeric(NA), # FK
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # PK
			foVal=as.character(NA),
			aggLev=as.character(NA),
			catReg=as.character(NA),
			sppReg=as.character(NA),
			date=as.character(NA),
			time=as.character(NA),
			foDur=as.numeric(NA),
			latIni=as.numeric(NA),
			lonIni=as.numeric(NA),
			latFin=as.numeric(NA),
			lonFin=as.numeric(NA),
			area=as.character(NA),
			rect=as.character(NA),
			subRect=as.character(NA),
			foDep=as.numeric(NA),
			waterDep=as.numeric(NA),
			foCatNat=as.character(NA),
			foCatEu5=as.character(NA),
			foCatEu6=as.character(NA),
			meshSize=as.numeric(NA),
			selDev=as.character(NA),
			meshSizeSelDev=as.numeric(NA),
			stringsAsFactors=F),
		sl=data.frame(
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			year=as.numeric(NA), # FK
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # FK
			spp=as.character(NA), # PK  
			catchCat=as.character(NA), # PK 
			landCat=as.character(NA), # PK 
			commCatScl=as.character(NA), # PK
			commCat=as.character(NA), # PK
			subSampCat=as.character(NA), # PK
 		  sex=as.character(NA), # ~PK       
#			valCode=as.character(NA), 
			wt=as.numeric(NA), 
			subSampWt=as.numeric(NA), 
			lenCode=as.character(NA),
			stringsAsFactors=F),
		hl=data.frame(
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			year=as.numeric(NA), # FK
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # FK
			spp=as.character(NA), # FK 
			catchCat=as.character(NA), # FK 
			landCat=as.character(NA), # FK 
			commCatScl=as.character(NA), # FK
			commCat=as.character(NA), # FK
			subSampCat=as.character(NA), # FK
			sex=as.character(NA), # ~PK
			lenCls=as.numeric(NA), # PK
			lenNum=as.numeric(NA),
			stringsAsFactors=F),
		ca=data.frame(
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			year=as.numeric(NA), # FK
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # PK
			quarter=as.numeric(NA), # PK
			month=as.numeric(NA), # PK
			spp=as.character(NA), # PK 
			sex=as.character(NA), # PK
			catchCat=as.character(NA), # PK 
			landCat=as.character(NA), # PK 
			commCatScl=as.character(NA), # PK
			commCat=as.character(NA), # PK
			stock=as.character(NA), # PK
			area=as.character(NA), # PK
			rect=as.character(NA), # PK
			subRect=as.character(NA), #PK
			lenCls=as.numeric(NA), # PK
			age=as.numeric(NA), # PK
			fishId=as.numeric(NA), # PK
			lenCode=as.character(NA),           
			ageMeth=as.character(NA),     #modif MM 01/12/2008     
			plusGrp=as.character(NA),        
			otoWt=as.numeric(NA),                         
			otoSide=as.character(NA),       
			indWt=as.numeric(NA),             
			matMeth=as.character(NA),     #modif MM 01/12/2008             
      matScale=as.character(NA),        
			matStage=as.character(NA),        
			stringsAsFactors=F)
	),
	validity=valcsData
)

#====================================================================
# Class constructor
#====================================================================
setGeneric("csData", function(tr, hh, sl, hl, ca, ...){
	standardGeneric("csData")
	}
)

setMethod("csData", signature("data.frame", "missing", "missing", "missing", "missing"), function(tr, desc="Unknown stock", check=FALSE, ...){
	# create object and name columns properly 
	obj <- new("csData")
	
	# tr
	tr0 <- obj@tr
	names(tr) <- names(tr0)
	tr <- coerceDataFrameColumns(tr, tr0)

  #check
  if (check) check.fields(new("csData", tr=tr, desc=desc))
	# object
	new("csData", tr=tr, desc=desc)
})

setMethod("csData", signature("data.frame", "data.frame", "missing", "missing", "missing"), function(tr, hh, desc="Unknown stock", check=FALSE, ...){
	# create object and name columns properly 
	obj <- new("csData")

	# tr
	tr0 <- obj@tr
	names(tr) <- names(tr0)
	tr <- coerceDataFrameColumns(tr, tr0)
	# hh
	hh0 <- obj@hh
	names(hh) <- names(hh0)
	hh <- coerceDataFrameColumns(hh, hh0)

  #check
  if (check) check.fields(new("csData", tr=tr, hh=hh, desc=desc))
	# object
	new("csData", tr=tr, hh=hh, desc=desc)
})

setMethod("csData", signature("data.frame", "data.frame", "data.frame", "missing", "missing"), function(tr, hh, sl, desc="Unknown stock", check=FALSE, ...){
	# create object and name columns properly 
	obj <- new("csData")

	# tr
	tr0 <- obj@tr
	names(tr) <- names(tr0)
	tr <- coerceDataFrameColumns(tr, tr0)
	# hh
	hh0 <- obj@hh
	names(hh) <- names(hh0)
	hh <- coerceDataFrameColumns(hh, hh0)
	# sl
	sl0 <- obj@sl
	names(sl) <- names(sl0)
	sl <- coerceDataFrameColumns(sl, sl0)

  #check
  if (check) check.fields(new("csData", tr=tr, hh=hh, sl=sl, desc=desc))
	# object
	new("csData", tr=tr, hh=hh, sl=sl, desc=desc)
})

setMethod("csData", signature("data.frame", "data.frame", "data.frame", "data.frame", "missing"), function(tr, hh, sl, hl, desc="Unknown stock", check=FALSE, ...){
	# create object and name columns properly 
	obj <- new("csData")

	# tr
	tr0 <- obj@tr
	names(tr) <- names(tr0)
	tr <- coerceDataFrameColumns(tr, tr0)
	# hh
	hh0 <- obj@hh
	names(hh) <- names(hh0)
	hh <- coerceDataFrameColumns(hh, hh0)
	# sl
	sl0 <- obj@sl
	names(sl) <- names(sl0)
	sl <- coerceDataFrameColumns(sl, sl0)
	# hl
	hl0 <- obj@hl
	names(hl) <- names(hl0)
	hl <- coerceDataFrameColumns(hl, hl0)

  #check
  if (check) check.fields(new("csData", tr=tr, hh=hh, sl=sl, hl=hl, desc=desc))
  #object
	new("csData", tr=tr, hh=hh, sl=sl, hl=hl, desc=desc)
})

setMethod("csData", signature("data.frame", "data.frame", "data.frame", "data.frame", "data.frame"), function(tr, hh, sl, hl, ca,  desc="Unknown stock", check=FALSE, ...){
	# create object and name columns properly 
	obj <- new("csData")
	# tr
	tr0 <- obj@tr
	names(tr) <- names(tr0)
	tr <- coerceDataFrameColumns(tr, tr0)
	# hh
	hh0 <- obj@hh
	names(hh) <- names(hh0)
	hh <- coerceDataFrameColumns(hh, hh0)
	# sl
	sl0 <- obj@sl
	names(sl) <- names(sl0)
	sl <- coerceDataFrameColumns(sl, sl0)
	# hl
	hl0 <- obj@hl
	names(hl) <- names(hl0)
	hl <- coerceDataFrameColumns(hl, hl0)
	# ca
	ca0 <- obj@ca
	names(ca) <- names(ca0)
	ca <- coerceDataFrameColumns(ca, ca0)

  #check
  if (check) check.fields(new("csData", tr=tr, hh=hh, sl=sl, hl=hl, ca=ca, desc=desc))
	# object
	new("csData", tr=tr, hh=hh, sl=sl, hl=hl, ca=ca, desc=desc)
})

setMethod("csData", signature("data.frame", "missing", "missing", "missing", "data.frame"), function(tr, hh, sl, hl, ca,  desc="Unknown stock", check=FALSE, ...){
	# create object and name columns properly 
	obj <- new("csData")
	# tr
	tr0 <- obj@tr
	names(tr) <- names(tr0)
	tr <- coerceDataFrameColumns(tr, tr0)
	# ca
	ca0 <- obj@ca
	names(ca) <- names(ca0)
	ca <- coerceDataFrameColumns(ca, ca0)

  #check
  if (check) check.fields(new("csData", tr=tr, ca=ca, desc=desc))
	# object
	new("csData", tr=tr, ca=ca, desc=desc)
})


setMethod("csData", signature("missing", "missing", "missing", "missing", "missing"), function(desc="Unknown stock", ...){
	# create object and name columns properly 
	new("csData", desc=desc)
})


#====================================================================
# IO constructor
#====================================================================
  #addition of a default format using colClasses to keep RECTANGLE and SUB_RECTANGLE as character 
setMethod("csData", signature("character", "character", "character", "character", "missing"), function(tr, hh, sl, hl, desc="Unknown stock", check=FALSE,...){

	# read CSV files
	# ToDo
	tr <- read.csv(tr,...)
	hh <- read.csv(hh,colClasses=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"character",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"character","character",NA,NA,NA,NA,NA,NA,NA,NA),...)
	sl <- read.csv(sl,...)
	hl <- read.csv(hl,...)

	# check names are correct
	checkTRnms(tr)
	checkHHnms(hh)
	checkSLnms(sl)
	checkHLnms(hl)

	# remove record type 
	tr <- tr[,-1]
	hh <- hh[,-1]
	sl <- sl[,-1]
	hl <- hl[,-1]

	# create object and name columns properly 
	obj <- new("csData")
	names(tr) <- names(obj@tr)
	names(hh) <- names(obj@hh)
	names(sl) <- names(obj@sl)
	names(hl) <- names(obj@hl)
	#check
  if (check) check.fields(csData(tr=tr, hh=hh, sl=sl, hl=hl, desc=desc))
  csData(tr=tr, hh=hh, sl=sl, hl=hl, desc=desc)
})


setMethod("csData", signature("character", "character", "character", "character", "character"), function(tr, hh, sl, hl, ca, desc="Unknown stock", check=FALSE,...){

	# read CSV files
	# ToDo
	tr <- read.csv(tr,...)
	hh <- read.csv(hh,colClasses=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"character",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"character","character",NA,NA,NA,NA,NA,NA,NA,NA),...)
	sl <- read.csv(sl,...)
	hl <- read.csv(hl,...)
	ca <- read.csv(ca,colClasses=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"character","character",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),...)

	# check names are correct
	checkTRnms(tr)
	checkHHnms(hh)
	checkSLnms(sl)
	checkHLnms(hl)
  checkCAnms(ca)

	# remove record type 
	tr <- tr[,-1]
	hh <- hh[,-1]
	sl <- sl[,-1]
	hl <- hl[,-1]
  ca <- ca[,-1]

	# create object and name columns properly 
	obj <- new("csData")
	names(tr) <- names(obj@tr)
	names(hh) <- names(obj@hh)
	names(sl) <- names(obj@sl)
	names(hl) <- names(obj@hl)
	names(ca) <- names(obj@ca)
	#check
  if (check) check.fields(csData(tr=tr, hh=hh, sl=sl, hl=hl, ca=ca, desc=desc))	
	csData(tr=tr, hh=hh, sl=sl, hl=hl, ca=ca, desc=desc)
})



setMethod("csData", signature("character", "character", "character", "missing", "missing"), function(tr, hh, sl, desc="Unknown stock", check=FALSE,...){

	# read CSV files
	# ToDo
	tr <- read.csv(tr,...)
	hh <- read.csv(hh,colClasses=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"character",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"character","character",NA,NA,NA,NA,NA,NA,NA,NA),...)
	sl <- read.csv(sl,...)

	# check names are correct
	checkTRnms(tr)
	checkHHnms(hh)
	checkSLnms(sl)

	# remove record type 
	tr <- tr[,-1]
	hh <- hh[,-1]
	sl <- sl[,-1]

	# create object and name columns properly 
	obj <- new("csData")
	names(tr) <- names(obj@tr)
	names(hh) <- names(obj@hh)
	names(sl) <- names(obj@sl)
	#check
  if (check) check.fields(csData(tr=tr, hh=hh, sl=sl, desc=desc))
  csData(tr=tr, hh=hh, sl=sl, desc=desc)
})



setMethod("csData", signature("character", "character", "missing", "missing", "missing"), function(tr, hh, desc="Unknown stock", check=FALSE,...){

	# read CSV files
	# ToDo
	tr <- read.csv(tr,...)
	hh <- read.csv(hh,colClasses=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"character",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"character","character",NA,NA,NA,NA,NA,NA,NA,NA),...)

	# check names are correct
	checkTRnms(tr)
	checkHHnms(hh)

	# remove record type 
	tr <- tr[,-1]
	hh <- hh[,-1]

	# create object and name columns properly 
	obj <- new("csData")
	names(tr) <- names(obj@tr)
	names(hh) <- names(obj@hh)
	#check
  if (check) check.fields(csData(tr=tr, hh=hh, desc=desc))
  csData(tr=tr, hh=hh, desc=desc)
})


setMethod("csData", signature("character", "missing", "missing", "missing", "missing"), function(tr, desc="Unknown stock", check=FALSE,...){

	# read CSV files
	# ToDo
	tr <- read.csv(tr,...)

	# check names are correct
	checkTRnms(tr)

	# remove record type 
	tr <- tr[,-1]

	# create object and name columns properly 
	obj <- new("csData")
	names(tr) <- names(obj@tr)
	#check
  if (check) check.fields(csData(tr=tr, desc=desc))
  csData(tr=tr, desc=desc)
})


setMethod("csData", signature("character", "missing", "missing", "missing", "character"), function(tr, hh, sl, hl, ca, desc="Unknown stock", check=FALSE,...){

	# read CSV files
	# ToDo
	tr <- read.csv(tr,...)
	ca <- read.csv(ca,colClasses=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"character","character",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),...)

	# check names are correct
	checkTRnms(tr)
  checkCAnms(ca)

	# remove record type 
	tr <- tr[,-1]
  ca <- ca[,-1]

	# create object and name columns properly 
	obj <- new("csData")
	names(tr) <- names(obj@tr)
	names(ca) <- names(obj@ca)
	#check
  if (check) check.fields(csData(tr=tr, ca=ca, desc=desc))	
	csData(tr=tr, ca=ca, desc=desc)
})



#====================================================================
# Accessor functions
#====================================================================

setGeneric("tr", function(object, ...){
	standardGeneric("tr")
	}
)

setMethod("tr", signature("csData"), function(object, ...){
	object@tr
	}
)

setGeneric("hh", function(object, ...){
	standardGeneric("hh")
	}
)

setMethod("hh", signature("csData"), function(object, ...){
	object@hh
	}
)

setGeneric("sl", function(object, ...){
	standardGeneric("sl")
	}
)

setMethod("sl", signature("csData"), function(object, ...){
	object@sl
	}
)

setGeneric("hl", function(object, ...){
	standardGeneric("hl")
	}
)

setMethod("hl", signature("csData"), function(object, ...){
	object@hl
	}
)

setGeneric("ca", function(object, ...){
	standardGeneric("ca")
	}
)

setMethod("ca", signature("csData"), function(object, ...){
	object@ca
	}
)

setMethod("desc", signature("csData"), function(object, ...){
	object@desc
	}
)

#====================================================================
# 'Head' and 'Tail' functions
#====================================================================

setMethod("head", signature("csData"), function(x, ...){
  object <- new("csData",desc=x@desc)
  object@tr <- head(x@tr)
  object@hh <- head(x@hh)
  object@sl <- head(x@sl)
  object@hl <- head(x@hl)
  object@ca <- head(x@ca)
  return(object)  
	}
)


setMethod("tail", signature("csData"), function(x, ...){
  object <- new("csData",desc=x@desc)
  object@tr <- tail(x@tr)
  object@hh <- tail(x@hh)
  object@sl <- tail(x@sl)
  object@hl <- tail(x@hl)
  object@ca <- tail(x@ca)
  return(object)  
	}
)

#====================================================================
# 'summary' function
#====================================================================

setMethod("summary", signature("csData"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$tr <- summary(object@tr)
  ll$hh <- summary(object@hh)
  ll$sl <- summary(object@sl)
  ll$hl <- summary(object@hl)
  ll$ca <- summary(object@ca)
  return(ll)  
	}
)


#====================================================================
# 'dim' function
#====================================================================

setMethod("dim", signature("csData"), function(x){
  ll <- list()
  ll$tr <- dim(x@tr)
  ll$hh <- dim(x@hh)
  ll$sl <- dim(x@sl)
  ll$hl <- dim(x@hl)
  ll$ca <- dim(x@ca)
  return(ll)  
	}
)

#====================================================================
# 'is.' function
#====================================================================

# setGeneric("is.csData", function(object){
# 	standardGeneric("is.csData")
# })


# setMethod("is.csData","ANY", function(object){
# 	return(is(object, "csData"))
# })

#====================================================================
# rbind2
#====================================================================

setMethod("rbind2", signature(x="csData", y="csData"), function(x,y){

	# get info
	tr1 <- tr(x)
	hh1 <- hh(x)
	sl1 <- sl(x)
	hl1 <- hl(x)
	ca1 <- ca(x)

	tr2 <- tr(y)
	hh2 <- hh(y)
	sl2 <- sl(y)
	hl2 <- hl(y)
	ca2 <- ca(y)

	# bind
	tr <- rbind2(tr1,tr2)
	hh <- rbind2(hh1,hh2)
	sl <- rbind2(sl1,sl2)
	hl <- rbind2(hl1,hl2)
	ca <- rbind2(ca1,ca2)

  fun <- function(df) if (!all(is.na(df))) df[apply(df,1,function(x) !all(is.na(x))),] else df[1,]                #modif MM 23/09/2009
	# new object                                                                                                    #
	csData(tr=unique(fun(tr)), hh=unique(fun(hh)), sl=unique(fun(sl)), hl=unique(fun(hl)), ca=unique(fun(ca)))      #
})




##====================================================================
## subset
##====================================================================
#
#setMethod("subset", signature(x="csData"), function(x,subset,..., table="tr"){
#
#	if(table!="tr") stop("Subseting implemented only for slot tr.")
#	
#	# get idx
#	trpk <- tr(x)[,1:6]
#	trpk <- apply(trpk,1,paste,collapse="")
#	trpk <- gsub("[[:space:]]","",trpk)
#
#	hhfk <- hh(x)[,1:6]
#	hhfk <- apply(hhfk,1,paste,collapse="")
#	hhfk <- gsub("[[:space:]]","",hhfk)
#
#	hhpk <- hh(x)[,1:7]
#	hhpk <- apply(hhpk,1,paste,collapse="")
#	hhpk <- gsub("[[:space:]]","",hhpk)
#
#	slfk <- sl(x)[,1:7]
#	slfk <- apply(slfk,1,paste,collapse="")
#	slfk <- gsub("[[:space:]]","",slfk)
#
#	slpk <- sl(x)[,1:13]
#	slpk <- apply(slpk,1,paste,collapse="")
#	slpk <- gsub("[[:space:]]","",slpk)
#
#	hlfk <- hl(x)[,c(1:13)]
#	hlfk <- apply(hlfk,1,paste,collapse="")
#	hlfk <- gsub("[[:space:]]","",hlfk)
#
##	hlpk <- hl(x)[,1:15]
##	hlpk <- apply(hlpk,1,paste,collapse="")
##	hlpk <- gsub("[[:space:]]","",hlpk)
#
#	cafk <- ca(x)[,c(1:6)]
#	cafk <- apply(cafk,1,paste,collapse="")
#	cafk <- gsub("[[:space:]]","",cafk)
#
##	capk <- ca(x)[,c(1:21)]
##	capk <- apply(capk,1,paste,collapse="")
##	capk <- gsub("[[:space:]]","",capk)
#	
#	# new idx
#	e <- substitute(subset)
#	df0 <- do.call(table, list(object=x))
#	r <- eval(e, df0, parent.frame())
#	
#	# subset
#	tr <- df0[r,]
#	tridx <- apply(tr[,1:6],1,paste,collapse="")
#	tridx <- gsub("[[:space:]]","",tridx)
#	hh <- hh(x)[hhfk %in% tridx,]	
#	hhidx <- apply(hh[,1:7],1,paste,collapse="")
#	hhidx <- gsub("[[:space:]]","",hhidx)
#	sl <- sl(x)[slfk %in% hhidx,]	
#	slidx <- apply(sl[,1:13],1,paste,collapse="")
#	slidx <- gsub("[[:space:]]","",slidx)
#	hl <- hl(x)[hlfk %in% slidx,]	
#	ca <- ca(x)[cafk %in% tridx,]
#
#	# output
#	if(nrow(tr)<1) csData()
#	if(nrow(hh)<1) csData(tr=tr)
#	if(nrow(sl)<1) csData(tr=tr, hh=hh)
#	if(nrow(hl)<1) csData(tr=tr, hh=hh, sl=sl)
#	if(nrow(ca)<1) csData(tr=tr, hh=hh, sl=sl, hl=hl)
#	else csData(tr=tr, hh=hh, sl=sl, hl=hl, ca=ca)
#})
#
#
#



#====================================================================
# MM 21/07/08
# subset : one specified table is subset, and other tables are accordingly subset downward and upward 
# (hh, sl , hl subsets don't impact on ca ; ca subset doesn't impact on hh, sl & hl) 
#====================================================================


subsetCOST <- function(x,subset,..., table="tr"){

isVal <- class(x)=="csDataVal"
  #-----------------------------------------------------------------------------
  # Extraction of each table
  #-----------------------------------------------------------------------------

  tr <- tr(x)
  hh <- hh(x)
  sl <- sl(x)
  hl <- slSex(sl,hl(x))     #hl with an artificial 'sex' field to consider 'sex' field in sl as a 'full' key field
  ca <- ca(x)

  #-----------------------------------------------------------------------------
  # Function to build a primary or a foreign key for any table
  #-----------------------------------------------------------------------------
  
  fpKey <- function(tab,colIndex,sep=":-:") {
    key <- tab[,colIndex]
    key <- apply(key,1,paste,collapse=sep)
    key <- gsub("[[:space:]]","",key)
    return(key)
  }


  #-----------------------------------------------------------------------------
  # Parts of tr that are linked to hh & ca are identified
  #-----------------------------------------------------------------------------
  
  indca <- fpKey(tr,1:6)%in%fpKey(ca,1:6)
  #part of tr that is linked to ca, and index
  trca <- tr[indca,] ; trca$N <- (1:nrow(tr))[indca]
  #part of tr that is linked to hh (ie not linked to ca), and index
  trhh <- tr[!indca,] ; trhh$N <- (1:nrow(tr))[!indca]

  #-----------------------------------------------------------------------------
  # Specified table is subset according to 'subset' parameter
  #-----------------------------------------------------------------------------
	e <- substitute(subset)
	df0 <- eval(parse('',text=table))#do.call(table, list(object=x))  
	r <- eval(e, df0, parent.frame(n=1))
  eval(parse('',text=paste(table, "<- df0[r,]")))

  #-----------------------------------------------------------------------------
  # Keyfield indexes according to table hierarchy are defined in Up & Down tables
  #-----------------------------------------------------------------------------

  Up <- matrix(c("trhh","hh","sl","trca","1:6","1:7","1:14","1:6"),nrow=2,byrow=TRUE)
  dimnames(Up) <- list(c("tab","index"),c("hh","sl","hl","ca"))

  Down <- matrix(c("hh","ca","sl","hl","1:6","1:6","1:7","1:14"),nrow=2,byrow=TRUE)
  dimnames(Down) <- list(c("tab","index"),c("tr","tr","hh","sl"))

  #-----------------------------------------------------------------------------
  # Generic subsetting function using Up & Down table format
  #-----------------------------------------------------------------------------
  
  subs <- function(tabName,tabKey){
  if (tabName%in%dimnames(tabKey)[[2]]) {
    indSub <<- TRUE   #index that shows that the procedure has been used 
    mat <- tabKey[,dimnames(tabKey)[[2]]%in%tabName,drop=FALSE]
    eval(parse('',text=paste(mat["tab",], " <<- ", mat["tab",], "[fpKey(", mat["tab",], ",", mat["index",],           #warning : "<<-" might be replaced by 'assign(...)'
                             ")%in%fpKey(", tabName, ",", mat["index",], "),]",sep="",collapse=";")))
    Recall(mat["tab",1],tabKey)      #mat["tab",1] because if tabName=="tr" & tabKey=Down, mat["tab",] = c("hh",ca")
  }}

  #-----------------------------------------------------------------------------
  # Let's apply the subsetting "loop" 
  #-----------------------------------------------------------------------------
 
    indSub <- FALSE
    #first, upward from 'table'...
    subs(table,Up)
    
    #then paste trhh & trca, and reorder according to N field (if indSub=TRUE)
    if (indSub) {
      tr <- rbind.data.frame(trhh,trca)
      tr <- tr[order(tr$N),1:(ncol(tr)-1)]
    }
 
    #and finally, downward from "tr" (for consistency)
    subs("tr",Down)
  
    if (nrow(hl)>0) {hl$sex <- hl$lsex ; hl <- hl[,-ncol(hl)]}                  #modif 08/12/2008 MM

  #-----------------------------------------------------------------------------
  # Output
  #-----------------------------------------------------------------------------

# if(nrow(tr)<1) res <- csData(desc=x@desc)                                                              #modif 19/01/2009
# 	else if ((nrow(hh)<1) & (nrow(ca)>0)) res <- csData(tr=tr,ca=ca,desc=x@desc)                         #
# 	      else if(nrow(hh)<1) res <- csData(tr=tr,desc=x@desc)                                           #
#             else if(nrow(sl)<1) res <- csData(tr=tr, hh=hh,desc=x@desc)                                #
#                  else if(nrow(hl)<1) res <- csData(tr=tr, hh=hh, sl=sl,desc=x@desc)                    #
#	                     else if(nrow(ca)<1) res <- csData(tr=tr, hh=hh, sl=sl, hl=hl,desc=x@desc)         #
#	                          else res <- csData(tr=tr, hh=hh, sl=sl, hl=hl, ca=ca,desc=x@desc)            #
#                                                                                                        #
                                                                                                         #
 if(nrow(tr)<1) tr <- csData()@tr                                                                        #
 if(nrow(hh)<1) hh <- csData()@hh                                                                        #
 if(nrow(sl)<1) sl <- csData()@sl                                                                        #
 if(nrow(hl)<1) hl <- csData()@hl                                                                        #
 if(nrow(ca)<1) ca <- csData()@ca                                                                        #
res <- csData(tr=tr,hh=hh,sl=sl,hl=hl,ca=ca,desc=x@desc)                                                 #

if (isVal) res <- csDataVal(res)
return(res)

}


setMethod("subset", signature(x="csData"), function(x,subset,..., table="tr",link=FALSE){    #if link, 'table' & ca tables are subset  
subset <- substitute(subset)
tab <- eval(parse('',text=paste("x@",table,sep="")))
x <- subsetCOST(x,subset=eval(subset,tab),table=table)
if (link) subsetCOST(x,subset=eval(subset,x@ca),table="ca") else x        #modif 9/05/2011
})


#====================================================================
# MM 21/07/08
# subsetSpp : only sl table is subset, and only hl is modified consequently
#====================================================================



setGeneric("subsetSpp", function(x,subset,link=FALSE,...){
	standardGeneric("subsetSpp")
	}
)

setMethod("subsetSpp", signature(x="csData"), function(x,subset,link=FALSE,...){

is.Val <- class(x)=="csDataVal"
  
  hl <- slSex(sl(x),hl(x))                                                    #08/12/2008 modif MM
  #get idx                                                                      #
	hlfk <- hl[,c(1:14)]                                                          #
	hlfk <- apply(hlfk,1,paste,collapse=":-:")
	hlfk <- gsub("[[:space:]]","",hlfk)
  
  # new idx
	e <- substitute(subset)
	df0 <- do.call("sl", list(object=x))
	r <- eval(e, df0, parent.frame(n=2))
	
	# subset
	sl <- df0[r,]
	slidx <- apply(sl[,1:14],1,paste,collapse=":-:")
	slidx <- gsub("[[:space:]]","",slidx)
	Hl <- hl[hlfk %in% slidx,]	                                                  #08/12/2008 modif MM
  Hl$sex <- Hl$lsex ; hl <- Hl[,-ncol(Hl)]                                      #

	# output
	if(nrow(sl)<1) {sl <- csData()@sl                                             #modif 19/01/2009
                  warning("No data kept from subsetting process!!")}            #
  if(nrow(hl)<1) {hl <- csData()@hl}  
  
  if (link) {subset <- substitute(subset)
             ca <- subset(ca(x),eval(subset)) 
             if(nrow(ca)<1) {ca <- csData()@ca                                             #modif 16/04/2009
                  warning("No data kept from subsetting process in ca table!!")}  
  } else {
  ca <- ca(x)}
  
  res <- csData(tr=tr(x), hh=hh(x), sl=sl, hl=hl, ca=ca,desc=x@desc)         #

if (is.Val) res <- csDataVal(res)
return(res)

})


#====================================================================
# replacement
#====================================================================

if (!isGeneric("replace")) setGeneric("replace")


