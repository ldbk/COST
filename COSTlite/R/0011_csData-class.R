#' Validity check function for \link{csData-class} object
#'
#' @description Sampling data class validity check 
#'
#' @param object a \link{csData-class} object
#' @return a boolean (TRUE if the object is a \link{csData-class} object, FALSE if not)
#'
#' @export
valcsData <- function(object){
	tr <- object@tr
	hh <- object@hh
	sl <- object@sl
	hl <- object@hl
	ca <- object@ca
	obj <- methods::new("csData")
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
	if(checkpk(tr,"tr")==FALSE) stop("Primary key not unique in slot candidate \"tr\".")
	if(checkpk(hh,"hh")==FALSE) stop("Primary key not unique in slot candidate \"hh\".")
	if(checkpk(sl,"sl")==FALSE) stop("Primary key not unique in slot candidate \"sl\".")
	if(checkpk(hl,"hl")==FALSE) stop("Primary key not unique in slot candidate \"hl\".")
	if(checkpk(ca,"ca")==FALSE) stop("Primary key not unique in slot candidate \"hl\".")
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

#' @title csData-class
#'
#' @description Commercial fisheries sampling data object
#'
#' @slot desc Character chain of a descriptor
#' @slot tr trip information
#' @slot hh haul information
#' @slot sl species list information
#' @slot hl length frequencies information
#' @slot ca age,weight, maturity information
#'
#' @export
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
			vslLen=as.numeric(NA), 
			vslPwr=as.numeric(NA), 
			vslSize=as.numeric(NA), 
			vslType=as.character(NA), 
			harbour=as.character(NA),      
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
			ageMeth=as.character(NA),
			plusGrp=as.character(NA),        
			otoWt=as.numeric(NA),                         
			otoSide=as.character(NA),       
			indWt=as.numeric(NA),             
			matMeth=as.character(NA),
			matScale=as.character(NA),        
			matStage=as.character(NA),        
			stringsAsFactors=F)
	),
	validity=valcsData
)

#' csData constructors
#' @param tr,hh,sl,hl,ca dataframe following the slots of a \link{csData-class} object
#' @param desc descriptor
#' @param missing : create an empty \link{csData-class} object
#' @param \\dots parameters
#' @rdname csData-constructors
#' @docType methods
#' @export
setGeneric("csData", function(tr, hh, sl, hl, ca, ...){
	standardGeneric("csData")
	}
)

#' @rdname csData-constructors
setMethod("csData", signature("data.frame", "missing", "missing", "missing", "missing"), function(tr, desc="Unknown stock", ...){
	# create object and name columns properly 
	obj <- methods::new("csData")
	# tr
	tr0 <- obj@tr
	names(tr) <- names(tr0)
	tr <- coerceDataFrameColumns(tr, tr0)
	# object
	methods::new("csData", tr=tr, desc=desc)
})

#' @rdname csData-constructors
setMethod("csData", signature("data.frame", "data.frame", "missing", "missing", "missing"), function(tr, hh, desc="Unknown stock", ...){
	# create object and name columns properly 
	obj <- methods::new("csData")
	# tr
	tr0 <- obj@tr
	names(tr) <- names(tr0)
	tr <- coerceDataFrameColumns(tr, tr0)
	# hh
	hh0 <- obj@hh
	names(hh) <- names(hh0)
	hh <- coerceDataFrameColumns(hh, hh0)
	# object
	methods::new("csData", tr=tr, hh=hh, desc=desc)
})

#' @rdname csData-constructors
setMethod("csData", signature("data.frame", "data.frame", "data.frame", "missing", "missing"), function(tr, hh, sl, desc="Unknown stock", ...){
	# create object and name columns properly 
	obj <- methods::new("csData")
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
	# object
	methods::new("csData", tr=tr, hh=hh, sl=sl, desc=desc)
})

#' @rdname csData-constructors
setMethod("csData", signature("data.frame", "data.frame", "data.frame", "data.frame", "missing"), function(tr, hh, sl, hl, desc="Unknown stock", ...){
	# create object and name columns properly 
	obj <- methods::new("csData")
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
	#object
	methods::new("csData", tr=tr, hh=hh, sl=sl, hl=hl, desc=desc)
})

#' @rdname csData-constructors
setMethod("csData", signature("data.frame", "data.frame", "data.frame", "data.frame", "data.frame"), function(tr, hh, sl, hl, ca,  desc="Unknown stock", ...){
	# create object and name columns properly 
	obj <- methods::new("csData")
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
	# object
	methods::new("csData", tr=tr, hh=hh, sl=sl, hl=hl, ca=ca, desc=desc)
})

#' @rdname csData-constructors
setMethod("csData", signature("data.frame", "missing", "missing", "missing", "data.frame"), function(tr, hh, sl, hl, ca,  desc="Unknown stock", ...){
	# create object and name columns properly 
	obj <- methods::new("csData")
	# tr
	tr0 <- obj@tr
	names(tr) <- names(tr0)
	tr <- coerceDataFrameColumns(tr, tr0)
	# ca
	ca0 <- obj@ca
	names(ca) <- names(ca0)
	ca <- coerceDataFrameColumns(ca, ca0)
	# object
	methods::new("csData", tr=tr, ca=ca, desc=desc)
})

#' @rdname csData-constructors
setMethod("csData", signature("missing", "missing", "missing", "missing", "missing"), function(desc="Unknown stock", ...){
	# create object and name columns properly 
	methods::new("csData", desc=desc)
})


#' @title csData-methods
#'
#' @description Methods for \link{csData-class} object
#' @param object,x,y \link{csData-class} object
#' @param \\dots parameters
#'
#' @docType methods
#' @rdname csData-methods
#' @export
setGeneric("tr", function(object, ...){ standardGeneric("tr") })

#' @rdname csData-methods
#' @export
setMethod("tr", signature("csData"), function(object, ...){ object@tr })

#' @rdname csData-methods
#' @export
setGeneric("hh", function(object, ...){ standardGeneric("hh") })

#' @rdname csData-methods
#' @export
setMethod("hh", signature("csData"), function(object, ...){ object@hh })

#' @rdname csData-methods
#' @export
setGeneric("sl", function(object, ...){ standardGeneric("sl") })

#' @rdname csData-methods
#' @export
setMethod("sl", signature("csData"), function(object, ...){ object@sl })

#' @rdname csData-methods
#' @export
setGeneric("hl", function(object, ...){ standardGeneric("hl") })

#' @rdname csData-methods
#' @export
setMethod("hl", signature("csData"), function(object, ...){ object@hl })

#' @rdname csData-methods
#' @export
setGeneric("ca", function(object, ...){ standardGeneric("ca") })

#' @rdname csData-methods
#' @export
setMethod("ca", signature("csData"), function(object, ...){ object@ca })

#' @rdname csData-methods
setMethod("head", signature("csData"), function(x, ...){
  object <- methods::new("csData",desc=x@desc)
  object@tr <- head(x@tr)
  object@hh <- head(x@hh)
  object@sl <- head(x@sl)
  object@hl <- head(x@hl)
  object@ca <- head(x@ca)
  return(object)  
	}
)

#' @rdname csData-methods
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

#' @rdname csData-methods
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

#' @rdname csData-methods
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
	tr <- methods::rbind2(tr1,tr2)
	hh <- methods::rbind2(hh1,hh2)
	sl <- methods::rbind2(sl1,sl2)
	hl <- methods::rbind2(hl1,hl2)
	ca <- methods::rbind2(ca1,ca2)
	fun <- function(df) if (!all(is.na(df))) df[apply(df,1,function(x) !all(is.na(x))),] else df[1,] 
	csData(tr=unique(fun(tr)), hh=unique(fun(hh)), sl=unique(fun(sl)), hl=unique(fun(hl)), ca=unique(fun(ca)))
})

#' Subset \link{csData-class} object
#'
#' @description subset : one specified table is subset, and other tables are accordingly subset downward and upward (hh, sl , hl subsets don't impact on ca ; ca subset doesn't impact on hh, sl & hl) 
#'
#' @param x a \link{csData-class} object
#' @param subset subset expression
#' @param \\dots parameters
#' @param table the table targeted by the subset
#' @param link boolean if TRUE 'table' & ca tables are subset  
#' @return a \link{csData-class} object, subsetted
#' @rdname subsetCOST
#'
#' @export
subsetCOST <- function(x,subset,..., table="tr"){
	isVal <- class(x)=="csDataVal"
  #-----------------------------------------------------------------------------
  # Extraction of each table
  #-----------------------------------------------------------------------------
  tr <- tr(x)
  hh <- hh(x)
  sl <- sl(x)
  hl <- slSex(sl,hl(x)) #hl with an artificial 'sex' field to consider 'sex' field in sl as a 'full' key field
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
    if (nrow(hl)>0) {hl$sex <- hl$lsex ; hl <- hl[,-ncol(hl)]}                  
  #-----------------------------------------------------------------------------
  # Output
  #-----------------------------------------------------------------------------
 if(nrow(tr)<1) tr <- csData()@tr                                                                        #
 if(nrow(hh)<1) hh <- csData()@hh                                                                        #
 if(nrow(sl)<1) sl <- csData()@sl                                                                        #
 if(nrow(hl)<1) hl <- csData()@hl                                                                        #
 if(nrow(ca)<1) ca <- csData()@ca                                                                        #
 res <- csData(tr=tr,hh=hh,sl=sl,hl=hl,ca=ca,desc=x@desc)                                                 #
 if (isVal) res <- csDataVal(res)
 return(res)
}


#' @rdname subsetCOST
setMethod("subset", signature(x="csData"), function(x,subset,..., table="tr",link=FALSE){    
	subset <- substitute(subset)
	tab <- eval(parse('',text=paste("x@",table,sep="")))
	x <- subsetCOST(x,subset=eval(subset,tab),table=table)
	if (link) subsetCOST(x,subset=eval(subset,x@ca),table="ca") else x     
})


#====================================================================
# MM 21/07/08
# subsetSpp : only sl table is subset, and only hl is modified consequently
#====================================================================


#' @title subsetSpp
#'
#' @description 
#' This method implements subsetting for the raw, the validated and the
#' consolidated CS objects provided by COSTcore, proceeding specifically on SL
#' table. This subset only impacts on HL table, and preserves the other tables.
#' If link, ca is subsetted.
#' 
#' @param x,subset,link parameters
#' @param \\dots parameters
#' @rdname subsetspp
#'
#' @export
setGeneric("subsetSpp", function(x,subset,link=FALSE,...){
	standardGeneric("subsetSpp")
	}
)

#' @rdname subsetspp
setMethod("subsetSpp", signature(x="csData"), function(x,subset,link=FALSE,...){
  is.Val <- class(x)=="csDataVal"
  hl <- slSex(sl(x),hl(x))                                                    
  #get idx                                                                      
	hlfk <- hl[,c(1:14)]                                                          
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
	Hl <- hl[hlfk %in% slidx,]	                                             
	Hl$sex <- Hl$lsex ; hl <- Hl[,-ncol(Hl)]                                      
	# output
	if(nrow(sl)<1) {sl <- csData()@sl
                  warning("No data kept from subsetting process!!")}            
  if(nrow(hl)<1) {hl <- csData()@hl}  
  if (link) {subset <- substitute(subset)
             ca <- subset(ca(x),eval(subset)) 
             if(nrow(ca)<1) {ca <- csData()@ca                                         
                  warning("No data kept from subsetting process in ca table!!")}  
  } else {
  ca <- ca(x)}
  res <- csData(tr=tr(x), hh=hh(x), sl=sl, hl=hl, ca=ca,desc=x@desc)
  if (is.Val) res <- csDataVal(res)
  return(res)
})
