#' Validity check function for \link{csDataCons-class} object
#'
#' @description Commercial sampling data class validity check 
#'
#' @param object a \link{csDataCons-class} object
#' @return a boolean (TRUE if the object is a \link{csDataCons-class} object, FALSE if not)
#'
#' @export
valcscData <- function(object){
	tr <- object@tr
	hh <- object@hh
	sl <- object@sl
	hl <- object@hl
	ca <- object@ca
	obj <- methods::new("csDataCons")
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
	# Everything is fine
	return(TRUE)
}

#' @title csDataCons-class
#'
#' @description Commercial fisheries sampling data consolidated object
#'
#' @slot desc Character chain of a descriptor
#' @slot tr trip information
#' @slot hh haul information
#' @slot sl species list information
#' @slot hl length frequencies information
#' @slot ca age,weight, maturity information
#'
#' @export
setClass("csDataCons",
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
			PSUid=as.numeric(NA), # 
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sampType=as.factor(NA), # PK
			landCtry=as.factor(NA), # PK
			vslFlgCtry=as.factor(NA), # PK
			proj=as.factor(NA), # PK
			trpCode=as.factor(NA), # PK
			foNum=as.numeric(NA), 
			daysAtSea=as.numeric(NA), 
			vslId=as.numeric(NA), 
			sampCtry=as.factor(NA), 
			sampMeth=as.factor(NA)),
		hh=data.frame(
			PSUid=as.numeric(NA), # FK
			SSUid=as.numeric(NA), # 
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # PK
			foVal=as.character(NA),
			aggLev=as.character(NA),
			catReg=as.character(NA),
			sppReg=as.character(NA),
			date=as.character(NA), #	=> time
			foDur=as.numeric(NA),
			latIni=as.numeric(NA),
			lonIni=as.numeric(NA),
			latFin=as.numeric(NA),
			lonFin=as.numeric(NA),
			foDep=as.numeric(NA)
			),
		sl=data.frame(
			PSUid=as.numeric(NA), # FK
			SSUid=as.numeric(NA), # 
			TSUid=as.numeric(NA), # 
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sort=as.factor(NA),
			sampType=as.factor(NA), # FK
			landCtry=as.factor(NA), # FK
			vslFlgCtry=as.factor(NA), # FK
			proj=as.factor(NA), # FK
			trpCode=as.factor(NA), # FK
			staNum=as.numeric(NA), # FK
			spp=as.factor(NA), # PK 
			sex=as.factor(NA), # PK 
			wt=as.numeric(NA), 
			subSampWt=as.numeric(NA), 
			lenCode=as.factor(NA)),
		hl=data.frame(
			PSUid=as.numeric(NA), # 
			SSUid=as.numeric(NA), #
			TSUid=as.numeric(NA), # FK
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sort=as.factor(NA),
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # FK
			spp=as.character(NA), # FK 
			sex=as.character(NA), # PK
			lenCls=as.numeric(NA), # PK
			lenNum=as.numeric(NA)),
		ca=data.frame(
			PSUid=as.numeric(NA), # FK
			SSUid=as.numeric(NA), # 
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sort=as.factor(NA),
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # PK
			spp=as.character(NA), # PK 
			sex=as.character(NA), # PK
			stock=as.character(NA), # PK
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
			matStage=as.character(NA))
	),
	validity=valcscData
)

#' Class constructor for validated objects with only tr & ca tables filled (not to be exported)
#'
#' @description Fishing effort data class validity check
#'
#' @param object a \link{csDataVal-class} object
#' @param objStrat stratification \link{strIni-class} object
#' @param desc character vector of stock information
#' @param \\dots parameter
#'
csDataConsCATR <- function(object,                 #no hh, sl & hl tables in validated object
                           objStrat,
                           desc="Unknown stock",  
                           ...){  

timeStrata <- objStrat@timeStrata              # 
spaceStrata <- objStrat@spaceStrata            #
techStrata <- objStrat@techStrata              #
tpRec <- objStrat@tpRec                        #
spRec <- objStrat@spRec                        #
tcRec <- objStrat@tcRec                        #
CA <- ca(object)
# Time stratification
Semester <- ceiling(CA$quarter/2) 
CA$month <- paste(as.character(CA$year),as.character(CA$quarter),as.character(CA$month),sep=" - ")
CA$quarter <- paste(as.character(CA$year),as.character(CA$quarter),sep=" - ")
CA$semester <- paste(as.character(CA$year),as.character(Semester),sep=" - ")
if (is.na(timeStrata)) {
  CA$time <- "all" 
  tpRec <- as.list(NA)
} else {
  CA$time <- CA[,timeStrata]}  
if (!is.na(tpRec[1])) CA <- recFun(CA,"time",tpRec)
# Space stratification
if (is.na(spaceStrata)) {
  CA$space <- "all" 
  spRec <- as.list(NA)
} else {
  CA$space <- CA[,spaceStrata]}
if (!is.na(spRec[1])) CA <- recFun(CA,"space",spRec)  
# Technical stratification  :--> no technical stratification for linked ca and tr (that are not linked to hh, sl & hl)
CA$technical <- NA
# PSUid : combination of trip code, time, space and technical stratification
psuid <- apply(CA[,c("sampType","landCtry","vslFlgCtry","proj","trpCode","time","space","technical")],1,paste,collapse=":-:")    
CA$PSUid <- mapvalues(psuid,from=unique(psuid),to=1:length(unique(psuid)))
CA$PSUid <- as.numeric(as.character(CA$PSUid))
# SSUid : cst for each PSUid in that particular case
CA$SSUid <- 1  
# addition of the sorting stratification fields
fields <- c("catchCat","landCat","commCatScl")
CA$sort <- apply(CA[,fields],1,paste,collapse="-")
# Addition of fields in TR
# merge TR and CA by the keyfields to include PSUid, time, space and technical fields in TR
tr <- merge(object@tr,unique(CA[,c("sampType",
                            "landCtry",
                            "vslFlgCtry",
                            "year",
                            "proj",
                            "trpCode",
                            "PSUid",
                            "time",
                            "space",
                            "technical")]),sort=FALSE,all.x=TRUE)  #no technical strata for tr table part that is only linked to ca  
#ordering by PSUid field
tr <- tr[order(tr$PSUid),]
# Creation of the CONSOLIDATED object
# selection of the appropriate fields (selection of the new stratification fields 
# instead of the original) in the tables created above
csc <- methods::new("csDataCons")
tr <- tr[,match(names(tr(csc)),names(tr))]
rownames(tr) <- 1:nrow(tr)
ca <- CA[,match(names(ca(csc)),names(CA))]
rownames(ca) <- 1:nrow(ca)  
methods::new("csDataCons",desc=desc,tr=coerceCons(tr,csc@tr),ca=coerceCons(ca,csc@ca))
}

#' csDataCons constructors
#' @param object \link{csDataVal-class} object
#' @param objStrat stratification \link{strIni-class} object
#' @param desc character vector of stock information
#' @param \\dots parameters
#' @rdname csDataCons-constructors
#' @export
setGeneric("csDataCons", function(object,objStrat,...){
	standardGeneric("csDataCons")
})

#' @rdname csDataCons-constructors 
#{{{
setMethod("csDataCons", signature("csDataVal","strIni"), function(object,
                                                                  objStrat,
                                                                  desc="Unknown stock",  
                                                                  ...){  

#if no information in hh, only tr & ca tables are supposed to be filled, so...
if (nrow(object@hh)==1 & all(is.na(object@hh))) {
  csDataConsCATR(object=object,objStrat=objStrat,desc=desc)
} else {
timeStrata <- objStrat@timeStrata              # 
spaceStrata <- objStrat@spaceStrata            #
techStrata <- objStrat@techStrata              #
tpRec <- objStrat@tpRec                        #
spRec <- objStrat@spRec                        #
tcRec <- objStrat@tcRec                        #
HH <- HHca <- object@hh
# Addition of fields in HH
# if technical stratification is Commercial category, sampType must be "M" or "D", and HH lines are duplicated according to sampled category  
cc <- is.na(techStrata)==FALSE & techStrata=="commCat"
if (cc) {
if (!all(HH$sampType%in%c("M","D"))) stop("for cc strategy, all data must be market sampling data!!")
HH <- merge(HH,unique(object@sl[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode","staNum","spp","catchCat","landCat","commCatScl","commCat")]))
}
# if tech strata = vslPwr, vslSize, vslLen, vslType
# this information stored in tr must be written in hh
vsl <- is.na(techStrata)==FALSE & techStrata%in%c("vslPwr","vslSize","vslLen","vslType")
if (vsl) { 
  indHh <- apply(HH[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode")],1,paste,collapse=":-:") 
  indTr <- apply(object@tr[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode")],1,paste,collapse=":-:") 
  HH[,techStrata] <- object@tr[match(indHh,indTr),techStrata]
}
    #---------------------------------------------------------------------------
    # Addition of fields based on the user specification and the post-stratification specified in the strIni object
    #---------------------------------------------------------------------------
        #-------
        # Time stratification
        #-------
# Creation of a data.frame containing the different modalities of time stratification
month <- sapply(HH$date,function(x) as.numeric(strsplit(x,"-")[[1]][2]))
Year <- HH$year
time.DF <- data.frame(month = paste(as.character(Year),as.character(ceiling(month/3)),as.character(month),sep=" - "), 
                      quarter = paste(as.character(Year),as.character(ceiling(month/3)),sep=" - "),
                      semester = paste(as.character(Year),as.character(ceiling(month/6)),sep=" - "),
                      year=Year)       

if (is.na(timeStrata)) {
  HH$time <- "all" 
  tpRec <- as.list(NA)
} else {
  HH$time <- time.DF[,timeStrata]}  

if (!is.na(tpRec[1])) HH <- recFun(HH,"time",tpRec)

        #-------
        # Space stratification
        #-------
       
if (is.na(spaceStrata)) {
  HH$space <- "all" 
  spRec <- as.list(NA)
} else {
  HH$space <- HH[,spaceStrata]}
  
if (!is.na(spRec[1])) HH <- recFun(HH,"space",spRec)  
  
        #-------
        # Technical stratification
        #-------
    
if (is.na(techStrata)) {
  HH$technical <- "all" 
  tcRec <- as.list(NA)
} else {
  HH$technical <- HH[,techStrata]}

if (!is.na(tcRec[1])) HH <- recFun(HH,"technical",tcRec)

if (cc) HH <- unique(HH[,-match("commCat",names(HH))])  #if commercial category recoding was made              #modif MM 27/03/2009
        #------- 
        # PSUid : combination of trip code, time, space and technical stratification
        #-------

psuid <- apply(HH[,c("sampType","landCtry","vslFlgCtry","proj","trpCode","time","space","technical")],1,paste,collapse=":-:")    
HH$PSUid <- mapvalues(psuid,from=unique(psuid),to=1:length(unique(psuid)))
HH$PSUid <- as.numeric(as.character(HH$PSUid))

        #------- 
        # SSUid : basically, one SSUid per HH row related to PSUid
        #-------

HH <- HH[order(HH$PSUid),]                                    #Need to be ordered like the result of SSUid below                                         
HH$SSUid <- unlist(tapply(HH$PSUid,list(HH$PSUid),function(x) 1:length(x)))


#-------------------------------------------------------------------------------
# Addition of fields in SL
#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # merge SL and HH by the keyfields to include PSUid, SSUid, time, space and technical fields in SL
    #---------------------------------------------------------------------------

if (cc) {object@sl$technical <- object@sl$commCat ; object@sl <- recFun(object@sl,"technical",tcRec)}      

sl <- merge(object@sl,HH[,c("sampType",
                     "landCtry",
                     "vslFlgCtry",
                     "year",
                     "proj",
                     "trpCode",
                     "staNum",
                     "time",
                     "space",
                     "technical",
                     "PSUid",
                     "SSUid")],sort=FALSE,all.x=TRUE)
      
    #---------------------------------------------------------------------------
    # creation of the sorting stratification : concatenation of 5 fields related to the categorisation 
    # of the catch (important: this field keeps track of the original values)
    #---------------------------------------------------------------------------
      
fields <- c("catchCat","landCat","commCatScl","subSampCat")    #"commCat",      #modif MM 27/03/2009
sl$sort <- apply(sl[,fields],1,paste,collapse="-")

    #---------------------------------------------------------------------------
    # creation of TSUid : basically, one TSUid per line of PSUid * SSUid * sorting level 
    # (= sorting stratification specified above without the sub-sampling considerations and with the recoded values if needed)
    #---------------------------------------------------------------------------

#Need to be ordered like the result of TSUid below     
sl <- sl[order(sl$PSUid,sl$SSUid),]                                  

#TSUid creation has been simplified by supposing that commCatScl is unique per species             #<<- 30/06/2008 update : only CC defines TSUid
                             
if (cc) {                                                                                          #<<- 30/06/2008 update : if techStrata!="commCat", then TSUid = NA
  sl$TSUid <- sl$technical                                                                         #modif MM 27/03/2009
} else {                                                                                           #
  sl$TSUid <- sl$commCat                                                                           #
}                                                                                                  #
  
#-------------------------------------------------------------------------------
# Addition of fields in HL
#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # merge HL and SL by the keyfields to include PSUid, SSUid, TSUid, sorting, time, space and technical stratification in HL
    #---------------------------------------------------------------------------

hlSex <- slSex(object@sl,object@hl)                                           #
hl <- merge(hlSex,sl[,c("sampType",                                             #08/12/2008 modif MM ('sex' as partial key field in SL)  : object@hl <-> hlSex
                     "landCtry",
                     "vslFlgCtry",
                     "year",
                     "proj",
                     "trpCode",
                     "staNum",
                     "spp",
                     "catchCat",
                     "landCat",
                     "commCatScl",
                     "commCat",
                     "subSampCat",
                     "sex",                #added 21/07/08
                     "sort",
                     "time",
                     "space",
                     "technical",
                     "PSUid",
                     "SSUid",
                     "TSUid")],sort=FALSE,all.x=TRUE)

hl$sex <- hl$lsex                                                               #08/12/2008 modif MM
hl <- hl[order(hl$PSUid,hl$SSUid,hl$TSUid),]


#-------------------------------------------------------------------------------
# Addition of fields in CA
#-------------------------------------------------------------------------------

if (!cc) {
    #---------------------------------------------------------------------------
    # merge CA and HH by the keyfields to include PSUid, SSUid, time, space and technical fields in CA
    #---------------------------------------------------------------------------

#ca & hh are merged
#presence of NAs will mean that ca information is only linked to tr table 
ca <- merge(object@ca,HH[,c("sampType",
                     "landCtry",
                     "vslFlgCtry",
                     "year",
                     "proj",
                     "trpCode",
                     "staNum",
                     "time",
                     "space",
                     "technical",
                     "PSUid",
                     "SSUid")],sort=FALSE,all.x=TRUE)

    #---------------------------------------------------------------------------
    # for the part of CA not linked to HH
    #---------------------------------------------------------------------------

#we order by ca$PSUid to put unlinked ca datas together at the bottom of the table
ca <- ca[order(ca$PSUid),]  
index <- is.na(ca$PSUid)

} else {

ca <- object@ca ; ca$time <- ca$space <- ca$technical <- ca$PSUid <- ca$SSUid <- NA 
index <- rep(TRUE,nrow(ca))

}

        #------- 
        # creation of time and space stratification fields for indexed part of ca (no technical stratification for tr/ca)
        #-------

#TIME
#'semester' field is added to ca table
#ca$semester <- ceiling(ca$quarter/2)  
Semester <- ceiling(ca$quarter/2)                                                                 #<<- 22/09/2008 update : addition of quarter information in 'month'
ca$month <- paste(as.character(ca$year),as.character(ca$quarter),as.character(ca$month),sep=" - ")#<<- 30/06/2008 update : addition of year information to "time" field                
ca$quarter <- paste(as.character(ca$year),as.character(ca$quarter),sep=" - ")                     #
ca$semester <- paste(as.character(ca$year),as.character(Semester),sep=" - ")                      #

                                                              
if (!is.na(timeStrata)) {
  ca$time <- as.character(ca$time)                                                                #<<- 30/06/2008 update 
  ca$time[index] <- ca[index,timeStrata]                                                           
  if (!is.na(tpRec[1])) ca <- recFun(ca,"time",tpRec)
} else {
  ca$time <- "all"}

#SPACE
if (!is.na(spaceStrata)) {
  ca$space[index] <- ca[index,spaceStrata]
  if (!is.na(spRec[1])) ca <- recFun(ca,"space",spRec)
} else {
  ca$space <- "all"}

     
        #------- 
        # creation of new values of PSUid
        #-------

if (any(index)) {    
# the new PSUid values must begin where current PSUid values in hh table end (because all trips from tr are supposed to be recorded in hh)
begin <- max(HH$PSUid,na.rm=TRUE)
#PSUid is defined by the combination of a trip, time, space and technical strata
psuid <- apply(ca[index,c("sampType","landCtry","vslFlgCtry","proj","trpCode","time","space","technical")],1,paste,collapse=":-:")
#ca$PSUid[index] <- as.numeric(as.character(factor(psuid,levels=unique(psuid),labels=(1:length(unique(psuid)))+begin)))
ca$PSUid[index] <- as.numeric(as.character(mapvalues(psuid,from=unique(psuid),to=(1:length(unique(psuid)))+begin)))
#SSuid field insertion in indexed ca table 
ca$SSUid[index] <- 1
}
        #------- 
        # addition of the sorting stratification fields
        #-------

fields <- c("catchCat","landCat","commCatScl") #,"commCat")                     #modif MM 27/03/2009
ca$sort <- apply(ca[,fields],1,paste,collapse="-")



#-------------------------------------------------------------------------------
# Addition of fields in TR
#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # merge TR and HH by the keyfields to include PSUid, time, space and technical fields in TR
    #---------------------------------------------------------------------------

tr <- merge(object@tr,unique(HH[,c("sampType",
                            "landCtry",
                            "vslFlgCtry",
                            "year",
                            "proj",
                            "trpCode",
                            "PSUid",
                            "time",
                            "space",
                            "technical")]),sort=FALSE,all=TRUE)


    #---------------------------------------------------------------------------
    # when the level of aggregation is "H", i.e. information is registered by fishing operation, 
    # calculation of the updated number of days at sea and number of FOs per PSUid has to be done.
    #---------------------------------------------------------------------------

HHh <- HH[HH$aggLev=="H",]
#table of updated 'number of days' and 'number of FO' fields by PSUid 
count <- function(x){length(unique(x))}
df <- data.frame(
   PSUid=tapply(HHh$PSUid,as.character(HHh$PSUid),unique),
   nOP=tapply(HHh$staNum,as.character(HHh$PSUid),count),
   nDay=tapply(HHh$date,as.character(HHh$PSUid),count)
   )
#substitution of 'number of days' and 'number of FO' fields by updated values in tr table 
index <- match(tr$PSUid,df$PSUid)
foNumTemp <- df$nOP[index] ; tr$foNum[!is.na(foNumTemp)] <- foNumTemp[!is.na(foNumTemp)]
daysAtSeaTemp <- df$nDay[index] ; tr$daysAtSea[!is.na(daysAtSeaTemp)] <- daysAtSeaTemp[!is.na(daysAtSeaTemp)]


    #---------------------------------------------------------------------------
    # update of the new values of PSUid, time, space and technical coming from CA in TR
    #---------------------------------------------------------------------------

colN <- c("sampType","landCtry","vslFlgCtry","year","proj","trpCode")    
index <- match(apply(tr[,colN],1,paste,collapse=""),apply(ca[,colN],1,paste,collapse=""))
indexTr <- (1:nrow(tr))[indic <- is.na(tr$PSUid)]  #to preserve the information already added from hh    ##10/03/2010 modif : !is.na(index) <-> is.na(tr$PSUid)
#index & indexTr are linking tr and ca datas
tr$PSUid[indexTr] <- ca$PSUid[index[indic]]
tr$time <- as.character(tr$time)                                                                #<<- 30/06/2008 update 
tr$time[indexTr] <- ca$time[index[indic]]
tr$space[indexTr] <- ca$space[index[indic]]
#no technical strata for tr table part that is only linked to ca
#ordering by PSUid field
tr <- tr[order(tr$PSUid),]


#-------------------------------------------------------------------------------
# Creation of the CONSOLIDATED object
#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # selection of the appropriate fields (selection of the new stratification fields 
    # instead of the original) in the tables created above
    #---------------------------------------------------------------------------

csc <- methods::new("csDataCons")

	tr <- tr[,match(names(tr(csc)),names(tr))]
  rownames(tr) <- 1:nrow(tr)
	
  hh <- HH[,match(names(hh(csc)),names(HH))]
  rownames(hh) <- 1:nrow(hh)
  
	sl <- sl[,match(names(sl(csc)),names(sl))]
  rownames(sl) <- 1:nrow(sl)
  
	hl <- hl[,match(names(hl(csc)),names(hl))]
  rownames(hl) <- 1:nrow(hl)
  
	ca <- ca[,match(names(ca(csc)),names(ca))]
  rownames(ca) <- 1:nrow(ca)  

if (all(is.na(object@ca))) {
methods::new("csDataCons",desc=desc,tr=coerceCons(tr,csc@tr),hh=coerceCons(hh,csc@hh),sl=coerceCons(sl,csc@sl),hl=coerceCons(hl,csc@hl))
} else {  
methods::new("csDataCons",desc=desc,tr=coerceCons(tr,csc@tr),hh=coerceCons(hh,csc@hh),sl=coerceCons(sl,csc@sl),hl=coerceCons(hl,csc@hl),ca=coerceCons(ca,csc@ca))
}
}
})



#' @rdname csDataCons-constructors
setMethod("csDataCons", signature("csDataVal","missing"), function(object,desc="Unknown stock", ...){

	csDataCons(object,strIni(),desc=desc,...)
})

#' @rdname csDataCons-constructors
setMethod("csDataCons", signature("missing","missing"), function(desc="Unknown stock", ...){

	methods::new("csDataCons", desc=desc)
})#}}}	

#====================================================================
# Accessor functions
#====================================================================

#' csDataCons methods
#' @param object,x,y,link \link{ceDataCons-class} object
#' @param subset subset expression
#' @param \\dots parameters
#' @rdname csDataCons-methods
#' @export
setMethod("tr", signature("csDataCons"), function(object, ...){ object@tr })

#' @rdname csDataCons-methods
#' @export
setMethod("hh", signature("csDataCons"), function(object, ...){ object@hh })

#' @rdname csDataCons-methods
#' @export
setMethod("sl", signature("csDataCons"), function(object, ...){ object@sl })

#' @rdname csDataCons-methods
#' @export
setMethod("hl", signature("csDataCons"), function(object, ...){ object@hl })

#' @rdname csDataCons-methods
#' @export
setMethod("ca", signature("csDataCons"), function(object, ...){ object@ca })

#' @rdname csDataCons-methods
setMethod("head", signature("csDataCons"), function(x, ...){
  object <- methods::new("csDataCons",desc=x@desc)
  object@tr <- head(x@tr)
  object@hh <- head(x@hh)
  object@sl <- head(x@sl)
  object@hl <- head(x@hl)
  object@ca <- head(x@ca)
  return(object)  
	}
)

#' @rdname csDataCons-methods
setMethod("summary", signature("csDataCons"), function(object, ...){
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

#' @rdname csDataCons-methods
setMethod("dim", signature("csDataCons"), function(x){
  ll <- list()
  ll$tr <- dim(x@tr)
  ll$hh <- dim(x@hh)
  ll$sl <- dim(x@sl)
  ll$hl <- dim(x@hl)
  ll$ca <- dim(x@ca)
  return(ll)  
	}
)

#' @rdname csDataCons-methods
setMethod("rbind2", signature(x="csDataCons", y="csDataCons"), function(x,y){
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
	# methods::new object
	methods::new("csDataCons", tr=unique(tr), hh=unique(hh), sl=unique(sl), hl=unique(hl), ca=unique(ca))
})


#' @rdname csDataCons-methods
setMethod("subset", signature(x="csDataCons"), function(x,subset,...){
    # subset each table
    e <- substitute(subset)
    tr <- tr(x)[eval(e,tr(x), parent.frame(n=2)),]
    hh <- hh(x)[eval(e,hh(x), parent.frame(n=2)),]
    sl <- sl(x)[eval(e,sl(x), parent.frame(n=2)),]
    hl <- hl(x)[eval(e,hl(x), parent.frame(n=2)),]
    ca <- ca(x)[eval(e,ca(x), parent.frame(n=2)),]
    methods::new("csDataCons",tr=tr,hh=hh,sl=sl,hl=hl,ca=ca)
})

# subsetSpp : only sl table is subset, and only hl is modified consequently
#setGeneric("subsetSpp", function(x,subset,...){
#	standardGeneric("subsetSpp")
#	}
#)
#
#' @rdname csDataCons-methods
#' @export
setMethod("subsetSpp", signature(x="csDataCons"), function(x,subset,link=TRUE,...){
  hl <- slSex(sl(x),hl(x))                                                    
  #get idx                                                                      
	hlfk <- hl[,c(1:15)]                                                          
	hlfk <- apply(hlfk,1,paste,collapse=":-:")
	hlfk <- gsub("[[:space:]]","",hlfk)
  # new idx
	e <- substitute(subset)
	df0 <- do.call("sl", list(object=x))
	r <- eval(e, df0, parent.frame(n=2))
	# subset
	sl <- df0[r,]
	slidx <- apply(sl[,1:15],1,paste,collapse=":-:")
	slidx <- gsub("[[:space:]]","",slidx)
	Hl <- hl[hlfk %in% slidx,]	                                                
  Hl$sex <- Hl$lsex ; hl <- Hl[,-ncol(Hl)]                                      
	# output
	if(nrow(sl)<1) {sl <- csDataCons()@sl                                             
                  warning("No data kept from subsetting process!!")}            
  if(nrow(hl)<1) {hl <- csDataCons()@hl}                                            
  if (link) {subset <- substitute(subset)
             ca <- subset(ca(x),eval(subset)) 
             if(nrow(ca)<1) {ca <- csDataCons()@ca
                  warning("No data kept from subsetting process in ca table!!")}  
  } else {
  ca <- ca(x)}
  methods::new("csDataCons",desc=x@desc,tr=tr(x), hh=hh(x), sl=sl, hl=hl, ca=ca)         
})




