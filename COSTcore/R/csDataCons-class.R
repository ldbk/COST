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

valcscData <- function(object){
	tr <- object@tr
	hh <- object@hh
	sl <- object@sl
	hl <- object@hl
	ca <- object@ca

	# I will rely o the prototype to check col names and size. I'm not sure it's a good strategy !
	obj <- new("csDataCons")
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
	
	# check PK (ToDo)

	# check data integrity
#	if(checkDataIntegrity(tr[,1:6], hh[,1:6])==FALSE) stop("Data integrity problem in table \"hh\". Missing related records in \"tr\".")
#	if(checkDataIntegrity(hh[,1:7], sl[,1:7])==FALSE) stop("Data integrity problem in table \"sl\". Missing related records in \"hh\".")
#	if(checkDataIntegrity(sl[,1:14], hl[,1:14])==FALSE) stop("Data integrity problem in table \"hl\". Missing related records in \"sl\".")
#	if(checkDataIntegrity(tr[,1:6], ca[,1:6])==FALSE) stop("Data integrity problem in table \"ca\". Missing related records in \"tr\".")

	# Everything is fine
	return(TRUE)
}

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
			PSUid=as.numeric(NA), # field to be created by the EDA process, one per each record = PK
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sampType=as.factor(NA), # PK
			landCtry=as.factor(NA), # PK
			vslFlgCtry=as.factor(NA), # PK
			#year=as.numeric(NA), # PK 	=> time
			proj=as.factor(NA), # PK
			trpCode=as.factor(NA), # PK
			#vslLen=as.numeric(NA), 	=> tech
			#vslPwr=as.numeric(NA), 	=> tech 
			#vslSize=as.numeric(NA), 	=> tech 
			#vsType=as.character(NA), 	=> tech
			foNum=as.numeric(NA), 
			daysAtSea=as.numeric(NA), 
			vslId=as.numeric(NA), 
			sampCtry=as.factor(NA), 
			sampMeth=as.factor(NA)),
		hh=data.frame(
			PSUid=as.numeric(NA), # FK
			SSUid=as.numeric(NA), # field to be created by the EDA process, one per each record = FK+PK
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			#year=as.numeric(NA), # PK 	=> time
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # PK
			foVal=as.character(NA),
			aggLev=as.character(NA),
			catReg=as.character(NA),
			sppReg=as.character(NA),
			date=as.character(NA), #	=> time
			#time=as.character(NA), 	=> time
			foDur=as.numeric(NA),
			latIni=as.numeric(NA),
			lonIni=as.numeric(NA),
			latFin=as.numeric(NA),
			lonFin=as.numeric(NA),
			#area=as.character(NA),	=> space
			#rect=as.character(NA),	=> space
			foDep=as.numeric(NA)
			#waterDep=as.numeric(NA), => space
			#foCatNat=as.character(NA),	=> tech
			#foCatEu5=as.character(NA),	=> tech
			#foCatEu6=as.character(NA),	=> tech
			#gear=as.character(NA),	=> tech
			#meshSize=as.numeric(NA),	=> tech
			#selDev=as.character(NA),	=> tech
			#meshSizeSelDev=as.numeric(NA)	=> tech
			),
		sl=data.frame(
			PSUid=as.numeric(NA), # FK
			SSUid=as.numeric(NA), # field to be created by the EDA process, one per each record = FK+PK
			TSUid=as.numeric(NA), # field to be created by the EDA process, one per each record = FK+PK
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sort=as.factor(NA),
			sampType=as.factor(NA), # FK
			landCtry=as.factor(NA), # FK
			vslFlgCtry=as.factor(NA), # FK
			#year=as.numeric(NA), # PK 	=> time
			proj=as.factor(NA), # FK
			trpCode=as.factor(NA), # FK
			staNum=as.numeric(NA), # FK
			spp=as.factor(NA), # PK 
			sex=as.factor(NA), # PK 
			#catchCat=as.character(NA), # PK	=> sort 
			#landCat=as.character(NA), # PK	=> sort 
			#commCatScl=as.character(NA), # PK	=> sort
			#commCat=as.character(NA), # PK	=> sort
			#subSampCat=as.character(NA), # PK	=> sort
      #valCode=as.factor(NA), 
			wt=as.numeric(NA), 
			subSampWt=as.numeric(NA), 
			lenCode=as.factor(NA)),
		hl=data.frame(
			PSUid=as.numeric(NA), # This field helps on linking with TR table
			SSUid=as.numeric(NA), # This field helps on linking with HH table 
			TSUid=as.numeric(NA), # FK
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sort=as.factor(NA),
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			#year=as.numeric(NA), # PK 	=> time
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # FK
			spp=as.character(NA), # FK 
			sex=as.character(NA), # PK
			#catchCat=as.character(NA), # PK	=> sort 
			#landCat=as.character(NA), # PK	=> sort 
			#commCatScl=as.character(NA), # PK	=> sort
			#commCat=as.character(NA), # PK	=> sort
			#subSampCat=as.character(NA), # PK	=> sort
			lenCls=as.numeric(NA), # PK
			lenNum=as.numeric(NA)),
		ca=data.frame(
			PSUid=as.numeric(NA), # FK
			SSUid=as.numeric(NA), # must match SSUid in HH so that info about tech can be used if necessary
			time=as.factor(NA),
			space=as.factor(NA),
			technical=as.factor(NA),
			sort=as.factor(NA),
			sampType=as.character(NA), # FK
			landCtry=as.character(NA), # FK
			vslFlgCtry=as.character(NA), # FK
			#year=as.numeric(NA), # FK	=> time
			#quarter=as.numeric(NA),	=> time
			#month=as.numeric(NA),	=> time
			proj=as.character(NA), # FK
			trpCode=as.character(NA), # FK
			staNum=as.numeric(NA), # PK
			spp=as.character(NA), # PK 
			sex=as.character(NA), # PK
			#catchCat=as.character(NA), # PK	=> sort 
			#landCat=as.character(NA), # PK	=> sort 
			#commCatScl=as.character(NA), # PK	=> sort
			#commCat=as.character(NA), # PK	=> sort
			stock=as.character(NA), # PK
			#area=as.character(NA),	=> space
			#rect=as.character(NA),	=> space
			lenCls=as.numeric(NA), # PK
			age=as.numeric(NA), # PK
			fishId=as.numeric(NA), # PK
			lenCode=as.character(NA),
			ageMeth=as.character(NA),   #modif MM 01/12/2008
			plusGrp=as.character(NA),
#			ageMeth=as.character(NA),
			otoWt=as.numeric(NA),
			otoSide=as.character(NA),
			indWt=as.numeric(NA),  
      matMeth=as.character(NA),    #modif MM 01/12/2008
			matScale=as.character(NA),
			matStage=as.character(NA))
#     matMeth=as.character(NA))
	),
	validity=valcscData
)

##====================================================================
## Class constructor
##====================================================================
#setGeneric("csDataCons", function(object, ...){
#	standardGeneric("csDataCons")
#	}
#)
#
#setMethod("csDataCons", signature("csDataVal"), function(object, desc="Unknown stock", ...){
#
#	#------------------------------------------------------------------------------
#	# create SUid
#	#------------------------------------------------------------------------------
#	suid <- createSUid(object)
#
#	#------------------------------------------------------------------------------
#	# time
#	#------------------------------------------------------------------------------
#	# hh
#	hh <- suid$hh
#	tm <- as.Date(hh$date)
#	tm <- paste(hh$year,quarters(tm),sep=".")
#	hh$time <- tm
#	# tr
#	tm <- unique(suid$hh[,c("PSUid", "time")])
#	tr <- merge(suid$tr, tm, all.x=TRUE)
#	# sl
#	sl <- suid$sl
#	sl$time <- hh$time[match(sl$SSUid,hh$SSUid)]
#	# hl
#	hl <- suid$hl
#	hl$time <- hh$time[match(hl$SSUid,hh$SSUid)]
#	# ca
#	ca <- suid$ca
#	ca$time <- paste(ca$year, paste("Q", ca$quarter, sep=""), sep=".")
#	
#	#------------------------------------------------------------------------------
#	# tech
#	#------------------------------------------------------------------------------
#	te1 <- apply(tr[,c("vslLen", "vslPwr", "vslSize", "vslType")], 1,paste, collapse=".")
#	te2 <- apply(hh[,c("foCatNat","foCatEu5","foCatEu6","gear","meshSize","selDev","meshSizeSelDev")], 1,paste, collapse=".")
#	#hh
#	hh$technical <- paste(te1[match(hh$PSUid,tr$PSUid)], te2, sep="-")
#	# tr
#	te <- unique(hh[,c("PSUid", "technical")])
#	tr <- merge(tr, te, all.x=TRUE)
#	# sl
#	sl$technical <- hh$technical[match(sl$SSUid,hh$SSUid)]
#	# hl
#	hl$technical <- hh$technical[match(hl$SSUid,hh$SSUid)]
#	# ca
#	ca$technical <- hh$technical[match(ca$SSUid,hh$SSUid)]
#	
#	# Not relevant
#	
#	#------------------------------------------------------------------------------
#	# space
#	#------------------------------------------------------------------------------
#	# hh
#	hh$space <- apply(hh[,c("area","rect","waterDep")], 1,paste, collapse=".") 
#	# tr
#	# not relevant
#	# sl
#	sl$space <- hh$space[match(sl$SSUid,hh$SSUid)]
#	# hl
#	hl$space <- hh$space[match(hl$SSUid,hh$SSUid)]
#	# ca
#	ca$space <- apply(ca[,c("area","rect")], 1,paste, collapse=".")
#	
#	#------------------------------------------------------------------------------
#	# sort
#	#------------------------------------------------------------------------------
#	# sl
#	sl$sort <- apply(sl[,c("catchCat","landCat","commCatScl", "commCat", "subSampCat")], 1,paste, collapse=".")
#	# hl
#	hl$sort <- sl$sort[match(hl$TSUid,sl$TSUid)]
#	# ca
#	ca$sort <- apply(ca[,c("catchCat","landCat","commCatScl", "commCat")], 1,paste, collapse=".")
#	
#	#------------------------------------------------------------------------------
#	# create csDataCons
#	#------------------------------------------------------------------------------
#	csc <- csDataCons()
#	tr <- tr[,match(names(tr(csc)),names(tr))]
#	hh <- hh[,match(names(hh(csc)),names(hh))]
#	sl <- sl[,match(names(sl(csc)),names(sl))]
#	hl <- hl[,match(names(hl(csc)),names(hl))]
#	ca <- ca[,match(names(ca(csc)),names(ca))]
#	new("csDataCons", tr=tr, hh=hh, sl=sl, hl=hl, ca=ca)
#})
#
#
#setMethod("csDataCons", signature("missing"), function(desc="Unknown stock", ...){
#	# create object and name columns properly 
#	new("csDataCons", desc=desc)
#})
#


##====================================================================
## Class constructor for validated objects with only tr & ca tables filled (not to be exported)
##====================================================================


csDataConsCATR <- function(object,                 #no hh, sl & hl tables in validated object
                           objStrat,
                           desc="Unknown stock",  
                           ...){  

timeStrata <- objStrat@timeStrata              # <<<- to make the code clearer, but maybe it's not the thing to do
spaceStrata <- objStrat@spaceStrata            #
techStrata <- objStrat@techStrata              #
tpRec <- objStrat@tpRec                        #
spRec <- objStrat@spRec                        #
tcRec <- objStrat@tcRec                        #

CA <- ca(object)

#recoding procedure
recFun <- function(df,field,rec) {                  
  Typ <- class(df[,field]) 
  fc <- factor(df[,field]) 
  #Lev <- levels(fc)[!levels(fc)%in%rec$from]
  #df[,field] <- factor(fc,levels=c(Lev,rec$from),labels=c(Lev,rec$to))
  df[,field] <- mapvalues(fc,from=rec$from,to=rec$to)
  eval(parse('',text=paste("df[,field] <- as.",Typ,"(as.character(df[,field]))",sep="")))
#	require(dplyr)
#	print("youpi")
#	df[,field]<-mapvalues(df[,field],from=rec$from,to=rec$to)
  return(df)
}
  
        #-------
        # Time stratification
        #-------
  
# 'semester' field is put in ca
#CA$semester <- ceiling(CA$quarter/2)
Semester <- ceiling(CA$quarter/2) 
CA$month <- paste(as.character(CA$year),as.character(CA$quarter),as.character(CA$month),sep=" - ")#<<- 22/09/2008 update : addition of quarter in 'month' information               
CA$quarter <- paste(as.character(CA$year),as.character(CA$quarter),sep=" - ")                     #<<- 30/06/2008 update : addition of year information to "time" field 
CA$semester <- paste(as.character(CA$year),as.character(Semester),sep=" - ")                      #
 

if (is.na(timeStrata)) {
  CA$time <- "all" 
  tpRec <- as.list(NA)
} else {
  CA$time <- CA[,timeStrata]}  

if (!is.na(tpRec[1])) CA <- recFun(CA,"time",tpRec)

        #-------
        # Space stratification
        #-------
       
if (is.na(spaceStrata)) {
  CA$space <- "all" 
  spRec <- as.list(NA)
} else {
  CA$space <- CA[,spaceStrata]}
  
if (!is.na(spRec[1])) CA <- recFun(CA,"space",spRec)  
  
        #-------
        # Technical stratification  :--> no technical stratification for linked ca and tr (that are not linked to hh, sl & hl)
        #-------
    
CA$technical <- NA

        
        #------- 
        # PSUid : combination of trip code, time, space and technical stratification
        #-------

psuid <- apply(CA[,c("sampType","landCtry","vslFlgCtry","proj","trpCode","time","space","technical")],1,paste,collapse=":-:")    
#CA$PSUid <- factor(psuid,levels=unique(psuid),labels=1:length(unique(psuid)))
CA$PSUid <- mapvalues(psuid,from=unique(psuid),to=1:length(unique(psuid)))
CA$PSUid <- as.numeric(as.character(CA$PSUid))

        #------- 
        # SSUid : cst for each PSUid in that particular case
        #-------
                                      
CA$SSUid <- 1  

        #------- 
        # addition of the sorting stratification fields
        #-------

fields <- c("catchCat","landCat","commCatScl")#,"commCat")
CA$sort <- apply(CA[,fields],1,paste,collapse="-")



#-------------------------------------------------------------------------------
# Addition of fields in TR
#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # merge TR and CA by the keyfields to include PSUid, time, space and technical fields in TR
    #---------------------------------------------------------------------------

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

 
#'number of days' and 'number of FO' fields should be updated in tr table according to stratification and ca table --> TO DO



#-------------------------------------------------------------------------------
# Creation of the CONSOLIDATED object
#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # selection of the appropriate fields (selection of the new stratification fields 
    # instead of the original) in the tables created above
    #---------------------------------------------------------------------------

csc <- new("csDataCons")

	tr <- tr[,match(names(tr(csc)),names(tr))]
  rownames(tr) <- 1:nrow(tr)
	
	ca <- CA[,match(names(ca(csc)),names(CA))]
  rownames(ca) <- 1:nrow(ca)  
  
new("csDataCons",desc=desc,tr=coerceCons(tr,csc@tr),ca=coerceCons(ca,csc@ca))

}




##====================================================================
## Class constructor
##====================================================================



setGeneric("csDataCons", function(object,objStrat,...){
	standardGeneric("csDataCons")
})


setMethod("csDataCons", signature("csDataVal","strIni"), function(object,
                                                                  objStrat,
                                                                  desc="Unknown stock",  
                                                                  ...){  

#if no information in hh, only tr & ca tables are supposed to be filled, so...
if (nrow(object@hh)==1 & all(is.na(object@hh))) {
  
  csDataConsCATR(object=object,objStrat=objStrat,desc=desc)

#else...
} else {

timeStrata <- objStrat@timeStrata              # <<<- to make the code clearer, but maybe it's not the thing to do
spaceStrata <- objStrat@spaceStrata            #
techStrata <- objStrat@techStrata              #
tpRec <- objStrat@tpRec                        #
spRec <- objStrat@spRec                        #
tcRec <- objStrat@tcRec                        #
                                               #
HH <- HHca <- object@hh                                ####


#-------------------------------------------------------------------------------
# Addition of fields in HH
#-------------------------------------------------------------------------------

    #---------------------------------------------------------------------------
    # if technical stratification is Commercial category, sampType must be "M" or "D", and HH lines are duplicated according to sampled category  
    #---------------------------------------------------------------------------
    
cc <- is.na(techStrata)==FALSE & techStrata=="commCat"

if (cc) {

if (!all(HH$sampType%in%c("M","D"))) stop("for cc strategy, all data must be market sampling data!!")
HH <- merge(HH,unique(object@sl[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode","staNum","spp","catchCat","landCat","commCatScl","commCat")]))

}

    #---------------------------------------------------------------------------
    # if tech strata = vslPwr, vslSize, vslLen, vslType
    # this information stored in tr must be written in hh
    #---------------------------------------------------------------------------

vsl <- is.na(techStrata)==FALSE & techStrata%in%c("vslPwr","vslSize","vslLen","vslType")
if (vsl) { 
  indHh <- apply(HH[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode")],1,paste,collapse=":-:") 
  indTr <- apply(object@tr[,c("sampType","landCtry","vslFlgCtry","year","proj","trpCode")],1,paste,collapse=":-:") 
  HH[,techStrata] <- object@tr[match(indHh,indTr),techStrata]
}

    #---------------------------------------------------------------------------
    # Addition of fields based on the user specification and the post-stratification specified in the strIni object
    #---------------------------------------------------------------------------

#recoding procedure
recFun <- function(df,field,rec) {                  # <<<- there's surely a more simple way to do this
  Typ <- class(df[,field]) 
  fc <- factor(df[,field]) 
  #Lev <- levels(fc)[!levels(fc)%in%rec$from]
  #df[,field] <- factor(fc,levels=c(Lev,rec$from),labels=c(Lev,rec$to))
  df[,field] <- mapvalues(fc,from=rec$from,to=rec$to)
  eval(parse('',text=paste("df[,field] <- as.",Typ,"(as.character(df[,field]))",sep="")))
#	require(dplyr)
#	print("youpi")
#	df[,field]<-mapvalues(df[,field],from=rec$from,to=rec$to)
  return(df)
}
  
        #-------
        # Time stratification
        #-------
  
# Creation of a data.frame containing the different modalities of time stratification
month <- sapply(HH$date,function(x) as.numeric(strsplit(x,"-")[[1]][2]))
#time.DF <- data.frame(month = month, quarter = ceiling(month/3), semester = ceiling(month/6),year=HH$year)    
Year <- HH$year                                                                                                       #<<- 22/09/2008 update : addition of quarter information in 'month'
time.DF <- data.frame(month = paste(as.character(Year),as.character(ceiling(month/3)),as.character(month),sep=" - "), #<<- 30/06/2008 update : addition of year information to "time" field
                      quarter = paste(as.character(Year),as.character(ceiling(month/3)),sep=" - "),                   #eg "2006 - 1"
                      semester = paste(as.character(Year),as.character(ceiling(month/6)),sep=" - "),                  #WARNING : this format will have to be taken into account in recoding process
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
#HH$PSUid <- factor(psuid,levels=unique(psuid),labels=1:length(unique(psuid)))
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

if (cc) {object@sl$technical <- object@sl$commCat ; object@sl <- recFun(object@sl,"technical",tcRec)}         #modif MM 27/03/2009

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

csc <- new("csDataCons")

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
new("csDataCons",desc=desc,tr=coerceCons(tr,csc@tr),hh=coerceCons(hh,csc@hh),sl=coerceCons(sl,csc@sl),hl=coerceCons(hl,csc@hl))
} else {  
new("csDataCons",desc=desc,tr=coerceCons(tr,csc@tr),hh=coerceCons(hh,csc@hh),sl=coerceCons(sl,csc@sl),hl=coerceCons(hl,csc@hl),ca=coerceCons(ca,csc@ca))
}
}
})





setMethod("csDataCons", signature("csDataVal","missing"), function(object,desc="Unknown stock", ...){

	csDataCons(object,strIni(),desc=desc,...)
})

setMethod("csDataCons", signature("missing","missing"), function(desc="Unknown stock", ...){

	new("csDataCons", desc=desc)
})	





#====================================================================
# Accessor functions
#====================================================================

setMethod("tr", signature("csDataCons"), function(object, ...){
	object@tr
	}
)

setMethod("hh", signature("csDataCons"), function(object, ...){
	object@hh
	}
)

setMethod("sl", signature("csDataCons"), function(object, ...){
	object@sl
	}
)

setMethod("hl", signature("csDataCons"), function(object, ...){
	object@hl
	}
)

setMethod("ca", signature("csDataCons"), function(object, ...){
	object@ca
	}
)

setMethod("desc", signature("csDataCons"), function(object, ...){
	object@desc
	}
)

#====================================================================
# 'Head' and 'Tail' functions
#====================================================================

setMethod("head", signature("csDataCons"), function(x, ...){
  object <- new("csDataCons",desc=x@desc)
  object@tr <- head(x@tr)
  object@hh <- head(x@hh)
  object@sl <- head(x@sl)
  object@hl <- head(x@hl)
  object@ca <- head(x@ca)
  return(object)  
	}
)

setMethod("tail", signature("csDataCons"), function(x, ...){
  object <- new("csDataCons",desc=x@desc)
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

#====================================================================
# 'dim' function
#====================================================================

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

#====================================================================
# 'is.' function
#====================================================================

setGeneric("is.csDataCons", function(object){
	standardGeneric("is.csDataCons")
})


setMethod("is.csDataCons","ANY", function(object){
	return(is(object, "csDataCons"))
})

#====================================================================
# rbind2
#====================================================================

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
	tr <- rbind2(tr1,tr2)
	hh <- rbind2(hh1,hh2)
	sl <- rbind2(sl1,sl2)
	hl <- rbind2(hl1,hl2)
	ca <- rbind2(ca1,ca2)

	# new object
	new("csDataCons", tr=unique(tr), hh=unique(hh), sl=unique(sl), hl=unique(hl), ca=unique(ca))
})

#====================================================================
# subset
#====================================================================

setMethod("subset", signature(x="csDataCons"), function(x,subset,...){
   
    # subset each table
    e <- substitute(subset)
    tr <- tr(x)[eval(e,tr(x), parent.frame(n=2)),]
    hh <- hh(x)[eval(e,hh(x), parent.frame(n=2)),]
    sl <- sl(x)[eval(e,sl(x), parent.frame(n=2)),]
    hl <- hl(x)[eval(e,hl(x), parent.frame(n=2)),]
    ca <- ca(x)[eval(e,ca(x), parent.frame(n=2)),]

    new("csDataCons",tr=tr,hh=hh,sl=sl,hl=hl,ca=ca)
})






#setMethod("subset", signature(x="csDataCons"), function(x,subset,..., table="tr"){
#
#	# get idx
#	trpk <- tr(x)$PSUid
#	hhfk <- hh(x)$PSUid
#	hhpk <- hh(x)$SSUid
#	slfk <- sl(x)$SSUid
#	slpk <- sl(x)$TSUid
#	hlfk <- hl(x)$TSUid
#	cafk <- ca(x)$PSUid
#	cafk2 <- ca(x)$SSUid
#	
#	# new idx
#	e <- substitute(subset)
#	df0 <- do.call(table, list(object=x))
#	r <- eval(e, df0, parent.frame())
#	
#	# subset
#	if(table=="tr"){
#		tr <- df0[r,]
#		hh <- hh[hh$PSUid %in% tr$PSUid]
#		sl <- sl[sl$SSUid %in% hh$SSUid]
#		hl <- hl[hl$TSUid %in% sl$TSUid]
#		ca <- ca[ca$PSUid %in% tr$PSUid]
#	} else if (table=="hh"){
#		hh <- df0[r,]
#		tr <- tr[tr$PSUid %in% unique(hh$PSUid)]
#		sl <- sl[sl$SSUid %in% hh$SSUid]
#		hl <- hl[hl$TSUid %in% sl$TSUid]
#		ca <- ca[ca$PSUid %in% tr$PSUid]
#	} else if(table=="sl"){
#		sl <- df0[r,]
#		tr <- tr[tr$PSUid %in% unique(sl$PSUid)]
#		hh <- hh[hh$SSUid %in% unique(sl$SSUid)]
#		hl <- hl[hl$TSUid %in% sl$TSUid]
#		ca <- ca[ca$PSUid %in% tr$PSUid]
#	} else if(table=="hl"){
#		hl <- df0[r,]
#		tr <- tr[tr$PSUid %in% unique(hl$PSUid)]
#		hh <- hh[hh$SSUid %in% unique(hl$SSUid)]
#		sl <- sl[sl$TSUid %in% unique(hl$TSUid)]
#		ca <- ca[ca$PSUid %in% tr$PSUid]
#	} else if(table=="ca"){
#		ca <- df0[r,]
#		tr <- tr[tr$PSUid %in% unique(ca$PSUid)]
#		hh <- hh[hh$PSUid %in% tr$PSUid]
#		sl <- sl[sl$SSUid %in% hh$SSUid]
#		hl <- hl[hl$TSUid %in% sl$TSUid]
#	}		
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

#====================================================================
# replacement
#====================================================================
#
#setMethod("csDataCons", signature("csDataVal"), function(object, desc="Unknown stock", ...){
#
#	#------------------------------------------------------------------------------
#	# create SUid
#	#------------------------------------------------------------------------------
#	suid <- createSUid(object)
#
#	#------------------------------------------------------------------------------
#	# time
#	#------------------------------------------------------------------------------
#	# hh
#	hh <- suid$hh
#	tm <- as.Date(hh$date)
#	tm <- paste(hh$year,quarters(tm),sep=".")
#	hh$time <- tm
#	# tr
#	tm <- unique(suid$hh[,c("PSUid", "time")])
#	tr <- merge(suid$tr, tm, all.x=TRUE)
#	# sl
#	sl <- suid$sl
#	sl$time <- hh$time[match(sl$SSUid,hh$SSUid)]
#	# hl
#	hl <- suid$hl
#	hl$time <- hh$time[match(hl$SSUid,hh$SSUid)]
#	# ca
#	ca <- suid$ca
#	ca$time <- paste(ca$year, paste("Q", ca$quarter, sep=""), sep=".")
#	
#	#------------------------------------------------------------------------------
#	# tech
#	#------------------------------------------------------------------------------
#	te1 <- apply(tr[,c("vslLen", "vslPwr", "vslSize", "vslType")], 1,paste, collapse=".")
#	te2 <- apply(hh[,c("foCatNat","foCatEu5","foCatEu6","gear","meshSize","selDev","meshSizeSelDev")], 1,paste, collapse=".")
#	#hh
#	hh$technical <- paste(te1[match(hh$PSUid,tr$PSUid)], te2, sep="-")
#	# tr
#	te <- unique(hh[,c("PSUid", "technical")])
#	tr <- merge(tr, te, all.x=TRUE)
#	# sl
#	sl$technical <- hh$technical[match(sl$SSUid,hh$SSUid)]
#	# hl
#	hl$technical <- hh$technical[match(hl$SSUid,hh$SSUid)]
#	# ca
#	ca$technical <- hh$technical[match(ca$SSUid,hh$SSUid)]
#	
#	# Not relevant
#	
#	#------------------------------------------------------------------------------
#	# space
#	#------------------------------------------------------------------------------
#	# hh
#	hh$space <- apply(hh[,c("area","rect","waterDep")], 1,paste, collapse=".") 
#	# tr
#	# not relevant
#	# sl
#	sl$space <- hh$space[match(sl$SSUid,hh$SSUid)]
#	# hl
#	hl$space <- hh$space[match(hl$SSUid,hh$SSUid)]
#	# ca
#	ca$space <- apply(ca[,c("area","rect")], 1,paste, collapse=".")
#	
#	#------------------------------------------------------------------------------
#	# sort
#	#------------------------------------------------------------------------------
#	# sl
#	sl$sort <- apply(sl[,c("catchCat","landCat","commCatScl", "commCat", "subSampCat")], 1,paste, collapse=".")
#	# hl
#	hl$sort <- sl$sort[match(hl$TSUid,sl$TSUid)]
#	# ca
#	ca$sort <- apply(ca[,c("catchCat","landCat","commCatScl", "commCat")], 1,paste, collapse=".")
#	
#	#------------------------------------------------------------------------------
#	# create csDataCons
#	#------------------------------------------------------------------------------
#	csc <- csDataCons()
#	tr <- tr[,match(names(tr(csc)),names(tr))]
#	hh <- hh[,match(names(hh(csc)),names(hh))]
#	sl <- sl[,match(names(sl(csc)),names(sl))]
#	hl <- hl[,match(names(hl(csc)),names(hl))]
#	ca <- ca[,match(names(ca(csc)),names(ca))]
#	new("csDataCons", tr=tr, hh=hh, sl=sl, hl=hl, ca=ca)
#})
#
#


#====================================================================
# MM 19/03/09
# subsetSpp : only sl table is subset, and only hl is modified consequently
#====================================================================



#setGeneric("subsetSpp", function(x,subset,...){
#	standardGeneric("subsetSpp")
#	}
#)
#
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
             if(nrow(ca)<1) {ca <- csDataCons()@ca                                             #modif 16/04/2009
                  warning("No data kept from subsetting process in ca table!!")}  
  } else {
  ca <- ca(x)}
 


new("csDataCons",desc=x@desc,tr=tr(x), hh=hh(x), sl=sl, hl=hl, ca=ca)         

})




