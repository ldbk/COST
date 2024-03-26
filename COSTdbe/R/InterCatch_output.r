# Convert COST dbeOutput object to InterCatch Exchange Format file
# v0.1 3 April 2009
#InterCatch Exchange Format Version 1.4
#http://www.ices.dk/datacentre/InterCatch/IC-ExchFormat1-0%20Doc1-4.pdf

#######################################

makeICfile <- function (ICdfs, filename, append=FALSE) {

# ICdfs is a list of data.frames: HI, SI, and usually SD
HI <- ICdfs$HI
SI <- ICdfs$SI
SD <- ICdfs$SD
SDpresent <- !is.null(SD)

# I think Key fields are columns 2-8, using same collapse string as in other COST code
keyHI = apply(HI[,2:8],1,function(x) paste(x,collapse=":-:"))
keySI = apply(SI[,2:8],1,function(x) paste(x,collapse=":-:"))
if(SDpresent) keySD = apply(SD[,2:8],1,function(x) paste(x,collapse=":-:"))

# Output file
# write file types in required order, loop by strata defined by Key fields

if(!append) write.table(NULL, filename, col.names=FALSE, quote=FALSE) # create empty file
  for (i in keyHI){
  write.table(HI[keyHI == i,,drop=FALSE], filename, row.names=FALSE, col.names=FALSE, quote=FALSE, sep=",", append=TRUE)
  write.table(SI[keySI == i,,drop=FALSE], filename, row.names=FALSE, col.names=FALSE, quote=FALSE, sep=",", append=TRUE)
  if(SDpresent) write.table(SD[keySD == i,,drop=FALSE], filename, row.names=FALSE, col.names=FALSE, quote=FALSE, sep=",", append=TRUE)
  }
} # end of makeICfile function


#############################


makeICdf <- function (dbeOutput, filename=NA, output.df = TRUE, append = FALSE,
 CANUMtype = "length", Country, Species = NA, ReportingCategory,
  Usage = NA, DataToFrom = NA, SamplesOrigin,
 InfoFleet = NA, InfoStockCoordinator = NA, InfoGeneral = NA, Sex = "N", PlusGroup = -9, unitMeanWeight = "g",
 unitCANUM = "n", Maturity = NA
  ) {

# Writes an InterCatch format file to filename, unless filename = NA

# If output.df = TRUE  a list with 3 data.frames HI, SI, SD is produced
# this can be edited and then converted to an InterCatch format file using makeICfile

# CANUMtype specifies whether data are reported by age (age) or length (lngt)
CANUMdat <- switch(CANUMtype, age = dbeOutput@ageStruc$estim, lngt=, length = dbeOutput@lenStruc$estim)
if (is.null(CANUMdat)) stop(paste("No data of CANUMtype",CANUMtype))

varNUMdat <- switch(CANUMtype, age = dbeOutput@ageVar, lngt=, length = dbeOutput@lenVar)
UnitAgeOrLength <- switch(CANUMtype, age = "year", lngt=, length = "mm")

# species (as scientific name) from data if code not supplied
if (is.na(Species))Species <- dbeOutput@species

# Header Information
HI <- data.frame (
  RecordType = "HI" ,
  Country ,
  Year =  substr (dbeOutput@totalW$estim$time, start=1, stop=4) ,

  SeasonType = dbeOutput@strataDesc@timeStrata ,
  Season = sapply (strsplit (as.character (dbeOutput@totalW$estim$time), " - "), FUN = function(x){x[2]} ) ,
  Fleet = as.character (dbeOutput@totalW$estim$technical) ,

  AreaType = dbeOutput@strataDesc@spaceStrata ,
  FishingArea = as.character (dbeOutput@totalW$estim$space)  ,
  DepthRange = NA ,
  # dbeOutput does not contain effort, could include this is link to CL data
  UnitEffort = NA ,
  Effort = NA ,
  AreaQualifier = NA,
  stringsAsFactors = FALSE  ) # end of HI data.frame

# Change case of values to match InterCatch definition
HI <- within (HI, {
 SeasonType[SeasonType == "month"] <- "Month"
 SeasonType[SeasonType == "quarter"] <- "Quarter"
 SeasonType[SeasonType == "year"] <- "Year"
  # if AreaType is "area", assume this means AreaTop not SubArea
 AreaType [ AreaType %in% c("area", "Area", "areatop", "Areatop" )] <- "AreaTop"
 AreaType [ AreaType %in% c("div", "division", "Division" )] <- "Div"
 AreaType [ AreaType %in% c("rect", "Rectangle", "rectangle", "statrect", "StatRect", "Statrect", "statrec", "Statrec" )] <- "StatRec"
 AreaType [ AreaType %in% c("subarea", "Subarea" )] <- "SubArea"
 AreaType [ AreaType %in% c("subdiv", "Subdiv" )] <- "SubDiv"
 } # end of expression
 ) # end of within call

# Species Information

SI <- data.frame (
  RecordType = "SI" ,
  Country,
  Year = substr (dbeOutput@totalW$estim$time, start=1, stop=4) ,     #MM 15/04/2010   totalN-->totalW
  SeasonType = dbeOutput@strataDesc@timeStrata ,
  Season = sapply (strsplit (as.character (dbeOutput@totalW$estim$time), " - "), FUN = function(x){x[2]} ) ,
  Fleet = as.character (dbeOutput@totalW$estim$technical) ,

  AreaType = dbeOutput@strataDesc@spaceStrata ,
  FishingArea = as.character (dbeOutput@totalW$estim$space)  ,
  DepthRange = NA ,
  Species ,
  Stock = NA,
  CatchCategory = toupper(dbeOutput@catchCat)  ,            #modif MM 29/04/2010
  ReportingCategory  ,
  DataToFrom ,
  Usage ,
  SamplesOrigin ,
  QualityFlag = NA ,
  UnitCATON = "kg",
  CATON = dbeOutput@totalW$estim$value ,
  OffLandings = NA , # Official landings not part of dbeObject, could provide from CL object
  varCATON = dbeOutput@totalWvar$value ,
  InfoFleet,
  InfoStockCoordinator,
  InfoGeneral,
  stringsAsFactors = FALSE  ) # end of SI data.frame

# Change case of values to match InterCatch definition
SI <- within (SI, {
 SeasonType[SeasonType == "month"] <- "Month"
 SeasonType[SeasonType == "quarter"] <- "Quarter"
 SeasonType[SeasonType == "year"] <- "Year"
  # if AreaType is "area", assume this means AreaTop not SubArea
 AreaType [ AreaType %in% c("area", "Area", "areatop", "Areatop" )] <- "AreaTop"
 AreaType [ AreaType %in% c("div", "division", "Division" )] <- "Div"
 AreaType [ AreaType %in% c("rect", "Rectangle", "rectangle", "statrect", "StatRect", "Statrect", "statrec", "Statrec" )] <- "StatRec"
 AreaType [ AreaType %in% c("subarea", "Subarea" )] <- "SubArea"
 AreaType [ AreaType %in% c("subdiv", "Subdiv" )] <- "SubDiv"
 CatchCategory [CatchCategory %in% c("all", "ALL")] <- "C"
 CatchCategory [CatchCategory %in% c("lan","LAN")] <- "L"
 CatchCategory [CatchCategory %in% c("dis","DIS")] <- "D"
 } # end of expression
 ) # end of within call

# SD Species data
SD <- data.frame (
  RecordType = "SD" ,
  Country,
  Year = substr (CANUMdat$time, start=1, stop=4) ,
  SeasonType = dbeOutput@strataDesc@timeStrata ,
  Season = sapply (strsplit (as.character (CANUMdat$time), " - "), FUN = function(x){x[2]} ) ,
  Fleet = as.character (CANUMdat$technical) ,

  AreaType = dbeOutput@strataDesc@spaceStrata ,
  FishingArea = as.character (CANUMdat$space)  ,
  DepthRange = NA ,
  Species ,
  Stock = NA,
  CatchCategory = toupper(dbeOutput@catchCat)  ,            #modif MM 29/04/2010
  ReportingCategory ,
  Sex ,
  CANUMtype,
  AgeLength = as.character(CANUMdat[ ,4]) , # column 4 is age or length
  PlusGroup,
  SampledCatch = -9,
  # information on number of samples and measurements is stored in dbeObject
  # but it is for total numbers not numbers by length group/ size class
  NumSamplesLngt = -9,
  NumLngtMeas = -9,
  NumSamplesAge = -9,
  NumAgeMeas = -9,
  unitMeanWeight,
  unitCANUM,
  UnitAgeOrLength,
  UnitMeanLength = "mm",
  Maturity,
  NumberCaught = CANUMdat$value ,
  MeanWeight = -9,
  MeanLength = -9,
  varNumLanded = varNUMdat$value,
  varWgtLanded = -9,
  varLgtLanded = -9,
  stringsAsFactors = FALSE  ) # end of SD data.frame

# Change case of values to match InterCatch definition
SD <- within (SD, {
 SeasonType[SeasonType == "month"] <- "Month"
 SeasonType[SeasonType == "quarter"] <- "Quarter"
 SeasonType[SeasonType == "year"] <- "Year"
  # if AreaType is "area", assume this means AreaTop not SubArea
 AreaType [ AreaType %in% c("area", "Area", "areatop", "Areatop" )] <- "AreaTop"
 AreaType [ AreaType %in% c("div", "division", "Division" )] <- "Div"
 AreaType [ AreaType %in% c("rect", "Rectangle", "rectangle", "statrect", "StatRect", "Statrect", "statrec", "Statrec" )] <- "StatRec"
 AreaType [ AreaType %in% c("subarea", "Subarea" )] <- "SubArea"
 AreaType [ AreaType %in% c("subdiv", "Subdiv" )] <- "SubDiv"
 CatchCategory [CatchCategory %in% c("all", "ALL")] <- "C"
 CatchCategory [CatchCategory %in% c("lan","LAN")] <- "L"
 CatchCategory [CatchCategory %in% c("dis","DIS")] <- "D"
 } # end of expression
 ) # end of within call

# Replace NA with -9 for integer variables (many already will be -9)

HInumvars = c("Season", "Effort")
HI[, HInumvars] [is.na(HI[, HInumvars])]  <- rep (-9, times= length(HI[, HInumvars] [is.na(HI[, HInumvars])] ) )

SInumvars = c("Season", "CATON", "varCATON")
SI[, SInumvars] [is.na(SI[, SInumvars])]  <- rep (-9, times= length(SI[, SInumvars] [is.na(SI[, SInumvars])] ) )

SDnumvars = c("Season", "AgeLength", "PlusGroup", "SampledCatch",
  "NumSamplesLngt", "NumLngtMeas", "NumSamplesAge", "NumAgeMeas",
  "NumberCaught", "MeanWeight", "MeanLength", "varNumLanded",
  "varWgtLanded", "varLgtLanded")

SD[, SDnumvars] [is.na(SD[, SDnumvars])]  <- rep (-9, times= length(SD[, SDnumvars] [is.na(SD[, SDnumvars])] ) )

out <- list(HI=HI, SI=SI, SD=SD)

if (!is.na(filename)) makeICfile (out, filename, append)

if (output.df) out
} # end of makeICdf function


#######################

## Examples
#
## library(COSTdbe)
## load example data set
#data("LEMexample")
## Setup object for output
#LEM.dbeOut.an = dbeObject(desc="Example",species="Microstomus kitt",param="landings",strataDesc=LEM.strat, catchCat="LAN",methodDesc="analytical")
#
### Run vesselRaise function
#LEM.dbeOut.an = vesselRaise.an (csObject = LEM.CScon, clObject = LEM.CLcon, dbeOutp = LEM.dbeOut.an)
#
## In two steps
## Create list with HI, SI, SD dataframes
#LEM.ICdf <- makeICdf (LEM.dbeOut.an, Country = "UKE", Species = "LEM", ReportingCategory = "R", SamplesOrigin = "M")
## edit dataframes in LEM.ICdf if required then write InterCatch file
#makeICfile (LEM.ICdf, filename ="IC_LEM.csv")
#
## In one step, straight to file. output.df=F stops dataframes being saved within R
#makeICdf (LEM.dbeOut.an, filename ="IC_LEM.csv", output.df = F, Country = "UKE", Species = "LEM", ReportingCategory = "R", SamplesOrigin = "M")
#


