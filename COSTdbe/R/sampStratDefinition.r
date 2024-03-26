
#library(COSTcore)
#
#====================================================================
# sampStrDef : * defines from a given dataset which was the sampling strategy  (according to ANNEX B of "Minutes of the 2nd COST expert meeting")
#                  - at the haul level (one value per row in hh)
#                  - at the trip level (one value per row in tr) -->  ex: only 0 at the haul level --> 0 at the trip level
#                                                                         0,1 at the haul level --> 1 ...
#                                                                         0,1,4 at the haul level --> 9 ...(several strategy in the same trip)
#                                                                         1,3,5 at the haul level --> 9 ...  ===> 9 is a warning
#
# Sampling strategies coding :
#         1. Sampling for length in fishing trips - Unsorted catch
#         2. Sampling for length in fishing trips - Commercial Categories
#         3. Sampling for length in Commercial categories
#         4. Sampling for age in fishing trips - Unsorted catch
#         5. Sampling for age in fishing trips - Commercial Categories
#         6. Sampling for age in commercial categories
#
#         0. No sampling or undefined strategy (eg no data in sl for the haul)
#         9. Several defined strategies (trip level)           
#====================================================================


ssdFun <- function(x,species,fraction="LAN",fishAct="foCatEu5",sampPar=TRUE,...) {        
                                                                                                              #x = raw or validated CS object
if (missing(species)) stop("Missing species parameter!!")                                                     #'sampPar' checks if given species is considered as automatically sampled
                                                                                                              #    (cf 'OvaluesIndex.r')

fraction <- toupper(fraction)                               #
x@sl$catchCat <- toupper(x@sl$catchCat)                     # MM 29/04/2010
x@hl$catchCat <- toupper(x@hl$catchCat)                     #
x@ca$catchCat <- toupper(x@ca$catchCat)                     #

#Values in 'catReg' field are "All", "Lan", "Dis" & "Non", so...
if (fraction=="DIS") frac <- "Dis" else frac <- "Lan"

#-------------------
# Step n°1 : sl, hl & ca are subset to given species and fraction
#-------------------

subSL <- sl(x)[sl(x)$spp%in%species & sl(x)$catchCat%in%fraction,]
subHL <- hl(x)[hl(x)$spp%in%species & hl(x)$catchCat%in%fraction,]
subCA <- ca(x)[ca(x)$spp%in%species & ca(x)$catchCat%in%fraction,]

#-------------------
# Step n°2 : index for --> ('catReg' = "All" or fraction) & ('sppReg' = "All" or ('sppReg' = "Par" & sampPar=TRUE)) (sampling strategy definition will only be checked for indReg==TRUE)
#-------------------

indReg <- hh(x)$catReg%in%c("All",frac) & (hh(x)$sppReg%in%"All" | (hh(x)$sppReg%in%"Par" & sampPar))

#-------------------
# Step n°3 : internal function that defines sampling strategy for one specified haul (= one row from HH)
#-------------------

  #keys for subsetting process are defined
slKey <- gsub(" ","",apply(subSL[,1:7],1,paste,collapse=":-:"))
hlKey <- gsub(" ","",apply(subHL[,1:7],1,paste,collapse=":-:"))
caKey <- gsub(" ","",apply(subCA[,1:7],1,paste,collapse=":-:"))

    #-------------------
    # Sub procedure : 'sampStr' -->  'n' is a row number 
    #-------------------

sampStr <- function(n) {

hhKey <- gsub(" ","",paste(hh(x)[n,1:7],collapse=":-:"))
#part of sl/hl/ca that is linked to hh row is subset...
sl <- subSL[slKey%in%hhKey,]
hl <- subHL[hlKey%in%hhKey,]
ca <- subCA[caKey%in%hhKey,]
IND <- 0

#...and sampling strategy is checked from this

if (indReg[n]) {

if (nrow(sl)==0) { IND <- 0       #if no data in sl, 0

} else {

if (nrow(hl)==0) {                      # 4, 5, 6 or 0

  if (nrow(ca)==0) { IND <- 0
  
  } else {

    if (!is.na(hh(x)[n,fishAct])) {         # 4 or 5

      if (all(!is.na(sl$commCat))) {IND <- 5
      } else { IND <- 4
      }

    } else {                                # 6
  
      if (all(!is.na(sl$commCat))) {IND <- 6
      } else { IND <- 0
      }
    }
  }

} else {                               # 1, 2, 3 or 0

if (!is.na(hh(x)[n,fishAct])) {             # 1 or 2

      if (all(!is.na(sl$commCat))) {IND <- 2
      } else { IND <- 1
      }

    } else {                                # 3

      if (all(!is.na(sl$commCat))) {IND <- 3
      } else { IND <- 0
      }
    }
}
}}

return(IND)}


val <- sapply(1:nrow(hh(x)),sampStr)

#now, we define the sampling strategy at the trip level (TR)
  #linking Key between tr & hh
hh_trKey <- gsub(" ","",apply(hh(x)[,1:6],1,paste,collapse=":-:"))
trKey <- gsub(" ","",apply(tr(x)[,1:6],1,paste,collapse=":-:"))
vv <- tapply(val,list(hh_trKey),function(x) {val <- unique(sort(x)) 
                                                 if (length(val)==1) {       #unique value for the trip
                                                    return(val)
                                                 } else {                     #several values for the trip
                                                    val <- val[!val%in%0]       #0 is removed
                                                    if (length(val)==1) {       #unique value for the trip except 0
                                                      return(val)
                                                    } else {                    #several values for the trip except 0 --> 9
                                                      return(9)
                                                    }
                                                 }}
)

#so, according to tr table...
trIND <- as.numeric(vv[trKey])


return(list(sampStrategyTR=trIND,sampStrategyHH=val))

}
  
  
 
 
#====================================================================
# sampStrDef : methods
#====================================================================  
   

setGeneric("sampStrDef", function(x,species,fraction="LAN",fishAct="foCatEu5",sampPar=TRUE,...){
	standardGeneric("sampStrDef")
	}
)
                       
  
setMethod("sampStrDef", signature(x="csData"), function(x,species,fraction="LAN",fishAct="foCatEu5",sampPar=TRUE,...){

ssdFun(x=x,species=species,fraction=fraction,fishAct=fishAct,sampPar=sampPar)

})


setMethod("sampStrDef", signature(x="csDataVal"), function(x,species,fraction="LAN",fishAct="foCatEu5",sampPar=TRUE,...){

ssdFun(x=x,species=species,fraction=fraction,fishAct=fishAct,sampPar=sampPar)

})





#---------------
# Examples
#---------------

#sampStrDef(sole.cs,species="Solea solea")
#sampStrDef(csDataVal(sole.cs),species="Solea solea",fraction="DIS")
#