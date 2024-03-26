#' Stratification definition object for consolidated objects creation
#'
#' @param timeStrata character vector definition of the temporal stratification
#' @param spaceStrata character vector definition of the spatial stratification
#' @param techStrata character vector definition of the technical stratification
#' @param tpRec a list defining the temporal stratification
#' @param spRec a list defining the spatial stratification
#' @param tcRec a list defining the technical stratification
#' @rdname strIni-class
#' 
#' @docType methods
#' @export
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

#' @rdname strIni-class
#' @export
strIni <- function(timeStrata=as.character(NA), spaceStrata=as.character(NA), techStrata=as.character(NA), tpRec=as.list(NA), spRec=as.list(NA), tcRec=as.list(NA)){   

	if (is.na(techStrata)==FALSE & techStrata%in%c("vslLen", "vslPwr", "vslSize")) 
		warning(paste("Check that original '", techStrata, "' field in 'tr' table has been categorized into a moderate number of levels. (see 'cut' function)", sep=""))   

	new("strIni",timeStrata=timeStrata, spaceStrata=spaceStrata, techStrata=techStrata, tpRec=tpRec, spRec=spRec, tcRec=tcRec)                      
}
