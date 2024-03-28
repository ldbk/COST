#' Check names in tables 
#' @param object, dataframe,character parameters
#' @param \\dots parameters
#' @param nms parameters
#' @rdname checkNms 
#' @export
setGeneric("checkNms", function(object, nms, ...){
	standardGeneric("checkNms")
	}
)

#' @rdname checkNms 
setMethod("checkNms", signature("data.frame", "character"), function(object, nms, ...){
	nms <- toupper(nms)
	lnms <- length(nms)
	nms1 <- toupper(names(object))
	if(length(nms1)!=lnms) stop(return(FALSE))
	if(sum(nms1==nms)!=lnms) stop(return(FALSE))
	return(TRUE)
})
#' Check primary keys in tables
#'
#' @description Fishing effort data class validity check
#'
#' @param object a data.frame
#' @param type the object type
#' @return a boolean (TRUE if the object is a \link{ceData-class} object, FALSE if not)
#'
#' @export
checkpk<-function(object,type){
	rez<-FALSE
	if(type=="tr"){rez<-nrow(unique(object[,1:6]))==nrow(object)}
	if(type=="hh"){rez<-nrow(unique(object[,1:7]))==nrow(object)}
	if(type=="sl"){rez<-nrow(unique(object[,1:14]))==nrow(object)}
	if(type=="hl"){rez<-nrow(unique(object[,1:15]))==nrow(object)}
	if(type=="ca"){rez<-nrow(unique(object[,1:22]))==nrow(object)}
	if(type=="cl"){rez<-nrow(unique(object[,1:17]))==nrow(object)}
	if(type=="ce"){rez<-nrow(unique(object[,1:12]))==nrow(object)}
	return(rez)
}

#====================================================================
# Methods to check data integrity 
#====================================================================
#' Check data integrity
#' @param target,current dataframe  
#' @param \\dots parameters
#' @param report boolean 
#' @rdname checkDataIntegrity 
#' @export
setGeneric("checkDataIntegrity", function(target, current, ...){
	standardGeneric("checkDataIntegrity")
	}
)

#' @rdname checkDataIntegrity 
setMethod("checkDataIntegrity", signature(target="data.frame", current="data.frame"), function(target, current, report=FALSE, ...){

	trg <- apply(target, 1, paste, collapse="")
	trg <- gsub("[[:space:]]","",trg)
	current <- unique(current)
	if(sum(is.na(current))==ncol(current)) return(TRUE)
	crr <- apply(current, 1, paste, collapse="")
	crr <- gsub("[[:space:]]","",crr)
	if(report==TRUE){
		current[!(crr %in% trg),]	
	} else {
		sum(crr %in% trg)==length(crr)
	}
})

#' @title checkTys 
#'
#' @description Methods to check types of columns in tables
#' @param object data.frame
#' @param tys parameters 
#' @param data.frame parameters 
#' @param list a list
#' @param \\dots parameters
#'
#' @export
setGeneric("checkTys", function(object, tys, ...){
	standardGeneric("checkTys")
	}
)

#' @rdname checkTys
setMethod("checkTys", signature("data.frame", "list"), function(object, tys, ...){
	n <- ncol(object)
	lst <- split(1:n, 1:n)
	lst <- lapply(lst, function(x) methods::is(object[,x], tys[[x]]))
	identical(sum(unlist(lst)),n) 
})
