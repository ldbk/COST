#' Check names in tables 
#' @param dataframe 
#' @param character vectors
#' @rdname checkNms 
#' @docType methods
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
#' @param object a \link{ceData-class} object
#' @return a boolean (TRUE if the object is a \link{ceData-class} object, FALSE if not)
#'
#' @export
checkpk<-function(data,names,type){

}

#====================================================================
# A set of methods to get and check PK in tables 
#====================================================================
setGeneric("checkTRpk", function(object, ...){
	standardGeneric("checkTRpk")
	}
)

setMethod("checkTRpk", signature(object="data.frame"), function(object, ...){
  nrow(unique(object[,1:6]))==nrow(object)
})

# HH
setGeneric("checkHHpk", function(object, ...){
	standardGeneric("checkHHpk")
	}
)

setMethod("checkHHpk", signature(object="data.frame"), function(object, ...){
	nrow(unique(object[,1:7]))==nrow(object)
})

# SL
setGeneric("checkSLpk", function(object, ...){
	standardGeneric("checkSLpk")
	}
)

setMethod("checkSLpk", signature(object="data.frame"), function(object, ...){
  nrow(unique(object[,1:14]))==nrow(object)
})

# HL
setGeneric("checkHLpk", function(object, ...){
	standardGeneric("checkHLpk")
	}
)

setMethod("checkHLpk", signature(object="data.frame"), function(object, ...){
  nrow(unique(object[,1:15]))==nrow(object)
})

# CA
setGeneric("checkCApk", function(object, ...){
	standardGeneric("checkCApk")
	}
)

setMethod("checkCApk", signature(object="data.frame"), function(object, ...){
  nrow(unique(object[,1:22]))==nrow(object)
})

# CL
setGeneric("checkCLpk", function(object, ...){
	standardGeneric("checkCLpk")
	}
)

setMethod("checkCLpk", signature(object="data.frame"), function(object, ...){
  nrow(unique(object[,1:17]))==nrow(object)
})

# CE
setGeneric("checkCEpk", function(object, ...){
	standardGeneric("checkCEpk")
	}
)

setMethod("checkCEpk", signature(object="data.frame"), function(object, ...){
  nrow(unique(object[,1:12]))==nrow(object)
})

#====================================================================
# Methods to check data integrity 
#====================================================================
setGeneric("checkDataIntegrity", function(target, current, ...){
	standardGeneric("checkDataIntegrity")
	}
)

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

#====================================================================
# check types of columns in tables 
#====================================================================
setGeneric("checkTys", function(object, tys, ...){
	standardGeneric("checkTys")
	}
)

setMethod("checkTys", signature("data.frame", "list"), function(object, tys, ...){
	n <- ncol(object)
	lst <- split(1:n, 1:n)
	lst <- lapply(lst, function(x) is(object[,x], tys[[x]]))
	identical(sum(unlist(lst)),n) 
})
