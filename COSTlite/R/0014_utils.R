#' coerceDataFrameColumns
#' 
#' @param object object to coerce
#' @param refObject reference object
#' @param \\dots parameters
#' @rdname coerceDataFrameColumns 
#' @export
setGeneric("coerceDataFrameColumns", function(object, refObject, ...){
	standardGeneric("coerceDataFrameColumns")
	}
)

#' @rdname coerceDataFrameColumns 
setMethod("coerceDataFrameColumns", signature("data.frame", "data.frame"), function(object, refObject, ...){
	if(ncol(object)!=ncol(refObject)) stop("Both objects must have the same number of columns.\n")
	n <- ncol(object)
	for(i in 1:n){
		cls <- class(refObject[,i])
		if(cls=="factor"){
			v <- methods::as(object[,i], "character")
			v <- methods::as(v, "factor")
		} else {
			v <- methods::as(object[,i], cls)
		}
		object[,i] <- v
	}
	object	
})

#' extCatchCat
#' @description internal function
#' @param x object
#' @export
extCatchCat <- function(x) {
sapply(x, function(x) substring(as.character(x),1,3))
}

#' spdAgreg
#' @description internal function
#' @param X,BY,FUN  object
#' @param \\dots parameters 
#' @export
spdAgreg <- function(X,BY,FUN,...){
if (length(BY[[1]])>1) {
  FactCar <- sapply(BY,as.character)
  val <- apply(FactCar,1,function(x) paste(x,collapse=":-:"))
} else {
  val <- paste(BY,collapse=":-:")
}
valAg <- stats::aggregate(X,list(val=val),FUN,...)
tab <- as.data.frame(matrix(unlist(strsplit(as.character(valAg$val),":-:")),ncol=length(BY),byrow=TRUE))
tab.ag <- data.frame(tab,valAg[,-1])
namBY <- names(BY) ; namX <- names(X)
if (is.null(namBY)) namBY <- rep("",length(BY)) ; if (is.null(namX)) namX <- rep("",length(X))
namBY[namBY==""] <- paste("c.",1:sum(namBY==""),sep="") ; namX[namX==""] <- paste("v.",1:sum(namX==""),sep="")
names(tab.ag) <- c(namBY,namX)
return(tab.ag)}

#' As.num 
#' @description internal function
#' @param x object
#' @export
As.num <- function(x) as.numeric(as.character(x))

#' resample 
#' @description internal function
#' sample function that returns one value if give data with one value x,
#' rather than returning sample of vector 1:x , see help file for sample
#' @param x,size,replace  object
#' @param \\dots parameters
#' @export
resample <- function(x, size, replace,...){
  if(length(x) <= 1) { 
	  if(!missing(size) && size == 0) x[FALSE] else x
  } else{ 
	  sample(x, size, replace,...)
  }
}

#' slSex 
#' @description internal function
#' method for handle 'sex' conditional key field in SL : an new 'sex' field is inserted in HL, matching SL sex field, and true HL sex field is moved down and renamed as 'lsex'
#' @param slTab,hlTab object
#' @export
slSex <- function(slTab,hlTab) {
	if (names(slTab)[1]!=names(hlTab)[1]) stop("tables must be consistent!!")
	ind <- NULL
	if (names(slTab)[1]=="sampType") ind <- 1:14 
	if (names(slTab)[1]=="PSUid") ind <- 1:15
	if (is.null(ind)) stop("wrong input tables!!") 
	slTab <- slTab[,ind] ; slTab$lsex <- slTab$sex ; hlTab$N <- 1:nrow(hlTab)
	hlTab <- merge(hlTab,slTab,all.x=TRUE)
	hlTab <- hlTab[order(hlTab$N),-match("N",names(hlTab))]
	sex <- as.character(hlTab$sex) 
	hlTab$sex <- as.character(hlTab$lsex)
	hlTab$lsex <- sex
	rownames(hlTab) <- 1:nrow(hlTab)
	return(hlTab)
}
