#' coerceCons
#'
#' Convert columns of a data.frame with respect to refObject format
#'
#' @param object dataframe 
#' @param refObject dataframe 
#' @param \\dots parameters 
#' @param data.frame parameters 
#' @rdname coerceCons
#' @export
setGeneric("coerceCons", function(object, refObject, ...){
	standardGeneric("coerceCons")
	}
)

#' @rdname coerceCons
setMethod("coerceCons", signature("data.frame", "data.frame"), function(object, refObject, ...){

	if(ncol(object)!=ncol(refObject)) stop("Both objects must have the same number of columns.\n")

	n <- ncol(object)
	for(i in 1:n){
		cls <- class(refObject[,i])
		v <- as.character(object[,i])
		eval(parse('',text=paste("v <- as.",cls,"(v)",sep="")))
		object[,i] <- v
	}
	object	
})
