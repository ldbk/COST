#' @title clDataVal-class
#'
#' @description Validated landing object
#'
#' @slot cl a \link{clData-class} object 
#' @param \\dots parameters
#' @param object parameters
#' @param desc parameters
#'
#' @rdname clDataVal-class
#' @export
setClass("clDataVal", contains="clData")

#' @rdname clDataVal-class
#' @export
setGeneric("clDataVal", function(object, ...){
	standardGeneric("clDataVal")
	}
)

#' @rdname clDataVal-class
#' @export
setMethod("clDataVal", signature("clData"), function(object, ...){
	methods::new("clDataVal", cl=object@cl, desc=object@desc)
})

#' @rdname clDataVal-class
#' @export
setMethod("clDataVal", signature("missing"), function(desc="Unknown stock", ...){
	methods::new("clDataVal", desc=desc)
})
