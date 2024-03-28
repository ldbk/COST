#' @title csDataVal-class
#'
#' @description Commercial sampling validated object
#'
#' @slot cs a \link{csData-class} object
#' @param object,desc,\dots parameters
#'
#' @rdname csDataVal-class
#' @export
setClass("csDataVal", contains="csData")

#' @rdname csDataVal-class
setGeneric("csDataVal", function(object, ...){
	standardGeneric("csDataVal")
	}
)

#' @rdname csDataVal-class
setMethod("csDataVal", signature("csData"), function(object, ...){
	methods::new("csDataVal", tr=tr(object), hh=hh(object), sl=sl(object), hl=hl(object), ca=ca(object), desc=object@desc)
})

#' @rdname csDataVal-class
setMethod("csDataVal", signature("missing"), function(desc="Unknown stock", ...){
	methods::new("csDataVal", desc=desc)
})
