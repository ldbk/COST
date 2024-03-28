#' @title ceDataVal-class
#'
#' @description Fishing effort object
#'
#' @slot ce a \link{ceData-class} object 
#' @param object,desc,\dots parameters
#'
#' @rdname ceDataVal-class
#' @export
setClass("ceDataVal", contains="ceData")

#' @rdname ceDataVal-class
#' @export
setGeneric("ceDataVal", function(object, ...){
	standardGeneric("ceDataVal")
	}
)

#' @rdname ceDataVal-class
#' @export
setMethod("ceDataVal", signature("ceData"), function(object, ...){
		  methods::new("ceDataVal", ce=object@ce, desc=object@desc)
})

#' @rdname ceDataVal-class
#' @export
setMethod("ceDataVal", signature("missing"), function(desc="Unknown stock", ...){
		  methods::new("ceDataVal", desc=desc)
})


