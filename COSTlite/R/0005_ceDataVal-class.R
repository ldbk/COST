#' @title ceDataVal-class
#'
#' @description Fishing effort object
#'
#' @slot ce a \link{ceData-class} object 
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
	new("ceDataVal", ce=ce(object), desc=desc(object))
})

#' @rdname ceDataVal-class
#' @export
setMethod("ceDataVal", signature("missing"), function(desc="Unknown stock", ...){
	new("ceDataVal", desc=desc)
})


