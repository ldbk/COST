#===================================
#
# EJ, 24/09/2007
# clData-class
#
#===================================

#====================================================================
# Class definition and validity check
#====================================================================

setClass("clDataVal", contains="clData")

#====================================================================
# Class constructor
#====================================================================
setGeneric("clDataVal", function(object, ...){
	standardGeneric("clDataVal")
	}
)

setMethod("clDataVal", signature("clData"), function(object, ...){
	new("clDataVal", cl=cl(object), desc=desc(object))
})

setMethod("clDataVal", signature("missing"), function(desc="Unknown stock", ...){
	new("clDataVal", desc=desc)
})



