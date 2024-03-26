#===================================
#
# EJ, 24/09/2007
# ceData-class
#
#===================================

#====================================================================
# Class definition
# 	Note: validity is done by parent class 
#====================================================================

setClass("ceDataVal", contains="ceData")

#====================================================================
# Class constructor
#====================================================================
setGeneric("ceDataVal", function(object, ...){
	standardGeneric("ceDataVal")
	}
)

setMethod("ceDataVal", signature("ceData"), function(object, ...){
	new("ceDataVal", ce=ce(object), desc=desc(object))
})

setMethod("ceDataVal", signature("missing"), function(desc="Unknown stock", ...){
	new("ceDataVal", desc=desc)
})


