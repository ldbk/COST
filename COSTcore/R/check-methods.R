#====================================================================
#
# EJ, 24/09/2007
# check methods to help on validity and constructors
#
#====================================================================

#====================================================================
# A set of methods to check names in tables 
#====================================================================

setGeneric("checkNms", function(object, nms, ...){
	standardGeneric("checkNms")
	}
)

setMethod("checkNms", signature("data.frame", "character"), function(object, nms, ...){
	nms <- toupper(nms)
	lnms <- length(nms)
	nms1 <- toupper(names(object))
	if(length(nms1)!=lnms) stop(return(FALSE))
#	if(sum(nms1 %in% nms)!=lnms) stop(return(FALSE))
	if(sum(nms1==nms)!=lnms) stop(return(FALSE))
	return(TRUE)
})

# TR
setGeneric("checkTRnms", function(object, ...){
	standardGeneric("checkTRnms")
	}
)

#modif MM 01/12/2008
setMethod("checkTRnms", signature(object="data.frame"), function(object, ...){
#	rnms <- c("RECORD_TYPE", "SAMPLING_TYPE", "LANDING_COUNTRY", "VESSEL_FLAG_COUNTRY", "YEAR", "PROJECT", "TRIP_NUMBER", "HARBOUR", "VESSEL_LENGTH", "VESSEL_POWER", "VESSEL_SIZE", "VESSEL_TYPE", "NUMBER_HAULS_SETS", "DAYS_AT_SEA", "VESSEL_ID", "SAMPLING_COUNTRY", "SAMPLING_METHOD")
  rnms <- c("TR","Sampling_type","Landing_country","Vessel_flag_country","Year","Project","Trip_number","Vessel_length","Vessel_power","Vessel_size","Vessel_type","Harbour","No_SetsHauls_on_trip","Days_at_sea","Vessel_identifier","Sampling_country","Sampling_method")
  rnms2 <- c("TR","Smpl_type","Lan_c","V_f_c","Year","Prjt","Trip_no","V_lng","V_pow","V_size","V_type","Harbour","No_SH_trp","Days_sea","V_Id","Smpl_c","Smpl_meth")
	if(checkNms(object, rnms)==FALSE & checkNms(object, rnms2)==FALSE) stop("Check table \"TR\" columns' size and names.")
	return(TRUE)
})

# HH
setGeneric("checkHHnms", function(object, ...){
	standardGeneric("checkHHnms")
	}
)

#modif MM 01/12/2008
setMethod("checkHHnms", signature(object="data.frame"), function(object, ...){
	nms <- names(object)
#	rnms <- c("RECORD_TYPE", "SAMPLING_TYPE", "LANDING_COUNTRY", "VESSEL_FLAG_COUNTRY", "YEAR", "PROJECT", "TRIP_NUMBER", "STATION_NUMBER", "FISHING_VALIDITY", "AGGREGATION_LEVEL", "CATCH_REGISTRATION", "SPECIES_REGISTRATION", "DATE", "TIME", "FISHING_DURATION", "POS_START_LAT_DEC", "POS_START_LON_DEC", "POS_STOP_LAT_DEC", "POS_STOP_LON_DEC", "AREA", "RECTANGLE", "SUB_RECTANGLE", "MAIN_FISHING_DEPTH", "MAIN_WATER_DEPTH", "FISHING_ACTIVITY_NAT", "FISHING_ACTIVITY_EU_L5", "FISHING_ACTIVITY_EU_L6", "MESH_SIZE", "SELECTION_DEVICE", "MESH_SIZE_IN_SEL_DEV")
  rnms <- c("HH","Sampling_type","Landing_country","Vessel_flag_country","Year","Project","Trip_number","Station_number","Fishing_validity","Aggregation_level","Catch_registration","Species_registration","Date","Time","Fishing_duration","Pos_Start_Lat_dec","Pos_Start_Lon_dec","Pos_Stop_Lat_dec","Pos_Stop_Lon_dec","Area","Statistical_rectangle","Sub_polygon","Main_fishing_depth","Main_water_depth","FAC_National","FAC_EC_lvl5","FAC_EC_lvl6","Mesh_size","Selection_device","Mesh_size_selection_device")
  rnms2 <- c("HH","Smpl_type","Lan_c","V_f_c","Year","Prjt","Trip_no","St_no","F_val","Aggr_lvl","Catch_reg","Sp_reg","Date","Time","F_dur","Pos_Strt_Lat","Pos_Strt_Lon","Pos_Stop_Lat","Pos_Stop_Lon","Area","Rect","S_poly","M_f_depth","M_w_depth","FAC_Nat","FAC_5","FAC_6","Mesh_s","Sel_dev","Mesh_s_sel_dev")
	if(checkNms(object, rnms)==FALSE & checkNms(object, rnms2)==FALSE) stop("Check table \"HH\" columns' size and names.")
	return(TRUE)
})

# SL
setGeneric("checkSLnms", function(object, ...){
	standardGeneric("checkSLnms")
	}
)

#modif MM 01/12/2008
setMethod("checkSLnms", signature(object="data.frame"), function(object, ...){
	nms <- names(object)
#	rnms <- c("RECORD_TYPE", "SAMPLING_TYPE", "LANDING_COUNTRY", "VESSEL_FLAG_COUNTRY", "YEAR", "PROJECT", "TRIP_NUMBER", "STATION_NUMBER", "SPECIES_CODE", "CATCH_CATEGORY", "LANDING_CATEGORY", "COMM_SIZE_CAT_SCALE", "COMM_SIZE_CAT", "SUBSAMPLING_CATEGORY", "SEX", "WEIGHT", "SUBSAMPLE_WEIGHT", "LENGTH_CODE")
  rnms <- c("SL","Sampling_type","Landing_country","Vessel_flag_country","Year","Project","Trip_number","Station_number","Species","Catch_category","Landing_category","Comm_size_cat_scale","Comm_size_cat","Subsampling_category","Sex","Weight","Subsample_weight","Length_code")
  rnms2 <- c("SL","Smpl_type","Lan_c","V_f_c","Year","Prjt","Trip_no","St_no","Species","Catch_cat","Lan_cat","C_size_cat_sc","C_size_cat","Ssmpl_cat","Sex","Weight","Ssmpl_w","L_code")
	if(checkNms(object, rnms)==FALSE & checkNms(object, rnms2)==FALSE) stop("Check table \"SL\" columns' size and names.")
	return(TRUE)
})

# HL
setGeneric("checkHLnms", function(object, ...){
	standardGeneric("checkHLnms")
	}
)

#modif MM 01/12/2008
setMethod("checkHLnms", signature(object="data.frame"), function(object, ...){
	nms <- names(object)
#	rnms <- c("RECORD_TYPE", "SAMPLING_TYPE", "LANDING_COUNTRY", "VESSEL_FLAG_COUNTRY", "YEAR", "PROJECT", "TRIP_NUMBER", "STATION_NUMBER", "SPECIES_CODE", "CATCH_CATEGORY", "LANDING_CATEGORY", "COMM_SIZE_CAT_SCALE", "COMM_SIZE_CAT", "SUBSAMPLING_CATEGORY", "SEX", "LENGTH_CLASS", "NUMBER_AT_LENGTH")
  rnms <- c("HL","Sampling_type","Landing_country","Vessel_flag_country","Year","Project","Trip_number","Station_number","Species","Catch_category","Landing_category","Comm_size_cat_scale","Comm_size_cat","Subsampling_category","Sex","Length_class","Number_at_length")
  rnms2 <- c("HL","Smpl_type","Lan_c","V_f_c","Year","Prjt","Trip_no","St_no","Species","Catch_cat","Lan_cat","C_size_cat_sc","C_size_cat","Ssmpl_cat","Sex","L_class","No_length")
	if(checkNms(object, rnms)==FALSE & checkNms(object, rnms2)==FALSE) stop("Check table \"HL\" columns' size and names.")
	return(TRUE)
})

# CA
setGeneric("checkCAnms", function(object, ...){
	standardGeneric("checkCAnms")
	}
)

#modif MM 01/12/2008
setMethod("checkCAnms", signature(object="data.frame"), function(object, ...){
	nms <- names(object)
#	rnms <- c("RECORD_TYPE", "SAMPLING_TYPE", "LANDING_COUNTRY", "VESSEL_FLAG_COUNTRY", "YEAR", "PROJECT", "TRIP_NUMBER", "STATION_NUMBER", "QUARTER", "MONTH", "SPECIES_CODE", "SEX", "CATCH_CATEGORY", "LANDING_CATEGORY", "COMM_SIZE_CAT_SCALE", "COMM_SIZE_CAT", "STOCK", "AREA", "RECTANGLE", "SUB_RECTANGLE", "LENGTH_CLASS", "AGE", "SINGLE_FISH_NB", "LENGTH_CODE", "AGE_PLUS_GROUP", "AGEING_METHOD", "OTOLITH_WEIGHT", "OTOLITH_SIDE", "INDIVIDUAL_WEIGHT", "MATURITY_SCALE", "MATURITY_STAGE", "MATURITY_STAGING_METHOD")
  rnms <- c("CA","Sampling_type","Landing_country","Vessel_flag_country","Year","Project","Trip_number","Station_number","Quarter","Month","Species","Sex","Catch_category","Landing_category","Comm_size_cat_scale","Comm_size_cat","Stock","Area","Statistical_rectangle","Sub_polygon","Length_class","Age","Single_fish_number","Length_code","Aging_method","Age_plus_group","Otolith_weight","Otolith_side","Weight","Maturity_staging_method","Maturity_scale","Maturity_stage")
  rnms2 <- c("CA","Smpl_type","Lan_c","V_f_c","Year","Prjt","Trip_no","St_no","Q","Month","Species","Sex","Catch_cat","Lan_cat","C_size_cat_sc","C_size_cat","Stock","Area","Rect","S_poly","L_class","Age","S_fish_no","L_code","Age_method","Age_pl_gr","Oto_w","Oto_side","Weight","Mat_s_method","Mat_scale","Mat_stage")
	if(checkNms(object, rnms)==FALSE & checkNms(object, rnms2)==FALSE) stop("Check table \"CA\" columns' size and names.")
	return(TRUE)
})

# CL
setGeneric("checkCLnms", function(object, ...){
	standardGeneric("checkCLnms")
	}
)

#modif MM 01/12/2008
setMethod("checkCLnms", signature(object="data.frame"), function(object, ...){
	nms <- names(object)
#	rnms <- c("RECORD_TYPE", "LANDING_COUNTRY", "VESSEL_FLAG_COUNTRY", "YEAR", "QUARTER", "MONTH", "AREA", "RECTANGLE", "SUB_RECTANGLE", "TAXON", "LANDING_CATEGORY", "COMM_SIZE_CAT_SCALE", "COMM_SIZE_CAT", "FISHING_ACTIVITY_NAT", "FISHING_ACTIVITY_EU_L5", "FISHING_ACTIVITY_EU_L6", "HARBOUR", "UNALL_CATCH_WEIGHT", "ARMIS_CATCH_WEIGHT", "OFF_LANDINGS_WEIGHT", "LANDINGS_MULT", "OFF_LANDINGS_VALUE")
  rnms <- c("CL","Landing_country","Vessel_flag_country","Year","Quarter","Month","Area","Statistical_rectangle","Sub_polygon","Taxon","Landing_category","Comm_size_cat_scale","Comm_size_cat","FAC_National","FAC_EC_lvl5","FAC_EC_lvl6","Harbour","Vessel_length_cat","Unallocated_catch_weight","Area_misreported_Catch_weight","Official_Landings_weight","Landings_multiplier","Official_landings_value")
  rnms2 <- c("CL","Lan_c","V_f_c","Year","Q","Month","Area","Rect","S_poly","Taxon","Lan_cat","C_size_cat_sc","C_size_cat","FAC_Nat","FAC_5","FAC_6","Harbour","V_l_cat","Unalloc_c_w","Area_mis_C_w","Off_Lan_w","Lan_multi","Off_lan_val")
	if(checkNms(object, rnms)==FALSE & checkNms(object, rnms2)==FALSE) stop("Check table \"Cl\" columns' size and names.")
	return(TRUE)
})

# CE
setGeneric("checkCEnms", function(object, ...){
	standardGeneric("checkCEnms")
	}
)

#modif MM 01/12/2008
setMethod("checkCEnms", signature(object="data.frame"), function(object, ...){
	nms <- names(object)
#	rnms <- c("RECORD_TYPE", "VESSEL_FLAG_COUNTRY", "YEAR", "QUARTER", "MONTH", "AREA", "RECTANGLE", "SUB_RECTANGLE", "FISHING_ACTIVITY_NAT", "FISHING_ACTIVITY_EU_L5", "FISHING_ACTIVITY_EU_L6", "HARBOUR", "NUMBER_OF_TRIPS", "NB_OF_SETS_HAULS", "FISHING_TIME", "KW_DAYS", "GT_DAYS", "DAYS_AT_SEA")
  rnms <- c("CE","Vessel_flag_country","Year","Quarter","Month","Area","Statistical_rectangle","Sub_polygon","FAC_National","FAC_EC_lvl5","FAC_EC_lvl6","Harbour","Vessel_length_cat","Number_of_trips","Number_of_SetsHauls","FishingSoaking_time","kW_days","GT_days","Days_at_sea")
  rnms2 <- c("CE","V_f_c","Year","Q","Month","Area","Rect","S_poly","FAC_Nat","FAC_5","FAC_6","Harbour","V_l_cat","No_trp","No_SH","FS_time","kW_days","GT_days","Days_sea")
	if(checkNms(object, rnms)==FALSE & checkNms(object, rnms2)==FALSE) stop("Check table \"CE\" columns' size and names.")
	return(TRUE)
})

#====================================================================
# A set of methods to get and check PK in tables 
#====================================================================

# TR
setGeneric("checkTRpk", function(object, ...){
	standardGeneric("checkTRpk")
	}
)

setMethod("checkTRpk", signature(object="data.frame"), function(object, ...){
	#identical(object[,1:6], unique(object[,1:6]))
  nrow(unique(object[,1:6]))==nrow(object)
})

# HH
setGeneric("checkHHpk", function(object, ...){
	standardGeneric("checkHHpk")
	}
)

setMethod("checkHHpk", signature(object="data.frame"), function(object, ...){
	#identical(object[,1:7], unique(object[,1:7]))
	nrow(unique(object[,1:7]))==nrow(object)
})

# SL
setGeneric("checkSLpk", function(object, ...){
	standardGeneric("checkSLpk")
	}
)

setMethod("checkSLpk", signature(object="data.frame"), function(object, ...){
	#identical(object[,1:14], unique(object[,1:14]))
  nrow(unique(object[,1:14]))==nrow(object)
})

# HL
setGeneric("checkHLpk", function(object, ...){
	standardGeneric("checkHLpk")
	}
)

setMethod("checkHLpk", signature(object="data.frame"), function(object, ...){
	#identical(object[,1:15], unique(object[,1:15]))
  nrow(unique(object[,1:15]))==nrow(object)
})

# CA
setGeneric("checkCApk", function(object, ...){
	standardGeneric("checkCApk")
	}
)

setMethod("checkCApk", signature(object="data.frame"), function(object, ...){
	#identical(object[,c(1:22)], unique(object[,c(1:22)]))
  nrow(unique(object[,1:22]))==nrow(object)
})

# CL
setGeneric("checkCLpk", function(object, ...){
	standardGeneric("checkCLpk")
	}
)

setMethod("checkCLpk", signature(object="data.frame"), function(object, ...){
	#identical(object[,c(1:17)], unique(object[,c(1:17)]))                         #modif MM 01/12/2008
  nrow(unique(object[,1:17]))==nrow(object)
})

# CE
setGeneric("checkCEpk", function(object, ...){
	standardGeneric("checkCEpk")
	}
)

setMethod("checkCEpk", signature(object="data.frame"), function(object, ...){
	#identical(object[,c(1:12)], unique(object[,c(1:12)]))                         #modif MM 01/12/2008
  nrow(unique(object[,1:12]))==nrow(object)
})

#====================================================================
# Methods to check data integrity 
#====================================================================

# data integrity 
setGeneric("checkDataIntegrity", function(target, current, ...){
	standardGeneric("checkDataIntegrity")
	}
)

setMethod("checkDataIntegrity", signature(target="data.frame", current="data.frame"), function(target, current, report=FALSE, ...){

	trg <- apply(target, 1, paste, collapse="")
	trg <- gsub("[[:space:]]","",trg)                                             #added MM 21/07/08
	current <- unique(current)
	if(sum(is.na(current))==ncol(current)) return(TRUE)
	crr <- apply(current, 1, paste, collapse="")
	crr <- gsub("[[:space:]]","",crr)                                             #added MM 21/07/08
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

