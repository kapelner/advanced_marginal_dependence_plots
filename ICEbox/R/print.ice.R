print.ice = function(ice_obj){
	cat("ICE object generated on data with n = ", nrow(ice_obj$apdps), " for predictor \"", ice_obj$predictor, "\"\n", sep = "")
	cat("predictor considered ", ifelse(ice_obj$nominal_axis, "discrete", "continuous"), ", logodds ", ifelse(ice_obj$logodds, "on", "off"), "\n", sep = "")
}
