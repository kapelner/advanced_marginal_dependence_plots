print.dice = function(dice_obj){
	cat("dice object generated on data with n = ", nrow(dice_obj$d_ice_curves), " for predictor \"", dice_obj$predictor, "\"\n", sep = "")
	cat("predictor considered ", ifelse(dice_obj$nominal_axis, "discrete", "continuous"), ", logodds ", ifelse(dice_obj$logodds, "on", "off"), "\n", sep = "")
}
