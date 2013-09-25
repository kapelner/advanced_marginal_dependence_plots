dice = function(amdp_obj, DerivEstimator, plot = FALSE){

	#error checking:
	if(class(amdp_obj) != "ice"){
		stop("amdp_obj is not a valid amdp object.")
	}

	gridpts = amdp_obj$gridpts

	if (missing(DerivEstimator)){
		EstimatorWrapper = function(y){
			D1tr( x = gridpts, y = supsmu(x=gridpts,y=y)$y)
		}
	} else{
		#argument checking???
		EstimatorWrapper = function(y){
			DerivEstimator(y=y,x=gridpts)		
		}
	}

	#compute derivatives
	damdp_obj = amdp_obj
	damdp_obj$d_ice_curves = t(apply(amdp_obj$ice_curves, 1, FUN = EstimatorWrapper))
	damdp_obj$ice_curves = NULL

	#do it for the pdp as well.
	damdp_obj$dpdp = EstimatorWrapper(amdp_obj$pdp)

	#figure out point on each curve that corresponds to observed X.
    col_idx_of_actual = c(1, 1 + cumsum(diff(amdp_obj$xj)>0))
	row_idx = 1:nrow(damdp_obj$d_ice_curves)
	actual_deriv_idx = cbind(row_idx, col_idx_of_actual)
	damdp_obj$actual_deriv = damdp_obj$d_ice_curves[actual_deriv_idx]

	#compute the sd of the derivatives at each gridpt.
	damdp_obj$sd_deriv = apply(damdp_obj$d_ice_curves, 2, sd)
	
	#clean up, make it of class 'damdp'	
	damdp_obj$actual_prediction = NULL
	class(damdp_obj) = "dice"
	
	if (plot){	#if the user wants to use a default plotting, they can get the plot in one line
		plot(damdp_obj)
	}
	invisible(damdp_obj)
}
