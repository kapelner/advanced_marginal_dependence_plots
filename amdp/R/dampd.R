damdp = function(amdp_obj, DerivEstimator = NULL, plot = FALSE){

	#error checking:
	if(class(amdp_obj) != "amdp"){
		stop("amdp_obj is not a valid amdp object.")
	}

	gridpts = amdp_obj$gridpts

	if(is.null(DerivEstimator)){
		DerivEstimator = function(y){
			D1ss(x = gridpts, y=y, xout = gridpts, spar.offset = 0, spl.spar=NULL)	
		}
	}

	#compute derivatives
	damdp_obj = amdp_obj
	damdp_obj$apdps = t(apply(amdp_obj$apdps, 1, FUN = DerivEstimator))

	#do it for the pdp as well.
	dampd_obj$dpdp = DerivEstimator(amdp_obj$pdp)

	#figure out point on each curve that corresponds to observed X.
    col_idx_of_actual = c(1, 1 + cumsum(diff(amdp_obj$xj)>0))
	row_idx = 1:nrow(damdp_obj$apdps)
	actual_deriv_idx = cbind(row_idx, col_idx_of_actual)
	damdp_obj$actual_deriv = damdp_obj$apdps[actual_deriv_idx]

	#clean up, make it of class 'damdp'	
	damdp_obj$actual_prediction = NULL
	class(damdp_obj) = "damdp"

	
	if (plot){	#if the user wants to use a default plotting, they can get the plot in one line
		plot(damdp_obj)
	}
	invisible(damdp_obj)
}

