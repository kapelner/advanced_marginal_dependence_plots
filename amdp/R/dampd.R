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

	damdp_obj = amdp_obj
	damdp_obj$apdps = t(apply(amdp_obj$apdps, 1, FUN = DerivEstimator))

	class(damdp_obj) = "damdp"
	
	if (plot){	#if the user wants to use a default plotting, they can get the plot in one line
		plot(damdp_obj)
	}
	invisible(damdp_obj)
}

