#nonparametric parametric bootstrap.

additivityLineup = function(backfit_obj, fitMethod, realAmdp, figs=10,...){
  
  ######## check inputs
  # backfit_obj
  if(class(backfit_obj)!="backfitter"){
    stop("'backfit_obj' is not of class 'backfitter'")
  }
  
  # fitMethod 
  if(missing(fitMethod)){
    stop("Must pass fitMethod that accepts arguments 'X' and 'y'.")
  }else{
    fcn_args = names(formals(fitMethod))
    if(!("X" %in% fcn_args) || !("y" %in% fcn_args)){
      stop("fitMethod must accept arguments X and y.")
    }
  }
  
  # predictfcn is taken from realAmdp!
  predictfcn = realAmdp$predictfcn  
  if(is.null(predictfcn)){
    use_generic = TRUE
  }
  
  predictor = backfit_obj$predictor
  #some pre-processing regarding centering.
  arg_list = list(...)
  
  #frac_to_build is not allowed
  if(!is.null(arg_list$frac_to_build)){
    cat("Cannot specify  frac_to_build. Can specify frac_to_plot, which applies to the realAmdp,
        and frac_to_build for the null PADs is then inferred to plot the same number of curves.")
    cat("\n")
  }
  
  centered = arg_list$centered
  if(is.null(centered)){
    centered = FALSE
  }
  centered_percentile = arg_list$centered_percentile
  if(is.null(centered_percentile) && centered==TRUE){ 
    centered_percentile = .01  #default in plot.amdp
  }
  #figure out min and max of real amdp object -- depends on centering  
  if(centered){
    centering_vector = realAmdp$apdps[, ceiling(ncol(realAmdp$apdps) * centered_percentile + 0.00001)]
    rg = range(realAmdp$apdps - centering_vector) 
  }else{  #just the min and max.
    rg = range(realAmdp$apdps)
  }
  apdp_min = rg[1]
  apdp_max = rg[2]
  
  #and some more for frac_to_plot
  frac_to_build_null = 1
  if(!is.null(arg_list$frac_to_plot)){
    warning_msg = paste("'frac_to_plot' only applies to plotting 'realAmdp'.",
    "'frac_to_build' is set in null PADs to ensure the same number of curves are plotted for null and real plots.",sep="\n")  
    warning(warning_msg)
    frac_to_build_null = nrow(realAmdp$apdps)*arg_list$frac_to_plot / nrow(backfit_obj$X)
  }
  
	additive_fit = backfit_obj$g1_of_Xs+backfit_obj$g2_of_Xc
	additive_res = backfit_obj$y - additive_fit
	
	null_additive_fits = list()
	null_amdps = list()
  
  #figure out frac_to_build in nulls so that when we 
  
	for(i in 1:(figs-1)){
		response = additive_fit + sample(additive_res, size=length(additive_res), replace = F)
		new_fit = fitMethod(X=backfit_obj$X, y=response)
		null_additive_fits[[i]] = new_fit
    if(use_generic){ #no predictfcn passed
  		null_amdps[[i]] = amdp(new_fit, X=backfit_obj$X, predictor=predictor, y = backfit_obj$y, 
							frac_to_build=frac_to_build_null)
    }else{
      null_amdps[[i]] = amdp(new_fit, X=backfit_obj$X, predictor=predictor, y = backfit_obj$y, 
                             frac_to_build=frac_to_build_null, predictfcn = realAmdp$predictfcn)
    }
    
    ### keep track of min and max. 
    if(!is.null(centered) && centered==TRUE){  #keep track of range after centered

      centering_vector = null_amdps[[i]]$apdps[, ceiling(ncol(null_amdps[[i]]$apdps) * centered_percentile + 0.00001)]
      rg = range(null_amdps[[i]]$apdps - centering_vector) #range for centered plot
    }
    else{  #regular pre-centered range
      rg = range(null_amdps[[i]]$apdps)      
    }

    #update min range and max range
    if(rg[1] < apdp_min){
        apdp_min = rg[1]
    }
    if(rg[2] > apdp_max){
        apdp_max = rg[2]
    }
    cat("Finished null amdp ",i,"\n")
	} #end loop through null amdps
  
	#graphics
	num_plot_cols = ceiling(figs/4)
	num_plot_rows = ceiling(figs/num_plot_cols)
	par(mfrow=c(num_plot_rows, num_plot_cols))
	par(cex=.3)
	par(mar=c(0.13,0.13,0.13,0.13))
  ylim = c(apdp_min,apdp_max)
  
  #argument list for the null plots.
  null_arg_list = arg_list
  null_arg_list$ylim = ylim
  null_arg_list$frac_to_plot = 1
  
	#randomly place the real plot somewhere...
	where_to_place = sample(1:figs,1)
	plotted_truth = FALSE
	for(i in 1:figs){

		if(plotted_truth){
			idx_to_plot = i-1
		}else{
			idx_to_plot = i
		}

		if((!plotted_truth) && (i==where_to_place)){
			plot(realAmdp,ylim = ylim,...)
			plotted_truth = TRUE
		}
		else{
      null_arg_list$amdp_obj = null_amdps[[idx_to_plot]]
			#plot(null_amdps[[idx_to_plot]], ylim = ylim)
      do.call(plot.amdp, null_arg_list)
		}
	}
  al_obj = list(location=where_to_place, null_additive_fits = null_additive_fits, 
                        null_pads = null_amdps, frac_to_build_null = frac_to_build_null)
  class(al_obj) = "additivityLineup"  
	invisible(al_obj)
}
