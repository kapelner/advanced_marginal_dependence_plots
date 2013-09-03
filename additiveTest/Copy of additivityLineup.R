#nonparametric parametric bootstrap.

additivityLineup = function(backfit_obj, fitMethod, realAmdp, figs=10, frac_to_build=1,...){
  
  predictor = backfit_obj$predictor
  #some pre-processing regarding centering.
  arg_list = list(...)
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
  
	additive_fit = backfit_obj$g1_of_Xs+backfit_obj$g2_of_Xc
	additive_res = backfit_obj$y - additive_fit
	
	null_additive_fits = list()
	null_amdps = list()
  
	for(i in 1:(figs-1)){
		response = additive_fit + sample(additive_res, size=length(additive_res), replace = F)
		new_fit = fitMethod(X = backfit_obj$X, y = response, run_in_sample = FALSE, use_missing_data = TRUE, use_missing_data_dummies_as_covars = FALSE)
		null_additive_fits[[i]] = new_fit
		null_amdps[[i]] = amdp(new_fit, X=backfit_obj$X, predictor=predictor, y = backfit_obj$y, 
							frac_to_build=frac_to_build)
    
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
			plot(realAmdp,ylim = ylim, ...)
			plotted_truth = TRUE
		}
		else{
			plot(null_amdps[[idx_to_plot]], ylim = ylim,...)
		}
	}
  al_obj = list(location=where_to_place, null_additive_fits = null_additive_fits, 
                        null_pads = null_amdps)
  class(al_obj) = "additivityLineup"  
	invisible(al_obj)
}
