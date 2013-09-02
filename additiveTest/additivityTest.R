#nonparametric parametric bootstrap.

additivityLineup = function(backfitter, predictor, fitMethod, figs=10, realAmdp, frac_to_build=1){
		
	additive_fit = backfitter$yhat_curr
	additive_res = backfitter$y - additive_fit
	

	null_additive_fits = list()
	null_amdps = list()
	for(i in 1:(figs-1)){
		response = additive_fit + sample(additive_res, size=length(additive_res), replace = F)
		new_fit = fitMethod(x=backfitter$X, y=response)
		null_additive_fits[[i]] = new_fit
		null_amdps[[i]] = amdp(new_fit, X=backfitter$X, predictor=predictor, y = backfitter$y, 
							frac_to_build=frac_to_build)
	}
	
	#graphics
	num_plot_cols = ceiling(figs/4)
	num_plot_rows = ceiling(figs/num_plot_cols)
	par(mfrow=c(num_plot_rows, num_plot_cols))
	par(cex=.3)
	par(mar=c(0.13,0.13,0.13,0.13))

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
			plot(realAmdp)
			plotted_truth = TRUE
		}
		else{
			plot(null_amdps[[idx_to_plot]])
		}
	}
	invisible(where_to_place)
}
