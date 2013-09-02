plot.damdp = function(damdp_obj, plot_margin = 0.05, frac_to_plot = 1, plot_sd = TRUE, plot_orig_pts_deriv = TRUE,
 						pts_preds_size = 1,colorvec, color_by = NULL, x_quantile = FALSE, plot_dpdp = FALSE, plot_new_data = FALSE, 
					rug = TRUE, prop_range_y = FALSE, ...){
	
	DEFAULT_COLORVEC = c("green", "red", "blue", "black", "green", "yellow", "pink", "orange", "forestgreen", "grey")
	arg_list = list(...)

	#list of passed arguments, including the ...
#	arg_list = as.list(match.call(expand.dots = TRUE))

	#some argument checking
	if (class(damdp_obj) != "damdp"){ 
		stop("object is not of class 'amdp'")
	}
	if (frac_to_plot <= 0 || frac_to_plot > 1 ){
		stop("frac_to_plot must be in (0,1]")
	}
	if(!is.null(arg_list$ylim) && plot_sd == TRUE){
		stop("Cannot specify both ylim and plot_sd=TRUE.")
	}
	
	#extract the grid and lines to plot	
	grid = damdp_obj$gridpts 
	n_grid = length(grid)
	ecdf_fcn = NULL
	if (x_quantile){
		ecdf_fcn = ecdf(grid)
		grid = ecdf_fcn(grid)
	}
	apdps = damdp_obj$apdps
	N = nrow(apdps)

	#### figure out the colorvec.
	legend_text = NULL #default is no legend.
	#case 1: random
	if (missing(colorvec) && missing(color_by)){
		colorvec = sort(rgb(runif(N, 0, 0.7), runif(N, 0, 0.7), runif(N, 0, 0.7)))
	} 
	#case 2: both colorvec and color_by specified, so print a warning but use colorvec.
	if(!missing(colorvec) && !missing(color_by)){
		if (!missing(colorvec) && length(colorvec) < N){
			stop("color vector has length ", length(colorvec), " but there are ", N, " lines to plot")
		}
#		warning("Both colorvec and color_by_predictor are specified...using colorvec.")
	}	
	#case 3: colorvec missing but color_by is specified.
	if(!missing(color_by) && missing(colorvec)){
		#argument checking first:
		arg_type = class(color_by)
		if(!(arg_type %in% c("character", "numeric"))){
			stop("color_by must be a column name in X or a column index")
		}
		if(class(color_by) == "character"){
			if(!(color_by %in% names(damdp_obj$Xamdp))){
				stop("The predictor name given by color_by was not found in the X matrix")
			}
		} else{  #check numeric
			if( color_by < 1 || color_by > ncol(damdp_obj$Xamdp) || (color_by%%1 !=0)){
				stop("color_by must be a column name in X or a column index")
			}
		}
		x_color_by = damdp_obj$Xamdp[, color_by]
		x_unique = unique(x_color_by)
		num_x_color_by = length(x_unique)		
		
		
		#if there are 10 or fewer unique values of this x value, we use the
		#same color in DEFAULT_COLORVEC for each. Otherwise, we use a rainbow.
		if (num_x_color_by <= 10){
			
			which_category = match(x_color_by, x_unique)
			colorvec = DEFAULT_COLORVEC[which_category]
			
			#now make the legend.
			legend_text = as.data.frame(cbind(x_unique, DEFAULT_COLORVEC[1 : num_x_color_by]))
			x_column_name = ifelse(is.character(color_by), color_by, paste("x_", color_by, sep = ""))
			names(legend_text) = c(x_column_name,"color")
			cat("AMDP Color Legend\n")
			print(legend_text)			
		} else {
			if (is.factor(x_color_by)){
				warning("color_by is a factor with greater than 10 levels: coercing to numeric.")
				x_color_by = as.numeric(x_color_by)
			}			
			#easy, just smallest to largest with ties broken randomly.
		
#			alpha_blend_colors = matrix(0.5, nrow = N, ncol = 4)
#			alpha_blend_colors[, 3] = 1
#			alpha_blend_colors[, 4] = c(seq(from = 0.2, to = 1, length.out = ceiling(N / 2)), rep(1, N - ceiling(N / 2)))
#			other_color_seq = seq(from = 1, to = 0, length.out = N - ceiling(N / 2))
#			alpha_blend_colors[(ceiling(N / 2) + 1) : N, 3] = seq(from = 1, to = 0, length.out = N - ceiling(N / 2))
#			alpha_blend_colors[(ceiling(N / 2) + 1) : N, 2] = seq(from = 0.5, to = 0, length.out = N - ceiling(N / 2))
#			alpha_blend_colors[(ceiling(N / 2) + 1) : N, 1] = seq(from = 0.5, to = 0, length.out = N - ceiling(N / 2))
	
			alpha_blend_colors = matrix(0, nrow = N, ncol = 4)
			alpha_blend_colors[, 3] = 1
			alpha_blend_colors[, 4] = c(seq(from = 0.2, to = 1, length.out = ceiling(N / 2)), rep(1, N - ceiling(N / 2)))
			alpha_blend_colors[(ceiling(N / 2) + 1) : N, 3] = seq(from = 1, to = 0, length.out = N - ceiling(N / 2))
			alpha_blend_colors[, 2] = seq(from = 0.6, to = 0, length.out = N)
			alpha_blend_colors[, 1] = seq(from = 0.6, to = 0, length.out = N)
			
			rgbs = array(NA, N)
			for (i in 1 : N){
				rgbs[i] = rgb(alpha_blend_colors[i, 1], alpha_blend_colors[i, 2], alpha_blend_colors[i, 3], alpha_blend_colors[i, 4])
			}
			
#			plot(1:200,1:200, type = "n")
#			for (i in 1 : N){
#				abline(a = i, b = 0, col = rgbs[i])
#			}
			colorvec = rgbs[sort(x_color_by, index.return = T)$ix]
		}
	}

	
	#pull out a fraction of the lines to plot
	plot_points_indices = which(as.logical(rbinom(N, 1, frac_to_plot)))
	apdps = apdps[plot_points_indices, ]
	if (nrow(apdps) == 0){
		stop("no rows selected: frac_to_plot too small.")
	}
	colorvec = colorvec[plot_points_indices]
	
	##### now start plotting
	min_apdps = min(apdps)
	max_apdps = max(apdps)
	range_apdps = max_apdps - min_apdps
	min_apdps = min_apdps - plot_margin * range_apdps
	max_apdps = max_apdps + plot_margin * range_apdps

   #add the x and y values
   arg_list = modifyList(arg_list, list(x = grid, y = apdps[1, ]))
  
	#get the xlabel if it wasn't already passed explicitly.
	if( is.null(arg_list$xlab)){
		xlab = damdp_obj$xlab
   		 arg_list = modifyList(arg_list, list(xlab = xlab))
	}
	if (x_quantile){
		xlab = paste("quantile(", xlab, ")", sep = "")
		arg_list = modifyList(arg_list, list(xlab = xlab))
	}
	
	#same for y label
	if( is.null(arg_list$ylab)){	
		if (damdp_obj$logodds){
			ylab = "partial log-odds"
			arg_list = modifyList(arg_list, list(ylab = ylab))
		} else {
			ylab = "derivative f-hat"
			arg_list = modifyList(arg_list, list(ylab = ylab))
		}
	}

	#set xact if not passed explicitly 
	if( is.null(arg_list$xaxt) ){
		xaxt = ifelse(damdp_obj$nominal_axis, "n", "s")
		arg_list = modifyList(arg_list, list(xaxt = xaxt))
	}

	#set ylim if not passed explicitly
	if( is.null(arg_list$ylim) ){
		if(plot_sd){
			offset = 1.5 * max(damdp_obj$sd_deriv)
			ylim = c(min_apdps - offset, max_apdps)	
		}else{
			ylim = c(min_apdps, max_apdps) 
		}
		arg_list = modifyList(arg_list, list(ylim = ylim))
	}
	#set type if not passed explicitly
	if( is.null(arg_list$type) ){
		type = "n"
		arg_list = modifyList(arg_list, list(type = type))
	}

  	#plot all the prediction lines
	# 	plot(grid, apdps[1, ], 
	# 			type = type, 
	# 			ylim = ylim, 
	# 			xlab = xlab, 
	# 			ylab = ylab, 
	# 			xaxt = xaxt, 
	# 			...)

	## if plot_sd = TRUE, set up the layout to have
    ## the dpdp above and the sd plot below.
	do.call("plot", arg_list)
  
  
	if (damdp_obj$nominal_axis){
		axis(1, at = sort(damdp_obj$xj), labels = sort(damdp_obj$xj))
	}	
	
	for (i in 1 : nrow(apdps)){
		points(grid, apdps[i, ], col = colorvec[i], type = "l")
	}

	if (plot_orig_pts_deriv){ #indicate the fitted values associated with observed xj values
		deriv_actual = damdp_obj$actual_deriv[plot_points_indices]
				
		if (x_quantile){
			xj = ecdf_fcn(damdp_obj$xj)[plot_points_indices]
		} else {
			xj = damdp_obj$xj[plot_points_indices]
		}
		points(xj, deriv_actual, col = "black", pch = 16, cex = pts_preds_size)
		points(xj, deriv_actual, col = colorvec, pch = 16)
	}


	#if plot_dpdp is true, plot actual dpdp (in the sense of Friedman '01)
	if (plot_dpdp){
		friedman_dpdp = damdp_obj$dpdp
		
		#calculate the line thickness based on how many lines there are
		num_lines = length(plot_points_indices)
		#every 100 lines we get 0.5 more highlight up to 8
		points(grid, friedman_dpdp, col = "yellow", type = "l", lwd = min(5.5 + (num_lines / 100) * 0.75, 8)) 
		points(grid, friedman_dpdp, col = "BLACK", type = "l", lwd = 4)
	}
	
	if (x_quantile==FALSE && rug){
		rug(damdp_obj$xj)	
	}

	#do the sd plot if required.
	if(plot_sd){
		abline(h = ylim[1] + offset, col = rgb(0.8,0.8,0.8))
		at = seq(ylim[1], ylim[1] + max(damdp_obj$sd_deriv), length.out = 2)	
		#labels = round(at / amdp_obj$range_y, 2)
		labels = round(seq(0, max(damdp_obj$sd_deriv), length.out = 2), 2)
		axis(4, at = at, labels = labels)
		mtext("sd(deriv)", side = 4,line = 0.5)

		points(x= grid, y = (damdp_obj$sd_deriv+ylim[1]),type='l')
	}
		
	if (is.null(legend_text)){
		invisible(list(plot_points_indices = plot_points_indices, legend_text = legend_text))
	} else {
		invisible(list(plot_points_indices = plot_points_indices, legend_text = legend_text))
	}
}
