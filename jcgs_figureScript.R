# This R script generates the figures in ...
# Examples pertaining to the ... dataset are omitted as this data
# cannot currently be made public.

### load R packages
library(amdp)
library(randomForest)
library(gam)
library(missForest)



#######################################################################
############ Section: "PAD Toolbox" ###################################
#######################################################################
# load the data:
library(MASS)
data(Boston)
X = Boston
y = X$medv
X$medv = NULL

# build an RF with default settings:
rf_mod = randomForest(X, y)

# build the PAD:
rf.pad = amdp(rf_mod, X, y, predictor = "age", frac_to_build = 1)

# plot Friedman's PDP by making the individual curves in the PAD white
plot(rf.pad, x_quantile = TRUE, plot_pdp = TRUE, 
		colorvec = rep("white", nrow(X)), plot_orig_pts_preds = FALSE)

# plot PAD:
plot(rf.pad, x_quantile = TRUE, plot_pdp = TRUE)

# plot cPAD
plot(rf.pad, x_quantile = TRUE, plot_pdp = TRUE, centered = TRUE)

# create the dPAD:
rf.dpad = damdp(rf.pad)

# plot the dPAD:
plot(rf.dpad, x_quantile = T)


# Visualizing a second feature: we investigate the cPAD by coloring it by the "rm" variable.
# first create an indicator variable:
rf.pad$Xamdp$I_rm = ifelse(rf.pad$Xamdp$rm > 6.2, 1, 0)  
# then plot:
plot(rf.pad, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,  
		x_quantile = T, plot_orig_pts_preds = T, color_by = "I_rm")

#######################################################################
############ Section: "Simulations" ###################################
#######################################################################

## function to generate the 'no interactions' example:

noInter_ex_sim = function(n,seednum=NULL){
	if(!is.null(seednum)){
		set.seed(seednum)
	}
	p = 2
	X = as.data.frame(matrix(runif(n * p, -1, 1), ncol = p))	
	colnames(X) = paste("x_", 1 : p, sep = "")
	bbeta = c(1,1)

	y = bbeta[1] * X[,1]^2  + bbeta[2] * X[,2]
	y = y + rnorm(n)
	Xy = as.data.frame(cbind(X, y))
	return(list(Xy=Xy,X=X,y=y))
}

# generate data:
noInter_data = noInter_ex_sim(1000)
Xy = noInter_data$Xy
X  = noInter_data$X
y  = noInter_data$y

# build gam with possible interactions:
gam_mod = gam(y~s(x_1)+s(x_2)+s(x_1*x_2),data=Xy)   

# build PAD and dPAD:
gam_amdp = amdp(gam_mod, X, predictor = 1, frac_to_build = 1)

# plot the PDP and PAD
plot(gam_amdp, x_quantile = F, plot_pdp = T, frac_to_plot = 0.01,colorvec=rep("WHITE",1000), plot_orig_pts=F)
plot(gam_amdp, x_quantile = F, plot_pdp = T, frac_to_plot = 0.3)



