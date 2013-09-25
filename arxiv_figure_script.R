#######################################################################
# This R script generates the figures in 
#   "Peeking Inside the Black Box: Visualizing Statistical Learning
#    with Plots of Individual Conditional Expectation,"
# available on arxiv at:
# <arxiv link here>. 
#
# The stable version of the ICEbox package is available on CRAN, 
# and all examples below will work with this version.  Beta
# versions of the package with additional features are available at
# <<github link here>>.
#
# Note that examples pertaining to the depression dataset are omitted 
# as this data cannot currently be made public.
#######################################################################

### load R packages
library(ICEbox)
library(randomForest)
library(gam)
library(missForest)



#######################################################################
############ Section: The ICE Toolbox #################################
#######################################################################
# load the BH data:
library(MASS)
data(Boston)
X = Boston
y = X$medv
X$medv = NULL

# build an RF with default settings:
rf_mod = randomForest(X, y)

### build the ICE object:
#make frac_to_build < 1 for fewer curves but faster computation
rf.ice = ice(rf_mod, X, y, predictor = "age", frac_to_build = 1) 

### plot Friedman's PDP by making the individual curves in the ICE white
plot(rf.ice, x_quantile = TRUE, plot_pdp = TRUE, 
		colorvec = rep("white", nrow(X)), plot_orig_pts_preds = FALSE)

### plot ICE:
# Make frac_to_plot < 1 to plot a fraction of the curves built.  
# This an make the plot less cluttered sometimes.
plot(rf.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1) 

### plot c-ICE
plot(rf.ice, x_quantile = TRUE, plot_pdp = TRUE, centered = TRUE)

### create the d-ICE object:
rf.dice = dice(rf.ice)

### plot the d-ICE:
plot(rf.dice, x_quantile = T)


### Visualizing a second feature: we investigate the c-ICE 
# by coloring it by the "rm" variable. First create an indicator variable:
rf.ice$Xice$I_rm = ifelse(rf.ice$Xice$rm > 6.2, 1, 0)  
# then plot:
plot(rf.ice, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,  
		x_quantile = T, plot_orig_pts_preds = T, color_by = "I_rm")

#######################################################################


#######################################################################
############ Section: "Simulations" ###################################
#######################################################################

### Additivity Assessment
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

# build ICE and d-ICE:
gam.ice = ice(gam_mod, X, predictor = 1, frac_to_build = 1) 
gam.dice = dice(gam.ice)

# plot the ICE plot with pdp, and d-ICE with dpdp
plot(gam.ice, x_quantile = F, plot_pdp = T, frac_to_plot = 1)  
plot(gam.dice, x_quantile = F, plot_dpdp = T, frac_to_plot = 1) 

### Extrapolation Detection


