library(randomForest)
library(missForest)
library(MASS)
library(amdp)


###########GRID COMPUTING
LAST_NAME = "kapelner"
NOT_ON_GRID = length(grep("wharton.upenn.edu", Sys.getenv(c("HOSTNAME")))) == 0

if (NOT_ON_GRID){
	directory_where_code_is = "C:\\Users\\kapelner\\workspace\\CGMBART_GPL"
} else {
	directory_where_code_is = getwd()
}
setwd(directory_where_code_is)

source("bartMachine/R/bart_package_inits.R")
source("bartMachine/R/bart_package_builders.R")
source("bartMachine/R/bart_package_predicts.R")
source("bartMachine/R/bart_package_data_preprocessing.R")
source("bartMachine/R/bart_package_plots.R")
source("bartMachine/R/bart_package_variable_selection.R")
source("bartMachine/R/bart_package_f_tests.R")
source("bartMachine/R/bart_package_summaries.R")

#get the Boston housing data
data(Boston)
X = Boston
#X = cbind(X, rnorm(nrow(X)))
y = X$medv
X$medv = NULL

set_bart_machine_num_cores(4)
bart_machine = build_bart_machine(X, y)



#windows()
#investigate_var_importance(bart_machine)

#create amdp's for all features in the boston housing data
amdb_bart_objs = list()
for (j in colnames(X)){
	amdb_bart_objs[[j]] = amdp(bart_machine, X, y, j, frac_to_build = 1)
}
save(amdb_bart_objs, file = "amdb_bart_objs.RData")

graphics.off()
for (j in colnames(X)){
	windows()
	par(mfrow = c(1, 3))
	plot(amdb_bart_objs[[j]], frac_to_plot = 0.1)
	plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = 0.02, prop_range_y = TRUE, x_quantile = FALSE)
	cluster.amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE, x_quantile = FALSE)
}

par(mfrow = c(1, 3))
j = "age"
plot(amdb_bart_objs[[j]], frac_to_plot = 0.1, x_quantile = FALSE, color_by = "nox", plot_pdp = TRUE)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = TRUE, x_quantile = TRUE, plot_orig_pts_preds = FALSE, color_by = "nox", plot_pdp = TRUE)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = TRUE, x_quantile = FALSE, plot_orig_pts_preds = FALSE, color_by = "nox")
cluster.amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE, plot_legend = TRUE)

#hack a new thing up
par(mfrow = c(2, 1))
amdb_bart_objs[[j]]$Xamdp$I_nox = ifelse(amdb_bart_objs[[j]]$Xamdp$nox > .538, 1, 0)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE, x_quantile = F, plot_orig_pts_preds = FALSE, color_by = "I_nox")
damdb_bart_objs = damdp(amdb_bart_objs[[j]])
plot(damdb_bart_objs, x_quantile = F, color_by = "I_nox")

j = "rm"
plot(amdb_bart_objs[[j]], frac_to_plot = 0.1, color_by = "lstat", plot_pdp = T)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE, plot_orig_pts_preds = FALSE, color_by = "lstat")
cluster.amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE, plot_legend = TRUE)

j = "chas"
plot(amdb_bart_objs[[j]], frac_to_plot = 0.1, color_by = "tax", plot_pdp = T)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE, plot_orig_pts_preds = FALSE, color_by = "tax")
cluster.amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE, plot_legend = TRUE)

j = "black"
plot(amdb_bart_objs[[j]], frac_to_plot = 0.1, plot_pdp = T)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE, plot_orig_pts_preds = FALSE)
cluster.amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE, plot_legend = TRUE)


#remind me of the linear model
lm_mod = lm(medv ~ ., Boston)
summary(lm_mod)

lm_mod = lm(medv ~ . * age, Boston)
summary(lm_mod)

lm_mod = lm(medv ~ . * rm, Boston)
summary(lm_mod)

lm_mod = lm(medv ~ . * chas, Boston)
summary(lm_mod)

lm_mod = lm(medv ~ . * black, Boston)
summary(lm_mod)


#pub images




rf_mod = randomForest(X, y)
j = "age"
rf.pad = amdp(rf_mod, X, y, j, frac_to_build = 1)


#just PDP
plot(rf.pad, x_quantile = TRUE, plot_pdp = TRUE, colorvec = rep("white", nrow(X)), plot_orig_pts_preds = FALSE)
#first PAD
plot(rf.pad, x_quantile = TRUE, plot_pdp = TRUE)
#cPAD
plot(rf.pad, x_quantile = TRUE, plot_pdp = TRUE, centered = TRUE)
#dPAD
rf.dpad = damdp(rf.pad)
plot(rf.dpad, x_quantile = T)
#cPAD by color
rf.pad$Xamdp$I_nox = ifelse(rf.pad$Xamdp$nox > .538, 1, 0)
rf.pad$Xamdp$I_crime = ifelse(rf.pad$Xamdp$crim > .256, 1, 0)
rf.pad$Xamdp$I_rm = ifelse(rf.pad$Xamdp$rm > 6.2, 1, 0)
plot(rf.pad, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE, x_quantile = T, plot_orig_pts_preds = T, color_by = "I_rm")



#plot(rf.pad, frac_to_plot = 1, x_quantile = TRUE, color_by = "nox", plot_pdp = TRUE)
#plot(rf.pad, frac_to_plot = 1, centered = TRUE, x_quantile = TRUE, plot_orig_pts_preds = FALSE, color_by = "nox", plot_pdp = TRUE)
#plot(rf.pad, frac_to_plot = 1, centered = TRUE, x_quantile = FALSE, plot_orig_pts_preds = FALSE, color_by = "nox")
#
##hack a new thing up
##par(mfrow = c(2, 1))
#rf.pad$Xamdp$I_nox = ifelse(rf.pad$Xamdp$nox > .538, 1, 0)
#plot(rf.pad, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE, x_quantile = T, plot_orig_pts_preds = T, color_by = "I_nox")
#rf.dpad = damdp(rf.pad)
#plot(rf.dpad, x_quantile = T, color_by = "I_nox")



