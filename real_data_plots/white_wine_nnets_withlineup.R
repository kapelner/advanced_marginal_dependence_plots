library(amdp)
library(nnet)

source("/home/alex/workspace/advanced_marginal_dependence_plots/additiveTest/backfitter.R")
source("/home/alex/workspace/advanced_marginal_dependence_plots/additiveTest/additivityLineup.R")


#dataset_dir = "C:/Users/jbleich/workspace/advanced_marginal_dependence_plots/BakeoffDatasets/"
dataset_dir = "/home/alex/workspace/advanced_marginal_dependence_plots/BakeoffDatasets/"
setwd(dataset_dir)
dataset = read.csv("wine_white.csv")

X = dataset[,-1]; 
for(i in 1:ncol(X)){
  X[,i] = as.numeric(X[,i])
}
y = dataset[,1]
predictors = names(X)
N = nrow(X)
frac_to_plot = 500/N; frac_to_plot = min(frac_to_plot, 1)   #suitable frac_to_plot

X_std = scale(x = X, center = T, scale = T)
X_center = attributes(X_std)$`scaled:center`  
X_scale = attributes(X_std)$`scaled:scale`  
nnet_mod = nnet(x = X_std, y = as.matrix(y), size = 3, maxit = 5000, decay = 5e-4, linout = ifelse(is.factor(y), F, T))

#ph is 9th col
X_std_noPH = scale(x = X[,-9], center = T, scale = T)
X_center_noPH = attributes(X_std_noPH)$`scaled:center`  
X_scale_noPH = attributes(X_std_noPH)$`scaled:scale`  


pred_name = "pH"

nn_wine_amdp = amdp(nnet_mod, X=X, predictor = pred_name, 
                                            predictfcn = function(object, newdata){
                                            newdata_std = scale(newdata, 
                                                          center = .GlobalEnv$X_center, scale = .GlobalEnv$X_scale)
                                            predict(object, newdata_std)
                                          }, y=y)


nn_predictfcn = function(object, newdata){ #for backfitting, one col removed from std
  X_std = scale(newdata, center = .GlobalEnv$X_center_noPH, scale = .GlobalEnv$X_scale_noPH)
  predict(object, X_std)
}

nn_Fit = function(X,y){ #for backfitting - no pH column
  X_std = scale(X, center = .GlobalEnv$X_center_noPH, scale = .GlobalEnv$X_scale_noPH)
  nnet(x = X_std, y = as.matrix(y), size = 3, maxit = 500, decay = 5e-4, linout = ifelse(is.factor(y), F, T))
}

nn_FitFull= function(X,y){ #for nulls.
  X_std = scale(X, center = .GlobalEnv$X_center, scale = .GlobalEnv$X_scale)
  nnet(x = X_std, y = as.matrix(y), size = 3, maxit = 500, decay = 5e-4, linout = ifelse(is.factor(y), F, T))
}

bf_pH = backfitter(X=X,y=y, predictor="pH", eps=.001, fitMethod=nn_Fit,
                           predictfcn = nn_predictfcn, iter.max=30)

#use nn_wine_amdp's predictfcn

colorFcn = function(amdpObj){
	ifelse(amdpObj$Xamdp$alcohol > 10, "RED", "GREEN")
}

alu_pH = additivityLineup(bf_pH, fitMethod=nn_FitFull, figs=12, realAmdp=nn_wine_amdp, 
                          centered=TRUE,x_quantile=TRUE,frac_to_plot=.2,
						  colorvecfcn=colorFcn, usecolorvecfcn_inreal=TRUE, plot_orig_pts=FALSE)  




plot(nn_wine_amdp, frac_to_plot=frac_to_plot, plot_pdp=TRUE, main=paste(pred_name,sep=" "), color_by = "al_ind")
plot(nn_wine_amdp, centered=TRUE,centered_percentile=0.01,frac_to_plot=frac_to_plot, color_by = "al_ind")
plot(nn_wine_amdp, centered=TRUE,centered_percentile=0.01,frac_to_plot=frac_to_plot, color_by = "al_ind", x_quantile = T)
cluster.amdp(amdp_obj=nn_wine_amdp, nClusters=2,plot_legend=T)


.nn_wine_damp = damdp(nn_wine_amdp)

plot(nn_wine_damp,frac_to_plot=frac_to_plot,plot_sd=TRUE, plot_dpdp=TRUE, color_by = "al_ind", x_quantile = T)


  
yhat = predictfcn(nnet_mod, X)
  
1 - sum((y-yhat)^2)/sum((y-mean(y))^2)  
cor(y,yhat)^2
