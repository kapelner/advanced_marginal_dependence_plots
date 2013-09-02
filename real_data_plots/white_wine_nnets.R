library(amdp)
library(nnet)


dataset_dir = "C:/Users/jbleich/workspace/advanced_marginal_dependence_plots/BakeoffDatasets/"
setwd(dataset_dir)
dataset = read.csv("wine_white.csv")

X = dataset[,-1]; 
for(i in 1:ncol(X)){
  X[,i] = as.numeric(X[,i])
}
y = dataset[,1]
predictors = names(X)
N = nrow(X)
frac_to_plot = 300/N; frac_to_plot = min(frac_to_plot, 1)   #suitable frac_to_plot

X_std = scale(x = X, center = T, scale = T)
X_center = attributes(X_std)$`scaled:center`  
X_scale = attributes(X_std)$`scaled:scale`  
nnet_mod = nnet(x = X_std, y = as.matrix(y), size = 3, maxit = 500, decay = 5^-4, linout = ifelse(is.factor(y), F, T))


nn_wine_amdp = amdp(nnet_mod, X=X, predictor = pred_name, 
                                          predictfcn = function(object, newdata){
                                            newdata_std = scale(newdata, center = X_center, scale = X_scale)
                                            predict(object, newdata_std)
                                          }, y=y)



nn_wine_amdp$Xamdp$al_ind = ifelse(nn_wine_amdp$Xamdp$alcohol > 10, 1, 0)

plot(nn_wine_amdp, frac_to_plot=frac_to_plot, plot_pdp=TRUE, main=paste(pred_name,sep=" "), color_by = "al_ind")
plot(nn_wine_amdp, centered=TRUE,centered_percentile=0.01,frac_to_plot=frac_to_plot, color_by = "al_ind")
plot(nn_wine_amdp, centered=TRUE,centered_percentile=0.01,frac_to_plot=frac_to_plot, color_by = "al_ind", x_quantile = T)


nn_wine_damp = damdp(nn_wine_amdp)

plot(nn_wine_damp,frac_to_plot=frac_to_plot,plot_sd=TRUE, plot_dpdp=TRUE, color_by = "al_ind", x_quantile = T)
