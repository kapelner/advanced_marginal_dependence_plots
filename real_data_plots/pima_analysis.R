library(amdp)
library(MASS)
library(randomForest)
pima = (Pima.te)

# Extract predictors and response

head(pima)
X = pima[,-8]
y = as.numeric(pima[,8]) - 1

#rf object
pima_rf = randomForest(x = X, y = y)



for(name in names(X)){
  
  #Create amdp objects of form pima_amdp_obj_[predictor name]
  assign(paste("pima_amdp_obj_",name, sep =""),  
       amdp(pima_rf, X = X, predictor = name, y = y, logodds = T))
  par(mfrow = c(1,2))
  
  #Plot centered amdp
  plot(get(paste("pima_amdp_obj_",name, sep ="")), frac_to_plot = 0.1, x_quantile = T, centered = T)
  
  #Create corresponding damdp of form pima_damdp_obj_predictor name]
  assign(paste("pima_damdp_obj_",name, sep =""), 
         damdp(get(paste("pima_amdp_obj_",name, sep =""))))
  
  #plot it
  plot(get(paste("pima_damdp_obj_",name, sep ="")), 
       frac_to_plot = 0.1, plot_sd = T, x_quantile = T, rug = F)
  readline("Click for next plot")
}
