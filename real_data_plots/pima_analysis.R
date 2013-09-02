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


amdps = list()
dampds = list()
par(mfrow = c(2, 1))

for (j in colnames(X)){
  amdps[[j]] = amdp(pima_rf, X = X, predictor = j, y = y, logodds = T)
#  plot(amdps[[j]], frac_to_plot = 0.5, x_quantile = T, centered = T, prop_range_y = F)
  dampds[[j]] = damdp(amdps[[j]])
#  plot(dampds[[j]], frac_to_plot = 0.5, plot_sd = T, x_quantile = T, rug = F, prop_range_y = F)
#  readline("Click for next plot")
}



amdps[[j]]$Xamdp$npreg_I = ifelse(pima$npreg > 2, 1, 0)
amdps[[j]]$Xamdp$glu_I = ifelse(pima$glu > 112, 1, 0)
amdps[[j]]$Xamdp$bp_I = ifelse(pima$bp > 72, 1, 0)
amdps[[j]]$Xamdp$bmi_I = ifelse(pima$bmi > 32.9, 1, 0)
amdps[[j]]$Xamdp$ped_I = ifelse(pima$ped > 0.44, 1, 0)
amdps[[j]]$Xamdp$age_I = ifelse(pima$age > 27, 1, 0)

dampds[[j]]$Xamdp$npreg_I = ifelse(pima$npreg > 2, 1, 0)
dampds[[j]]$Xamdp$glu_I = ifelse(pima$glu > 112, 1, 0)
dampds[[j]]$Xamdp$bp_I = ifelse(pima$bp > 72, 1, 0)
dampds[[j]]$Xamdp$bmi_I = ifelse(pima$bmi > 32.9, 1, 0)
dampds[[j]]$Xamdp$ped_I = ifelse(pima$ped > 0.44, 1, 0)
dampds[[j]]$Xamdp$age_I = ifelse(pima$age > 27, 1, 0)


j = "skin"
k = "age"
plot(amdps[[j]], frac_to_plot = 0.5, x_quantile = T, centered = T, prop_range_y = F, color_by = k)
plot(dampds[[j]], frac_to_plot = 0.5, plot_sd = T, x_quantile = T, rug = F, prop_range_y = F, color_by = k)

summary(pima)

