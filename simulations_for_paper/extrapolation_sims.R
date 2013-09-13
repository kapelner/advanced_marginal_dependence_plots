library(amdp)
library(mlbench)
library(randomForest)

#d = mlbench.peak(n=1000, d = 2)
N = 400
d = mlbench.xor(N,d=2)
X = d$x
kill = (X[,1]>0)*(X[,2]>0)
Xkeep = X[kill==F,]
plot(Xkeep)

Y = 10*Xkeep[,1]^2 + (Xkeep[,2]>0)+rnorm(nrow(Xkeep),sd=.1)
data = as.data.frame(cbind(Y,Xkeep)); names(data)[2:3]=c("x1","x2")

rf_mod = randomForest(Y~.,data)
rf_amdp = amdp(rf_mod,X=data[,2:3],predictor="x1")

#x2_indic = 1 <==> no extrapolation 
rf_amdp$Xamdp$x2_indic = ifelse(rf_amdp$Xamdp$x2>0,0,1) 

no_extrap_indices = which(rf_amdp$Xamdp$x2_indic==1)
extrap_indices = which(rf_amdp$Xamdp$x2_indic==0)

xtrap = rf_amdp
no_xtrap = rf_amdp


### full plot
plot(rf_amdp, plot_pdp=FALSE, color_by="x2_indic")

### plot of extraps in red
xtrap$apdps = xtrap$apdps[extrap_indices,]
xtrap$actual_prediction = xtrap$actual_prediction[extrap_indices] 
xtrap$xj = xtrap$xj[extrap_indices] 
plot(xtrap, plot_pdp=FALSE, colorvec = rep("red",N))

### plot of no extraps in green
no_xtrap$apdps = xtrap$apdps[no_extrap_indices,]
no_xtrap$actual_prediction = no_xtrap$actual_prediction[no_extrap_indices] 
no_xtrap$xj = no_xtrap$xj[no_extrap_indices] 
plot(no_xtrap, plot_pdp=FALSE, colorvec = rep("green",N))
