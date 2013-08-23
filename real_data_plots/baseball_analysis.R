library(randomForest)
library(gbm)   #boost
library(e1071) #svm
library(nnet)  #nnet
library(gam)   #gam
library(amdp)
library(scatterplot3d);

data(Baseball)
X = Baseball[, -1]
rf_mod = randomForest(salary.thou~., Baseball)

BA  = amdp(rf_mod,predictor="BA", X=X, y = Baseball[,1])
RBI = amdp(rf_mod,predictor="RBI", X=X, y = Baseball[,1])
HR  = amdp(rf_mod,predictor="HR", X=X, y = Baseball[,1])
RUNS = amdp(rf_mod,predictor="Runs", X=X, y = Baseball[,1])
HITS = amdp(rf_mod,predictor="Hits", X=X, y = Baseball[,1])
OBP  = amdp(rf_mod,predictor="OBP", X=X, y = Baseball[,1])

dBA  = damdp(BA)
dRBI = damdp(RBI)
dHR  = damdp(HR)
dRUNS = damdp(RUNS)
dHITS = damdp(HITS)
dOBP = damdp(OBP)

#low RBI players do not have many hits, by definition.
#thus the majority of the high |deriv| curves when Hits is low
#comes from low RBI guys. Similarly, the high |deriv| curves
#when hits are high comes from the high RBI guys.
plot(HITS,plot_sd=T,plot_dpdp=T,color_by="RBI",centered=T,centered_percentie=0)
plot(dHITS,plot_sd=T,plot_dpdp=T,color_by="RBI")


#sweet interaction between on base percentage and free agency eligibility:
plot(dOBP,plot_sd=T,plot_dpdp=T,plot_orig_pts_deriv =F,x_quantile=T,color_by= "Free.agency.eligibility")


#### boston housing
library(randomForest)
library(gbm)
library(amdp)
library(mlbench); data(BostonHousing); X=BostonHousing[,-14]
rf_mod = randomForest(medv~.,BostonHousing)

lstat = amdp(rf_mod, predictor="lstat", X=X, frac_to_build=.3)
rm = amdp(rf_mod, predictor="rm", X=X, frac_to_build=.3)
age = amdp(rf_mod, predictor="age", X=X, frac_to_build=.3)
crim = amdp(rf_mod, predictor="crim", X=X, frac_to_build=.3)
ptratio = amdp(rf_mod, predictor="ptratio", X=X, frac_to_build=.3)

dlstat = damdp(lstat); drm = damdp(rm); dage = damdp(age); dcrim = damdp(crim)
dptratio = damdp(ptratio)
