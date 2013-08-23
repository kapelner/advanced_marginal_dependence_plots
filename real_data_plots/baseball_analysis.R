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

BA  = amdp(rf_mod,predictor="BA", X=X)
RBI = amdp(rf_mod,predictor="RBI", X=X)
HR  = amdp(rf_mod,predictor="HR", X=X)

dBA  = damdp(BA)
dRBI = damdp(RBI)
dHR  = damdp(HR)
