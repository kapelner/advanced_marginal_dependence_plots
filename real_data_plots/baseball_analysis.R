library(randomForest)
library(gbm)   #boost
library(e1071) #svm
library(nnet)  #nnet
library(gam)   #gam
library(amdp)
library(scatterplot3d);

mlb = read.csv("/home/alex/Data/BakeoffDatasets/baseballsalary.csv",h=T)
X = mlb[, -1]
rf_mod = randomForest(salary.thou~.,mlb)

BA  = amdp(rf_mod,predictor="BA", X=X)
RBI = amdp(rf_mod,predictor="RBI", X=X)
HR  = amdp(rf_mod,predictor="HR", X=X)

dBA  = damdp(BA)
dRBI = damdp(RBI)
dHR  = damdp(HR)
