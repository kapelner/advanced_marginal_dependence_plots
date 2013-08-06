#this short script generates the plots used in the intro section
#to explain partial dependence plots. 

library(randomForest)
library(missForest)
library(MASS)
library(amdp)

data(Boston)
X = Boston

rf_mod = randomForest(medv~.,Boston)
partialPlot(rf_mod, X, x.var = "rm")

partialPlot(rf_mod, X, x.var = "rm",main="",ylab="yhat")
partialPlot(rf_mod, X, x.var = "lstat",main="",ylab="yhat")

#caution: alex's machine
dev.copy2pdf(file="/home/alex/Dropbox/amdp_project/paper/bh_intro_pdp_example.pdf")

