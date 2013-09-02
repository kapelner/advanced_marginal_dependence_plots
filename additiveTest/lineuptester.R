library(randomForest)
library(mlbench)
library(amdp)
source("/home/alex/workspace/advanced_marginal_dependence_plots/additiveTest/backfitter.R")
source("/home/alex/workspace/advanced_marginal_dependence_plots/additiveTest/additivityTest.R")

#get the data
data(BostonHousing); X=BostonHousing[,-14]

#build real rf
rf_mod = randomForest(medv~.,BostonHousing)



########### rm
#real amdp
rm = amdp(rf_mod, predictor="rm", X=X, frac_to_build=1, y = BostonHousing$medv)

#additive backfitter model
bf = backfitter(X=X,y=BostonHousing$medv, predictor=6, eps=.01, g2Fit=randomForest)

#lineup test
realplot = additivityLineup(bf, predictor=6, fitMethod=randomForest, figs=10, realAmdp=rm)


########## crim 
crim = amdp(rf_mod, predictor="crim", X=X, frac_to_build=1, y = BostonHousing$medv)

#additive backfitter model
bfcrim = backfitter(X=X,y=BostonHousing$medv, predictor= 1, eps=.01, g2Fit=randomForest)

#lineup test
realplot = additivityLineup(bfcrim, predictor=1, fitMethod=randomForest, figs=10, realAmdp=crim)



########################3
#### criss-cross example
n = 1000
p = 2
X = matrix(runif(n * p, -1, 1), ncol = p)
X = cbind(X, rbinom(n, 1, 0.5)) #indicator variable
colnames(X) = paste("x_", 1 : (p + 1), sep = "")
bbeta1 = as.matrix(c(0.2, 5, 0))
bbeta2 = as.matrix(c(0.2, -5, 0))

y = array(NA, n)
for (i in 1 : n){
	if (X[i, 3] == 1){
		y[i] = X[i, ] %*% bbeta1
	} else {
		y[i] = X[i, ] %*% bbeta2
	}
}

rf_mod = randomForest(X, y)

bf_cross = backfitter(X=X,y=y, predictor=2, eps = .01, g2Fit=randomForest)
amdp_2 = amdp(rf_mod, X=X, predictor=2, y=y)

realplot = additivityLineup(bf_cross, predictor=2, fitMethod=randomForest, figs=4, realAmdp=amdp_2,frac_to_build=.1)

