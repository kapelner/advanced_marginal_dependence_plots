library(randomForest)
library(mlbench)
library(amdp)
source("/home/alex/workspace/advanced_marginal_dependence_plots/additiveTest/backfitter.R")
source("/home/alex/workspace/advanced_marginal_dependence_plots/additiveTest/additivityLineup.R")

#get the data
data(BostonHousing); X=BostonHousing[,-14]

#build real rf
rf_mod = randomForest(medv~.,BostonHousing)



########### rm
#real amdp
rm = amdp(rf_mod, predictor="rm", X=X, frac_to_build=.2)

#additive backfitter model
bf = backfitter(X=X,y=BostonHousing$medv, predictor=6, eps=.001, fitMethod=randomForest,iter.max=10)

#lineup test
realplot = additivityLineup(bf, fitMethod=randomForest, figs=12, realAmdp=rm, centered=TRUE)

########### rm
#real amdp
age = amdp(rf_mod, predictor="age", X=X, frac_to_build=.1)

#additive backfitter model
bf_age = backfitter(X=X,y=BostonHousing$medv, predictor=7, eps=.001, g2Fit=randomForest,iter.max=10)

#lineup test
realplot = additivityLineup(bf_age, predictor=7, fitMethod=randomForest, figs=10, realAmdp=age, frac_to_build=.1,centered=T)



########## crim 
crim = amdp(rf_mod, predictor="crim", X=X, frac_to_build=1, y = BostonHousing$medv)

#additive backfitter model
bfcrim = backfitter(X=X,y=BostonHousing$medv, predictor= 1, eps=.01, g2Fit=randomForest)

#lineup test
realplot = additivityLineup(bfcrim, predictor=1, fitMethod=randomForest, figs=10, realAmdp=crim)


####### Depression data (not submitted to git for privacy concerns)

amdp.treatment = amdp(bart_machine, X, y, "treatment")

amdp.backfitter = backfitter(X = X, y = y, predictor = 38, eps = .01, fitMethod = build_bart_machine)

realplot = additivityLineup(amdp.backfitter, fitMethod = build_bart_machine, figs = 12, realAmdp = amdp.treatment, centered = TRUE, color_by = "marstat")



