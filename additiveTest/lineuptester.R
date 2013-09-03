library(randomForest)
library(mlbench)
library(amdp)
source("/home/alex/workspace/advanced_marginal_dependence_plots/additiveTest/backfitter.R")
source("/home/alex/workspace/advanced_marginal_dependence_plots/additiveTest/additivityLineup.R")


#### wrappers for bart and rf
rfFit = function(X,y){
  randomForest(x=X,y=y)
}
rfPred = function(object,newdata){
  predict(object)
}

bartFit = function(X,y){
  build_bart_machine(X=X, y=y, run_in_sample = FALSE, 
      use_missing_data = TRUE, use_missing_data_dummies_as_covars = FALSE)  
}

bartPred = function(object, newdata){
  predict(object, new_data = newdata)
}

################################ EXAMPLES:
########### Boston housing, rm
#get the data
data(BostonHousing); X=BostonHousing[,-14]

#build real rf
rf_mod = randomForest(medv~.,BostonHousing)

#real amdp
rm = amdp(rf_mod, predictor="rm", X=X, frac_to_build=1)

#additive backfitter model
bf_rm = backfitter(X=X,y=BostonHousing$medv, predictor="rm", eps=.005, fitMethod=rfFit,
                predictfcn = rfPred, iter.max=10)

#lineup test
#frac_to_plot applies to realAmdp, which has 506 curves. bf_rm is built to 506 observations as well,
# so the null pads are only built to .2*506 = 101 observations in bf_rm to save time. Then
# the null pads are printed with frac_to_plot = 1 and the real one with frac_to_plot=.2
alu_rm = additivityLineup(bf_rm, fitMethod=rfFit, figs=12, realAmdp=rm, centered=TRUE, frac_to_plot=.2)

#######################################################################################
####### Depression data (not submitted to git for privacy concerns)

amdp.treatment = amdp(bart_machine, X, y, "treatment")

amdp.backfitter = backfitter(X = X, y = y, predictor="treatment", eps=.005, 
                    fitMethod=bartFit, predictfcn = bartPred, iter.max = 10)

alu_trt = additivityLineup(bart_machine, fitMethod = bartFit, figs = 12, 
                  realAmdp = amdp.treatment, centered = TRUE, color_by = "marstat")
