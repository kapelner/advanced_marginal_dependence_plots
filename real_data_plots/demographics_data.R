library(amdp)
library(kernlab)
data(income)
demo = income
dim(demo)

##recodes
levels(demo$INCOME) = (-4:4)*2
demo$INCOME = as.numeric(as.character(demo$INCOME))

levels(demo$AGE) = 1 : 7
demo$AGE = as.numeric(as.character(demo$AGE))

levels(demo$EDUCATION) = 1 : 6
demo$EDUCATION = as.numeric(as.character(demo$EDUCATION))

levels(demo$AREA) = 1 : 5
demo$AREA = as.numeric(as.character(demo$AREA))

levels(demo$HOUSEHOLD.SIZE) = 1 : 9
demo$HOUSEHOLD.SIZE = as.numeric(as.character(demo$HOUSEHOLD.SIZE))

levels(demo$UNDER18) = 1 : 10
demo$UNDER18 = as.numeric(as.character(demo$UNDER18))


demo2 = na.omit(demo)
dim(demo2)
colnames(demo2)

library(randomForest)
rf = randomForest(x = demo2[,-1], y = demo2[,1])
rf_amdp = amdp(object = rf, X = demo2[,-1], y = demo2[,1], predictor = "AGE")
partialPlot(rf,pred.data=demo2[,-1],x.var="AGE")
plot.amdp(rf_amdp,frac_to_plot=.01, plot_pdp=T, rug=T, xaxt = "s")
plot.amdp(rf_amdp, frac_to_plot=.01, plot_pdp=T,rug=T, centered = T )
cluster.amdp(amdp_obj=rf_amdp,nClusters=2, plot_legend=T)


rf_amdp2 = amdp(object = rf, X = demo2[,-1], y = demo2[,1], predictor = "EDUCATION")
rf_damdp2 = damdp(amdp_obj = rf_amdp2, plot = T)
plot.damdp(rf_damdp2, frac_to_plot = .05, plot_sd = T)
partialPlot(rf, pred.data = demo2[,-1], x.var="EDUCATION")

plot.amdp(rf_amdp2, frac_to_plot = .01, plot_pdp = T,rug = T, color_by = "MARITAL.STATUS")
plot.amdp(rf_amdp2, frac_to_plot = .01, plot_pdp = T,rug = T, color_by = "MARITAL.STATUS", centered = T)
cluster.amdp(amdp_obj = rf_amdp2,nClusters = 2, plot_legend = T)



##Try to reproduce with gradient boosting
library(gbm)

boost = gbm(formula = INCOME~., data = demo2, n.trees = 500,interaction.depth = 2, cv.folds = 3, bag.fraction = .5)
predict(boost, )

predict_gbm = function(object, newdata){
  best = gbm.perf(object, plot.it = F)
  predict.gbm(object, newdata, n.tree, n.trees = best)
}

##RF vs. Friedman's booster
test_idx = sample(1: nrow(demo2), size = 1000, replace = F)
train_idx = setdiff(1 : nrow(demo2), test_idx)
train = demo2[train_idx, ]
test = demo2[test_idx, ]

rf = randomForest(x = train[,-1], y = train[,1])
preds_rf = predict(rf, newdata = test)
mse_rf = sum((preds_rf - test$INCOME)^2)/nrow(test)

boost = gbm(formula = INCOME~., data = demo2, n.trees = 50000, interaction.depth = 1, cv.folds = 5, bag.fraction = .5)
  best = gbm.perf(boost, plot.it = F)
preds_sgb = predict(boost, test[,-1], n.trees = best)
mse_sgb = sum((preds_sgb - test$INCOME)^2)/nrow(test)






