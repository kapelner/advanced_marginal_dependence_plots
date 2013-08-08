library(amdp)
library(kernlab)
data(income)
demo = income
head(income)
head(demo$INCOME)
dim(demo)
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

demo$INCOME[1:5]
income$INCOME[1:5]

summary(income)

##Re-codes
income_level = demo$INCOME
levels(income_level) = 1:9
sex = demo$SEX
age = demo$AGE
levels()
levels(demo$AGE)


library(randomForest)W
rf = randomForest(x = demo2[,-1], y = demo2[,1])
rf_amdp = amdp(object = rf, X = demo2[,-1], y = demo2[,1], predictor = "AGE")
partialPlot(rf,pred.data=demo2[,-1],x.var="AGE")
plot.amdp(rf_amdp,frac_to_plot=.01,plot_pdp=T,rug=T, xaxt = "s")
plot.amdp(rf_amdp,frac_to_plot=.01,plot_pdp=T,rug=T, centered = T )
cluster.amdp(amdp_obj=rf_amdp,nClusters=2, plot_legend=T)


rf_amdp2 = amdp(object = rf, X = demo2[,-1], y = demo2[,1], predictor = "EDUCATION")
partialPlot(rf,pred.data=demo2[,-1],x.var="EDUCATION")
par(mfrow = c(1,2))
plot.amdp(rf_amdp2,frac_to_plot=.002,plot_pdp=T,rug=T)
cluster.amdp(amdp_obj=rf_amdp2,nClusters=2, plot_legend = T)

table(demo2$EDUCATION)