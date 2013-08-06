###### script for example 2 in the paper
library(randomForest)
library(gbm)   #boost
library(e1071) #svm
library(nnet)  #nnet
library(gam)   #gam
library(amdp)
library(scatterplot3d);

# generates X and y:
ex2a_sim = function(n,seednum=NULL){
	if(!is.null(seednum)){
		set.seed(seednum)
	}
	p = 3
	X = as.data.frame(matrix(runif(n*p,min=-1,max=1), ncol = p, nrow = n))
	X[, 2] = (X[,1] < 0)*abs(X[,2])
	X[, 3] = (X[,1] > 0)*abs(X[,3])
		
	X = X[order(X[, p]), ]
	colnames(X) = paste("x_", 1 : p, sep = "")
	bbeta1 = as.matrix(c(4, 2, 2))

	#make X dependent...
	y = as.matrix(X) %*% bbeta1	
	y = y + rnorm(n,sd=1)
	Xy = as.data.frame(cbind(X, y))
	return(list(Xy=Xy,X=X,y=y))
}

ex2a_sim = function(n,seednum=NULL,coefs=c(10,10),sig=1){
	if(!is.null(seednum)){
		set.seed(seednum)
	}
	p = 2
	X = as.data.frame(matrix(runif(n*p,min=-1,max=1), ncol = p, nrow = n))
	#X[, 2] = (X[,1] < 0)*abs(X[,2])
	#X[, 3] = (X[,1] > 0)*abs(X[,3])
		
	X = X[order(X[, p]), ]
	colnames(X) = paste("x_", 1 : p, sep = "")
	bbeta1 = coefs
	#bbeta2 = as.matrix(c(-5, -5))

	#make X dependent...
	y = bbeta1[1]*X[,1]^2 - bbeta1[2]*X[,2]^2
	y = y + rnorm(n,sd=sig)

	keep = (X[,1]<0)*(X[,2]<0) + (X[,1]>0)*(X[,2]>0)
	Xy = as.data.frame(cbind(X, y))
	Xykeep = Xy[keep==1,]
	Xykill = Xy[keep==0,]
	return(list(Xy=Xy,X=X,y=y, Xykeep=Xykeep, Xykill=Xykill, keep=keep))
}


#generate data
ex2_data = ex2a_sim(n=2000,coefs=c(10,10))
Xy = ex2_data$Xy
X  = ex2_data$X
y  = ex2_data$y
Xykeep = ex2_data$Xykeep
Xykill = ex2_data$Xykill
#explore
#all
scatterplot3d(x=Xy$x_1,y=Xy$x_2,z=Xy$y)
scatterplot3d(x=Xykeep$x_1,y=Xykeep$x_2,z=Xykeep$y)
scatterplot3d(x=Xykill$x_1,y=Xykill$x_2,z=Xykill$y)
#


#build all models
rf_mod = randomForest(y~.,Xykeep)
gam_mod = gam(y~s(x_1)+s(x_2)+s(x_3),data=Xy)
gbm_mod = gbm(y ~ ., data = Xy, n.tree = 500, interaction.depth = 3, shrinkage = 0.1, cv.folds = 5)

#amdps
a1_rf  = amdp(rf_mod, Xykeep, predictor = 1, frac_to_build = 0.3)
a1_gam = amdp(gam_mod, X, predictor = 1, frac_to_build = 0.3)
a1_gbm = amdp(gbm_mod, X, predictor = 1, frac_to_build = 0.3)

xamdp = a1_rf$Xamdp
apdps = a1_rf$apdps
zlim = range(apdps)*1.05
grid = a1_rf$gridpts
ngrid = length(grid)

sweetplot = scatterplot3d(x=grid,y=rep(xamdp$x_2[1],ngrid), z=as.vector(apdps[1, ]),type='l',zlim=zlim,xlim=c(-1,1),ylim=c(-1,1))
for(i in 1:nrow(a1_rf$apdps)){
	sweetplot$points3d(x=grid, y = rep(xamdp$x_2[i],ngrid), z=as.vector(apdps[i, ]), type = "l")
}
sweetplot$plane3d(x=0, y=0,Intercept=-15)
sweetplot$points3d(x=xamdp$x_1, y=xamdp$x_2, z=rep(-15,nrow(xamdp)),col="red")



#how does random forest do in mse sense?
rf_mse = rep(NA,100)
for(i in 1:100){
	train = ex1_sim(1000)
	test  = ex1_sim(1000)
	fhat = randomForest(train$X, train$y)
	rf_mse[i] = mean( (test$y - predict(fhat, test$X) )^2 )
	cat(i); cat(" ")
}


ntree = gbm.perf(gbm_mod, method = "cv")
summary(gbm_mod)
lm_mod = lm(y ~ . * . * ., Xy)
summary(lm_mod)

library(amdp)
amdp_obj_rf = amdp(rf_mod, X, predictor = 1, frac_to_build = 0.5)

#plot only 10% of curves with quantiles, actual pdp, and original points. 
plot(amdp_obj_rf, x_quantile = F, plot_pdp = T, frac_to_plot = 0.5)
amdpCluster(amdp_obj_rf, nClusters = 2)

set.seed(1989)
amdp_obj_gbm = amdp(gbm_mod, X, predictor = 3, predictfcn = function(object, newdata){predict(object, newdata, n.tree = ntree)}, frac_to_build = 1)
set.seed(1989)
plot(amdp_obj_gbm, plot_pdp = T, frac_to_plot = 0.01)
windows()
cluster.amdp(amdp_obj_gbm, nClusters = 2, colorvec = c(rgb(0.4, 0.4, 0.4), rgb(0.8, 0.8, 0.8)))

set.seed(1989)
amdp_obj_lm = amdp(lm_mod, X, predictor = 3, frac_to_build = 0.5)

#plot only 10% of curves with quantiles, actual pdp, and original points. 
windows()
set.seed(1989)
plot(amdp_obj_lm, plot_pdp = T, frac_to_plot = 0.015)
amdpCluster(amdp_obj_lm, nClusters = 2)



