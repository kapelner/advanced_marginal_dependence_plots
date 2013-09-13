###### script for example in the paper
# illustrating the 'parallel curves' idea.
library(randomForest)
library(gbm)
library(gam)
library(amdp)

# generates X and y:
ex_sim = function(n,seednum=NULL){
	if(!is.null(seednum)){
		set.seed(seednum)
	}
	p = 2
	X = as.data.frame(matrix(runif(n * p, -1, 1), ncol = p))	
	colnames(X) = paste("x_", 1 : p, sep = "")
	bbeta = c(1,1)

	y = bbeta[1] * X[,1]^2  + bbeta[2] * X[,2]
	y = y + rnorm(n)
	Xy = as.data.frame(cbind(X, y))
	return(list(Xy=Xy,X=X,y=y))
}

#generate data
ex_data = ex_sim(1000)
Xy = ex_data$Xy
X  = ex_data$X
y  = ex_data$y

#build all models
gam_mod1 = gbm(y~s(x_1)+s(x_2),data=Xy)  #no interactions by definition.
gam_mod2 = gbm(y~s(x_1)+s(x_2)+s(x_1*x_2),data=Xy)   #interactions
rf_mod  = randomForest(y~.,data=Xy)

x1_gam1_amdp = amdp(gam_mod1, X, predictor = 1, frac_to_build = 1)
x1_gam1_dampd = damdp(x1_gam1_amdp)
x1_gam2_amdp = amdp(gam_mod2, X, predictor = 1, frac_to_build = 1)
x1_rf_amdp = amdp(rf_mod, X, predictor = 1, frac_to_build = 1)

#plot only 10% of curves with quantiles, actual pdp, and original points. 
plot(x1_gam1_amdp$gridpts,apply(x1_gam_amdp$apdps,2,mean),xlab="x_1",ylab="yhat",type='l',lwd=3, ylim=range(x1_gam_amdp$apdps)*1.05)

plot(x1_gam1_amdp, x_quantile = F, plot_pdp = F, frac_to_plot = 0.05)

#caution: alex's machine
dev.copy2pdf(file="/home/alex/Dropbox/amdp_project/paper/no_inter_example.pdf")

plot(x1_gam1_damdp, x_quantile = F, plot_dpdp = F, frac_to_plot = 0.3,plot_sd=FALSE,
      plot_orig_pts_deriv=FALSE)
dev.copy2pdf(file="/home/alex/Dropbox/amdp_project/paper/no_inter_example_dpad.pdf")
