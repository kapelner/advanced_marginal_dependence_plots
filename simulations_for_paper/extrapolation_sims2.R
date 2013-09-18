library(amdp)
library(mlbench)
library(randomForest)

#d = mlbench.peak(n=1000, d = 2)

sim1 = function(){
	s = runif(1)
	if(s <  (1/3)){
		x1 = runif(1,min=-1,max=0)
		x2 = runif(1,min=-1,max=0)
	}
	else{
		if( s < (2/3)){
			x1 = runif(1,min=-1,max=0)
			x2 = runif(1,min=0,max=1)
		}
		else{
			x1 = runif(1,min=0,max=1)
			x2 = runif(1,min=-1,max=0)
		}
	}
	return(cbind(x1,x2))
}

simN <- function(N,b1=10,b2=1,sd=.1){
	X = matrix(NA,ncol=2,nrow=N)
	for(i in 1:N){X[i,] = sim1()}

	Y = b1*X[,1]^2 + b2*(X[,2]>0)+rnorm(N,sd=sd)
	df = as.data.frame(cbind(Y,X))
	names(df) = c("Y","x1","x2")
	df
}


data = simN(1000)

rf_mod = randomForest(Y~.,data)
rf_amdp = amdp(rf_mod,X=data[,2:3],predictor="x1")

#x2_indic = 1 <==> no extrapolation 
rf_amdp$Xamdp$x2_indic = ifelse(rf_amdp$Xamdp$x2>0,0,1) 

no_extrap_indices = which(rf_amdp$Xamdp$x2_indic==1)
extrap_indices = which(rf_amdp$Xamdp$x2_indic==0)

xtrap = rf_amdp
no_xtrap = rf_amdp


### full plot
plot(rf_amdp, plot_pdp=FALSE, color_by="x2_indic")

### plot of extraps in green
xtrap$apdps = xtrap$apdps[extrap_indices,]
xtrap$actual_prediction = xtrap$actual_prediction[extrap_indices] 
xtrap$xj = xtrap$xj[extrap_indices] 
plot(xtrap, plot_pdp=FALSE, colorvec = rep("green",N))

### plot of no extraps in red
no_xtrap$apdps = no_xtrap$apdps[no_extrap_indices,]
no_xtrap$actual_prediction = no_xtrap$actual_prediction[no_extrap_indices] 
no_xtrap$xj = no_xtrap$xj[no_extrap_indices] 
plot(no_xtrap, plot_pdp=FALSE, colorvec = rep("red",N))
