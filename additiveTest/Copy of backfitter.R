#fits 
# \hat{f}(x) = \hat{g}_{1}(x_S)+\hat{g}_{2}(x_C)


backfitter = function(X, y, predictor, fitMethod, eps = .01, iter.max=10, verbose=TRUE,...){

	N = nrow(X)
#	X[, predictor] = X[, predictor]	+ runif(N)*sd(y)*.0001 #break ties.

	#order by the predictor
	xorder = order(X[, predictor])
	X = X[xorder, ]
	y = y[xorder]

	Xc = X[, -predictor]
	Xs = as.vector(X[, predictor])	

	#initialize
	g1_of_Xs = rep(0,N)
	g2_of_Xc = rep(0,N)
	yhat_curr = g1_of_Xs+g2_of_Xc
	current_g2 = NULL
	
	OneStep = function(){
		#do g2 first
		new_g2_mod = fitMethod(X=Xc, y=(y-g1_of_Xs), run_in_sample = FALSE, use_missing_data = TRUE, use_missing_data_dummies_as_covars = FALSE)
		new_g2 = predict(new_g2_mod, X) 
		new_g1 = supsmu(x=Xs, y=(y-new_g2))$y
		return(list(new_g1=new_g1,new_g2=new_g2,new_g2_mod=new_g2_mod))
	}

	delta = Inf
	iter = 0
	while( delta > eps && iter <= iter.max){
		#one iteration
		nextStep = OneStep()
		
		#compute delta
		delta = sum((nextStep$new_g1 - g1_of_Xs)^2) / sum(g1_of_Xs^2)
    delta = delta + sum((nextStep$new_g2 - g2_of_Xc)^2) / sum(g2_of_Xc^2)  

		#update
		current_g2 = nextStep$new_g2_mod
		g1_of_Xs = nextStep$new_g1
		g2_of_Xc = nextStep$new_g2
		iter = iter + 1
		
		#print message
		if(verbose){
			cat(paste("iter",iter," delta: ", round(delta,3),sep=""))
			cat("\n")
		}
	}
	#leaving us with...
	bf_obj = list(g1_of_Xs=g1_of_Xs, g2_of_Xc = g2_of_Xc, g2_mod=current_g2,X=X,y=y,
				 predictor = predictor, iter=iter,delta=delta)
	class(bf_obj) = "backfit_obj"
	return(bf_obj)
}
