library(amdp)
library(randomForest)
library(gbm)
library(nnet) #figure out how to use this thing...

####
full_dnames <- c("abalone", "ankara", "baseballsalary", "compactiv", "cpu", "ozone", "pole", "triazine", "wine_red", "wine_white")
#dnames = c("baseballsalary","wine_red", "ozone","ankara","wine_white")
dnames = c("ozone","ankara","wine_white")
dataset_dir = "/home/alex/workspace/advanced_marginal_dependence_plots/BakeoffDatasets/"
studyDir = "/home/alex/workspace/advanced_marginal_dependence_plots/realDataStudy"
#dataset_dir = "C:/Users/jbleich/workspace/advanced_marginal_dependence_plots/BakeoffDatasets/"

dataset = list() 
for(name in dnames){
   dataset[[name]] = read.csv(paste(dataset_dir,name,".csv", sep = ""), header = TRUE)
}		

getFormula <- function(dframe){
	ff <- paste(names(dframe)[1],"~.",sep='')
	formula(ff)
}

#dataset = dataset[[1]]

#called for its side-effects
datasetPics = function(dataset,picturedir){
	
	X = dataset[,-1]; 
	#for(i in 1:ncol(X)){
	#	X[,i] = as.numeric(X[,i])
	#}
	y = dataset[,1]
	predictors = names(X)
	N = nrow(X)
	frac_to_plot = 300/N; frac_to_plot = min(frac_to_plot, 1)   #suitable frac_to_plot

	form = getFormula(dataset)
	rf_mod  = randomForest(form, data = dataset)
	gbm_mod  = gbm(form, data= dataset, n.tree = 500, interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, verbose=FALSE)
	ntree = gbm.perf(gbm_mod, method = "cv", plot.it=FALSE)
	  
    X_std = scale(x = X, center = T, scale = T)
	X_center = attributes(X_std)$`scaled:center`  
	X_scale = attributes(X_std)$`scaled:scale`  
  	nnet_mod = nnet(x = X_std, y = as.matrix(y), size = ncol(X), maxit = 500, decay = 5e-4, linout = ifelse(is.factor(y), F, T))
  	pad_study = list()

	#list for each "technology"
	pad_study[["rf"]] = list()
	pad_study[["gbm"]] = list()
	pad_study[["nnet"]] = list()

	mod_names = c("rf","gbm","nnet")

	#save down the models
	pad_study[["rf"]]$mod = rf_mod
	pad_study[["gbm"]]$mod = gbm_mod;  pad_study[["gbm"]]$mod_parms = ntree	
	pad_study[["nnet"]]$mod = nnet_mod

	for(pred_name in predictors){
		print(pred_name)
		amdp_name = paste(pred_name,"amdp",sep="_")
		damdp_name = paste(pred_name,"damdp",sep="_")

		for(this_mod in mod_names){
			print(this_mod)

			if(this_mod == "rf"){
					pad_study[[this_mod]][[amdp_name]] = amdp(pad_study[[this_mod]]$mod, X=X, predictor=pred_name, y=y)
			}
			if(this_mod == "gbm"){
					pad_study[[this_mod]][[amdp_name]] = amdp(gbm_mod, X=X, predictor=pred_name, 
						predictfcn = function(object, newdata){predict(object, newdata, n.tree = ntree)}, y=y)
          	}
			if(this_mod == "nnet"){
		        pad_study[[this_mod]][[amdp_name]] = amdp(nnet_mod, X=X, predictor=pred_name, 
		          predictfcn = function(object, newdata){
		            newdata_std = scale(newdata, center = X_center, scale = X_scale)
		            predict(object, newdata_std)
		            }, y=y)      
            }
			
			#2nd round = create damdp
			pad_study[[this_mod]][[damdp_name]] = damdp(pad_study[[this_mod]][[amdp_name]])			
			
			#create plots of amdp, c-amdp, d-amdp
			par(mfrow=c(1,3))
			plot(pad_study[[this_mod]][[amdp_name]], frac_to_plot=frac_to_plot, plot_pdp=TRUE,
					main=paste(pred_name,":",this_mod,sep=" "))
			plot(pad_study[[this_mod]][[amdp_name]], centered=TRUE,centered_percentile=0.01,frac_to_plot=frac_to_plot)
			plot(pad_study[[this_mod]][[damdp_name]],frac_to_plot=frac_to_plot,plot_sd=TRUE,plot_dpdp=TRUE)

			fname= paste(paste(pred_name,this_mod,sep="_"),"pdf",sep=".")
			dev.copy2pdf(file=paste(picturedir,fname,sep="/"))
			dev.off()

			#with quantiles now
			par(mfrow=c(1,3))
			plot(pad_study[[this_mod]][[amdp_name]], frac_to_plot=frac_to_plot, plot_pdp=TRUE, x_quantile=TRUE,
						main=paste("q",pred_name,":",this_mod,sep=" "))
			plot(pad_study[[this_mod]][[amdp_name]],centered=TRUE,centered_percentile=0.01,frac_to_plot=frac_to_plot, x_quantile=TRUE)
			plot(pad_study[[this_mod]][[damdp_name]],frac_to_plot=frac_to_plot,plot_sd=TRUE,plot_dpdp=TRUE, x_quantile=TRUE)

			fname= paste(paste(pred_name,"q",this_mod,sep="_"),"pdf",sep=".")
			dev.copy2pdf(file=paste(picturedir,fname,sep="/"))			
			dev.off()
			cat("\n\n",pred_name,this_mod,"\n\n")
		}
	}
	fname = paste(picturedir,"pad_study.Rda",sep="/")
	save(pad_study,file=fname,compress=TRUE) #gives an .rda for each.
}


for(name in dnames){
	picturedir = paste(studyDir,name,sep="/")
	dir.create(picturedir)
	datasetPics(dataset[[name]],picturedir)
}



