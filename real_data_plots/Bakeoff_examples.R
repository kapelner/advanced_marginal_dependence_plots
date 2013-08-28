library(amdp)
library(sfsmisc)
library(randomForest)
setwd("C:/Users/pitkin/Hello-World/advanced_marginal_dependence_plots/BakeoffDatasets")
source("C:/Users/pitkin/Hello-World/advanced_marginal_dependence_plots/amdp/R/damdp.R")
source("C:/Users/pitkin/Hello-World/advanced_marginal_dependence_plots/amdp/R/amdp.R")
source("C:/Users/pitkin/Hello-World/advanced_marginal_dependence_plots/amdp/R/plot.amdp.R")
source("C:/Users/pitkin/Hello-World/advanced_marginal_dependence_plots/amdp/R/plot.damdp.R")

#Create list of bakeoff datasets

dataset = list() 
for(name in c("abalone", "ankara", "baseballsalary", "compactiv", "cpu", "ozone", "pole", "triazine", "wine_red", "wine_white")){
  dataset[[name]] = read.csv(paste(name, ".csv", sep = ""), header = TRUE)
}

#Ankara

ankara = dataset[["ankara"]]
names(ankara)
X = ankara[,-1]
ankara_rf = randomForest(meantemp ~ ., data = ankara)
maxtemp = amdp(object = ankara_rf, X = X, y = ankara[,1], predictor = "maxtemp")
mintemp = amdp(object = ankara_rf, X = X, y = ankara[,1], predictor = "mintemp")
dewpoint = amdp(object = ankara_rf, X = X, y = ankara[,1], predictor = "dewpoint")
precip = amdp(object = ankara_rf, X = X, y = ankara[,1], predictor = "precip")
sea.level.pressure = amdp(object = ankara_rf, X = X, y = ankara[,1], predictor = "sea.level.pressure")
standard.pressure = amdp(object = ankara_rf, X = X, y = ankara[,1], predictor = "standard.pressure")
visibility = amdp(object = ankara_rf, X = X, y = ankara[,1], predictor = "visibility")
wind.speed = amdp(object = ankara_rf, X = X, y = ankara[,1], predictor = "wind.speed")
max.wind.speed = amdp(object = ankara_rf, X = X, y = ankara[,1], predictor = "max.wind.speed")


#put the amdp objects in a list
ankara_vars = list()
for(var_name in 1 : (ncol(ankara) - 1)){
  ankara_vars[[var_name]] = get(names(ankara)[-1][var_name])
}

#list of variable names
ankara_names = names(ankara)[-1]


#create damdp objects
for(i in 1 : length(ankara_vars)){
  assign(paste("d", ankara_names[i], sep=""), damdp(ankara_vars[[i]]))
}

windows()

#plot amdps

for(amdp_objs in ankara_vars){
  plot.amdp(amdp_objs, frac_to_plot = 0.1, x_quantile = T)
  readline("Press enter when ready")
} 

#plot damdps
for(i in 1 : length(ankara_vars)){
  damdp_obj_cur = get(paste("d",ankara_names[i], sep = ""))
  plot(damdp_obj_cur, plot_sd = T, frac_to_plot = 0.1, plot_dpdp = T)
  readline("Press enter when ready")
}



#######################

#Red Wine

wine_red = dataset[["wine_red"]]
names(wine_red)
X = wine_red[,-1]
wine_red_rf = randomForest(quality ~ ., data = wine_red)
fixed.acidity = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "fixed.acidity")
volatile.acidity = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "volatile.acidity")
citric.acid = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "citric.acid")
residual.sugar = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "residual.sugar")
chlorides = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "chlorides")
free.sulfur.dioxide = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "free.sulfur.dioxide")
total.sulfur.dioxide = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "total.sulfur.dioxide")
density = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "density")
pH = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "pH")
sulphates = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "sulphates")
alcohol = amdp(object = wine_red_rf, X = X, y = wine_red[,1], predictor = "alcohol")




#put the amdp objects in a list
wine_red_vars = list()
for(var_name in 1 : (ncol(wine_red) - 1)){
  wine_red_vars[[var_name]] = get(names(wine_red)[-1][var_name])
}

#list of variable names
wine_red_names = names(wine_red)[-1]


#create damdp objects
for(i in 1 : length(wine_red_vars)){
  assign(paste("d", wine_red_names[i], sep=""), damdp(wine_red_vars[[i]]))
}

windows()

#plot amdps

for(amdp_objs in wine_red_vars){
  plot.amdp(amdp_objs, frac_to_plot = 0.1, x_quantile = T)
  readline("Press enter when ready")
} 

#plot damdps
for(i in 1 : length(wine_red_vars)){
  damdp_obj_cur = get(paste("d",wine_red_names[i], sep = ""))
  plot(damdp_obj_cur, plot_sd = T, frac_to_plot = 0.1, plot_dpdp = T)
  readline("Press enter when ready")
}

























