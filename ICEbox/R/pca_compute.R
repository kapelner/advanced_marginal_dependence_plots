library(ICEbox)

calculate_pca = function(train, test=NULL, keep_original=FALSE, scale=F, frac_var = .50){
  pca <- prcomp(train, scale=scale)
  calculated_pcs = NULL
  if(!is.null(test)){
    calculated_pcs <- predict(pca, newdata=test)
  }
  
  eig_vars_pct = pca$sdev^2/sum(pca$sdev^2)
  cum_eig_vars_pct = cumsum(eig_vars_pct)
  num_pcs = which.min(cum_eig_vars_pct[which(cum_eig_vars_pct >= frac_var)])
  print(num_pcs)
  if (keep_original == T){
    train_pca = cbind(train, pca$x[, 1: num_pcs])
    if(!is.null(test)){
      test_pca = cbind(test, calculated_pcs[, 1 : num_pcs])
    }
  }else{
    train_pca = pca$x[ , 1 : num_pcs]
    if(!is.null(test)){
      test_pca = calculated_pcs[ , 1 : num_pcs]
    }
  }
  ###
  # for reference, (assuming we keep all columns)
  #  train_pca = (train - CENTER ) %*% pca$rotation
  #  where CENTER is a matrix dim = dim(train) whose rows are = pca$center.
  #  If scale=TRUE then 
  #  train_pca = ((train - CENTER )/SCALE ) %*% pca$rotation
  #  where SCALE  is a matrix dim = dim(train) whose rows are = pca$scale.
  ##
  pca_result = list(train_pca = train_pca, test_pca = test_pca, calc = pca)
  return(pca_result)
}

library(mlbench)
data(BostonHousing)
bh = BostonHousing
bh = bh[,-4]
pc = prcomp(bh[,-13])
npcs = 10
dat = data.frame(cbind(pc$x[,1:npcs],bh$medv)); colnames(dat)[npcs+1]="medv"
library(randomForest)
rf = randomForest(medv~.,dat); rf2 = randomForest(medv~.,bh)
X = bh[,-13]
hope = ice_pca(object=rf,X=X,predictor="lstat",prcomp_obj=pc, npcs=npcs)
hope2 = ice_pca(object=rf2,X=X,predictor="lstat")

hope = ice_pca(object=rf,X=X,predictor="rm",prcomp_obj=pc, npcs=npcs)
hope2 = ice_pca(object=rf2,X=X,predictor="rm")