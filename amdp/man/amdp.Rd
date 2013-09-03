\name{amdp}
\alias{amdp}
\title{
Create amdps
}
\description{
Create amdps
}
\usage{
amdp(object, X, y, predictor, predictfcn, newdata, verbose = TRUE, 
		plot = FALSE, frac_to_build = 1, indices_to_build = NULL, num_grid_pts,
    logodds = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
A fitted model of any kind.
}
\item{X}{
Matrix or data frame of predictors (observations along rows).  To the extent that \code{object} requires
predictors in a certain format, \code{X} must obey that format. Note that \code{X} does not include the response.
}
\item{y}{
Optional vector of responses of the dataset, used to return the range of observed values. Cannot be a factor. If missing, returns the range of all points in the amdp grid.
}
\item{predictor}{
Column number of \code{X} for which to calculate partial dependence.  Alternatively \code{predictor} can be a string corresponding to a column name in \code{X}.
}
\item{predictfcn}{
Optional prediction function to use with \code{object}. Must accept arguments \code{object} and \code{newdata}. The software does minimal further checking -- it is up to the user to ensure that \code{predictfcn(object = object, newdata = X)} is supported. If not passed, the function looks for the default \code{predict} function associated with \code{class(object)}.
}
\item{}
\item{verbose}{
  If \code{verbose} is \code{TRUE} we print updates.
}
\item{plot}{
If \code{plot} is \code{TRUE} we make a plot using default plot settings at the end of the routine.
}
\item{frac_to_build}{
Fraction of curves to build. If less than 1, the rows chosen correspond to equally distributed  quantiles of \code{predictor}.
}


\item{indices_to_build}{
If specified, program builds objects from the rows specified in \code{indices_to_build}
}

\item{num_grid_pts}{
The number of points along \code{X[, predictor]} at which the program will predict. If unspecified, it will predict at all unique points. If the number of unique points is less than the number of grid points specified, the program will warn the user and the number of unique points will be used.
}
\item{logodds}{
If TRUE, the predicted values (which should be probabilities), are converted to logits
before the object is returned. 
}

\item{\dots}{
Other parameters to be passed.
}

}
\value{If assigned, returns an object of class \code{amdp}. The object is a list with elements:\cr
\item{apdps}{An \code{nrow(X)} by \code{length(gridpts)} matrix where row i is observation i's marginal dependence curve.}
\item{gridpts}{A vector of the unique grid points at which all marginal curves are evaluated.}
\item{predictor}{The predictor used. If possible, returns the predictor's name; if not, returns column index in \code{X}.}
\item{xj}{The actual value of \code{X[, predictor]} for each observation.}
\item{actual_prediction}{The predicted value for each observation at its actual vector of predictors.}
\item{logodds}{The \code{logodds} specified by the user, either TRUE or FALSE.}
\item{xlab}{The x-axis label used for plotting}
\item{nominal_axis}{Whether or not this variable is treated as a nominal variable for plotting purposes (if it has more than 5 unique values, it's not).}
\item{N}{The number of rows in the inputted matrix or data frame}
\item{range_y}{If \code{y} is passed as an argument, the range of values. Otherwise the range of values in \code{apdps}.}
\item{Xamdp}{A matrix whose i-th row is the true X-vector associated with the observation in the i-th row of \code{apdps}. Used by the \code{plot} function to color by levels of a categorical variable.}			
\item{pdp}{Friedman's partial dependence function}
}
\examples{
\dontrun{
require(randomForest)
require(MASS)
library(amdp)
data(Pima.te)

#fit a random forest
pima_rf = randomForest(type~., Pima.te)

#define a predict function that returns Prob(diabetes=Yes)
pima_predict = function(object, newdata){
  preds = predict(object, newdata, type="prob")
  col_idx = which(colnames(preds)=="Yes")
  preds[,col_idx] 
}

#get amdp object for the 'skin' predictor, but converted to logit.
pima_amdp = amdp(pima_rf, X = Pima.te[,1:7], predictor = "skin", predictfcn = pima_predict, logodds = T)

#plot only 10% of curves with quantiles, actual pdp, and original points. 

plot(pima_amdp, x_quantile = T, plot_pdp = T, frac_to_plot = .1, centered=T)

}
}
