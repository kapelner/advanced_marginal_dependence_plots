\name{dice}
\title{
Creates an object of class \code{dice}.
}
\description{
Estimates the partial derivative function for each curve in an \code{ice} object.
}
\usage{
dice(ice_obj, DerivEstimator)
}
\arguments{
  \item{ice_obj}{
Object of class \code{ice}. This function generages partial derivatie 
estimates for each row in \code{ice$ice_curves}.
}
  \item{DerivEstimator}{
Optional function with a single argument \code{y}.  Returns the estimated 
partial derivative a function sampled at the points (\code{ice_obj$gridpts},\code{y}).
If ommited, the default (a) smooths (\code{ice_obj$gridpts},\code{y}) using
\code{supsmu} and then (b) uses the \code{D1tr} ("discrete first derivative using simple difference ratios")
function found in Package \code{sfsmisc} to estimate the derivative.
}
}
\value{
A list of class 'dice' with the following elements. Most are passed directly through
from \code{ice_object} and exist to enable various plotting facilities.

\item{d_ice_curves}{Matrix of dimension \code{nrow(Xice)} by \code{length(gridpts)}.
Each row corresponds to an observation's d-ICE curve, estimated at the values of \code{predictor} set to the values specified by 
\code{gridpts}.}
\item{gridpts}{Passed from \code{ice_object}.}
\item{predictor}{Passed from \code{ice_object}.}
\item{xj}{The actual values of \code{predictor} observed in the data in the order
of \code{Xice}.}
\item{actual_deriv}{Vector of length \code{nrow(Xice)} containing the estimated partial derivatives
at the value of the \code{predictor} actually found in \code{Xice}.}
\item{sd_deriv}{Vector of length \code{length(gridpts)} with the cross-observation sd of partial derivative
estimates.  For instance \code{sd_deriv[1]} equals \code{sd(d_ice_curves[,1])}.}
\item{logodds}{Passed from \code{ice_object}.  If \code{TRUE}, \code{d_ice_curves} are
estimated derivatives of the centered log-odds.}
\item{xlab}{Passed from \code{ice_object}.}
\item{nominal_axis}{Passed from \code{ice_object}.}
\item{range_y}{Passed from \code{ice_object}.}
\item{Xice}{Passed from \code{ice_object}.}
\item{dpdp}{The estimated partial derivative of the PDP.}

}
\references{
See... section ..
}
\author{
%%  ~~who you are~~
}

\seealso{
plot.dice, print.dice, summary.dice
}

\examples{
 \dontrun{
# same examples as for 'ice', but now create a derivative estimate as well.
require(ICEbox)
require(randomForest)
require(MASS) #has Boston Housing data, Pima

########  regression example
data(Boston) #Boston Housing data
X = Boston
y = X$medv
X$medv = NULL

## build a RF:
bh_rf = randomForest(X, y)

## Create an 'ice' object for the predictor "age":
bh.ice = ice(object = bh_rf, X = X, y = y, predictor = "age",
                    frac_to_build = .1)

# make a dice object:
bh.dice = dice(bh.ice)

#### classification example
data(Pima.te)  #Pima Indians diabetes classification
X = Pima.te[,-8]
y = Pima.te[, 8]

## build a RF:
pima_rf = randomForest(x = X, y = y)

## Create an 'ice' object for the predictor "skin":
# For classification we plot the centered log-odds. If we pass a predict
# function that returns fitted probabilities, setting logodds = TRUE instructs
# the function to set each ice curve to the centered log-odds of the fitted 
# probability. 
pima.ice = ice(object = pima_rf, X = X, predictor = "skin", logodds = TRUE,
                    predictfcn = function(object, newdata){ 
                         predict(object, newdata, type = "prob")[, 2]
                    }
              )

# make a dice object:
pima.dice = dice(pima.ice)
}

}
