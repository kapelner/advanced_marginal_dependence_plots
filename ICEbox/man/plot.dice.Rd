\name{plot.dice}
\alias{plot.dice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of \code{dice} objects.
}
\description{
Plotting of \code{dice} objects.
}
\usage{
\method{plot}{dice}(x, plot_margin = 0.05, frac_to_plot = 1, 
      plot_sd = TRUE, plot_orig_pts_deriv = TRUE, pts_preds_size = 1.5, 
      colorvec, color_by = NULL, x_quantile = FALSE, plot_dpdp = TRUE, 
      plot_new_data = FALSE, rug = TRUE, ...)
}
\arguments{
  \item{x}{
Object of class \code{dice} to plot.
}
  \item{plot_margin}{
Extra margin to pass to \code{ylim} as a fraction of the range of \code{ice_obj$ice_curves}.
}
  \item{frac_to_plot}{
If \code{frac_to_plot} is less than 1, randomly plot \code{frac_to_plot} fraction of the
curves in \code{ice_obj$ice_curves}.
}
  \item{plot_sd}{
If \code{TRUE}, below the derivative plots, the cross-observation sd of partial derivatives
is plotted at each gridpoint.
}
  \item{plot_orig_pts_deriv}{
If \code{TRUE}, marks each curve at the location of the derivative estimate at the
location of \code{predictor} actually occurring in the data. If \code{FALSE}
no mark is drawn.
}
  \item{pts_preds_size}{
Size of points to make if \code{plot_origin_pts_deriv} is \code{TRUE}.
}
  \item{colorvec}{
Optional vector of colors to use for each curve.
}
  \item{color_by}{
Optional variable name (or column number) to color curves by. If \code{color_by} has 10 or fewer
unique values, a discrete set of colors is used for each value and a legend is printed (and returned).
If there are more values, curves are colored from light to dark corresponding to low to high values of
\code{color_by}.
}
  \item{x_quantile}{
If \code{TRUE}, the plot is drawn with the x-axis taken to be \code{quantile(gridpts)}. If \code{FALSE},
the predictor's original scale is used.
}
  \item{plot_dpdp}{
If \code{TRUE}, the estimated derivative of the PDP is plotted and highlighed in yellow.
}
  \item{plot_new_data}{
Adam???
}
  \item{rug}{
If \code{TRUE}, marks are drawn on the x-axis indicating the presence of actual observed values.
Forced to \code{FALSE} when \code{x_quantile} is set to \code{TRUE}.
}
  \item{\dots}{
Additional plotting arguments.
}
}
\value{
A list with the following elements.
  \item{plot_points_indices}{Row numbers of \code{Xice} of those observations presented in the plot.}
  \item{legend_text}{If the \code{color_by} argument was used, 
		a legend describing the map between the \code{color_by} predictor
        and curve colors.}
}

\seealso{
dice
}
\examples{
\dontrun{
require(ICEbox)
require(randomForest)
require(MASS) #has Boston Housing data, Pima

data(Boston) #Boston Housing data
X = Boston
y = X$medv
X$medv = NULL

## build a RF:
bh_rf = randomForest(X, y)

## Create an 'ice' object for the predictor "age":
bh.ice = ice(object = bh_rf, X = X, y = y, predictor = "age",
            frac_to_build = .1) 

# estimate derivatives, then plot.
bh.dice = dice(bh.ice)
plot(bh.dice)
}
}
