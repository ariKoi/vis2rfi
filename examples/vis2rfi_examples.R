\dontrun{
# See vignette for a more detailed introduction.

# Using example and data from the forestFloor::forestFloor documentation  --------------------------------

### Start of forestFloor::forestFloor documentation :
#3 - binary regression example
#classification of two classes can be seen as regression in 0 to 1 scale
set.seed(1234)
library(forestFloor)
library(randomForest)
data(iris)
X = iris[-1:-50,!names(iris) %in% "Species"] #drop third class virginica
Y = iris[-1:-50,"Species"]
Y = droplevels((Y)) #drop unused level virginica

rf = randomForest(
  X,Y,
  keep.forest=TRUE,  # mandatory
  keep.inbag=TRUE,   # mandatory
  samp=20,           # reduce complexity of mapping structure, with same OOB%-explained
  importance  = TRUE # recommended, else giniImpurity
)

ff = forestFloor(rf,X,
                 calc_np=TRUE,    # mandatory to recalculate
                 binary_reg=TRUE) # binary regression, scale direction is printed
Col = fcol(ff,1) # color by most important feature
plot(ff,col=Col)   # plot features

#interfacing with rgl::plot3d
show3d(ff,1:2,col=Col,plot.rgl.args = list(size=2,type="s",alpha=.5))
# End of forestFloor::forestFloor documentation.

# Note that RGL device is already slow to enlarge and to manipulate, even with only 100 data points.
# In addition, this isn't the most representative example as in practice we might have a larger data set.
# Repeated usage of show3d might even crash the R session.
# Interaction effect : As Petal.Length and Petal.Width both increase in value, their combined contribution
# to the Random Forest prediction also rises.

library(vis2rfi)
# Let's take a 25% subsample of the data, and similarly examine the interaction effect of the first two
# most important variables i.e. as per Random Forest output.
# By default forestFloor sums the feature contributions to obtain the combined contribution. Hence, we
# update the outer value similarly.
# Also we do not standardize the variables so that we can more easily compare to the show3d result.
vis2rfi(impInd = c(1,2), ffObj = ff, rsf = 0.25, scaleXY = FALSE, outer = list(FUN = "+"))
# ✔ Taking the random subsample. An rsf value of 0.25 corresponds to 25 cases, out of the 100 ones.
# ✔ Computing the 3d visualization.
# ...examining...
# Notes & Diffs :
# - Visualization is a structure-like object composed of various data slices. This is in contrast to a
# single surface object, and as per package premise.
# - Easy to turn around and examine at any angle.
# - Rich in colorfulness and interactivity, since leveraging the plotly package.
# - Contour lines, as well as data point interactivity, help in discovering areas of both low and high
# contribution.
# - The interaction effect is similarly found as before but with an important difference: There are also
# data points exhibiting a high contribution but with both Petal.Length and Petal.Width having mid-to-low
# values. Place for example the mouse cursor on the object's apex and observe the red lined contours appearing on the feature plane.
# Such cases are more evident when viewing the interaction effect in this manner since their contribution is
# not as attenuated as when plotting all of the data.
# The premise underlying *vis2rfi* is that 2 way interaction effects, when viewed as a 3d object as opposed
# to a single surface, can be easier discovered and further examined in a richer manner.
# Furthermore, taking data subsamples and visually experimenting with them is an idea well rooted in
# statistical thinking.

# Let's take a different subsample, by changing the seed.
vis2rfi(impInd = c(1,2), ffObj = ff, rsf = 0.25, scaleXY = FALSE, seed = 007,
        outer = list(FUN = "+"))
# ✔ Taking the random subsample. An rsf value of 0.25 corresponds to 25 cases, out of the 100 ones.
# ✔ Computing the 3d visualization.
#
# A bit different shape is observed but with similar conclusions to the last plot above. This is due to the
# small size of this data set. In less contrived scenarios the difference would be more intense.

# Taking a random subsample does not guarantee having a variety of contribution values in the subsample.
# To account for this we can use the maxDissim argument so as to include in the subsample data points that
# better span the variety of values observed in the initial data set.
vis2rfi(impInd = c(1,2), ffObj = ff, rsf = 0.25, scaleXY = FALSE, maxDissim = TRUE,
        outer = list(FUN = "+"))
# Taking the random subsample.
# An rsf value of 0.25 corresponds to 25 cases, out of the 100 ones.
# Taking the random base 0.67 & pool 0.33 subsample percentage of rsf = 0.25 for maxDissim.
# Base subsample size is :
#   17 cases
# Pool subsample size is :
#   83 cases
# Computing the maxDissim object.
# Adding to base subsample 8 cases from the pool subsample, for a total of 25 cases.
# Computing the 3d visualization.

# Now the distinction in combined contribution between high and low values of the variables is also
# visible in other parts of the feature plane. Similarly as before, these can be viewed by placing
# the mouse cursor on the 3d object's apex.
# Note also how the depicted object has altered it's structure over these newly identified regions
# of interest at say point (x = 6.9, y = 1.4).
# If we take another data view, by changing the seed value, we can see this distinction a bit better.
vis2rfi(impInd = c(1,2), ffObj = ff, rsf = 0.25, scaleXY = FALSE, maxDissim = TRUE, seed = 007,
        outer = list(FUN = "+"))
# Taking the random subsample.
# An rsf value of 0.25 corresponds to 25 cases, out of the 100 ones.
# Taking the random base 0.67 & pool 0.33 subsample percentage of rsf = 0.25 for maxDissim.
# Base subsample size is :
#   17 cases
# Pool subsample size is :
#   83 cases
# Computing the maxDissim object.
# Adding to base subsample 8 cases from the pool subsample, for a total of 25 cases.
# Computing the 3d visualization.

# Observe how at the same data point i.e. (x = 6.9, y = 1.4), the object's "roof" tilts more vividly
# than in the previous visualisation.

# The main theme of vis2rfi is to repeatedly alter the function's parameters so as to view the 2 way
# contribution from a variety of perspectives, as shown for example here. The added benefit of doing
# this is expected to be more prominent in situations having a larger data size and/or a less easily
# established 2 way contribution to the predicted target.

# Visualising 3d objects can be helpful in extracting detailed insights of an important 2 way contribution.
# Sometimes though we would like to isolate the interaction effect so as obtain a clearer picture, which
# can then be shown to a less technical audience.

# We can portray the effect by a smoothed 3d surface that immediately shows us the important interaction
# and with a tilt towards the variable that contributes the most i.e. Petal.Length here, having a median
# marginal feature contribution of 0.85% as opposed to Petal.Width with a median of -15.34%.

# GAM technology is utilized in order to conduct the smoothing, and the default smoothing parameters in
# argument df are 4 degrees of freedom for each of the features.
vis2rfi(impInd = c(1,2), ffObj = ff, rsf = 0.25, scaleXY = FALSE, maxDissim = TRUE, seed = 007,
        outer = list(FUN = "+"), smoother = TRUE)
# Taking the random subsample.
# An rsf value of 0.25 corresponds to 25 cases, out of the 100 ones.
# Taking the random base 0.67 & pool 0.33 subsample percentage of rsf = 0.25 for maxDissim.
# Base subsample size is :
#   17 cases
# Pool subsample size is :
#   83 cases
# Computing the maxDissim object.
# Adding to base subsample 8 cases from the pool subsample, for a total of 25 cases.
# Fitting the GAM smoothing spline with 4 and 4 degrees of freedom respectively on each of the two interaction variables.
# Computing the 3d visualization.

# More succinctly than without smoothing, the surface's high upwards incline depicts the increase in the
# contribution to the prediction. In addition, the interaction effect has been established with only a
# 25% subset of the data. In many practical applications, this is what is mainly needed to convey the
# message.

# If we would like to further emphasize the interaction effect, we can flatten the 3d surface by lowering
# the smoothing spline degrees of freedom in the GAM fit to unit values i.e.
vis2rfi(impInd = c(1,2), ffObj = ff, rsf = 0.25, scaleXY = FALSE, maxDissim = TRUE, seed = 007,
        outer = list(FUN = "+"), smoother = TRUE, df = c(1,1))
# Resulting in a 3d plane for this data set while the tilt towards Petal.Length is more evident.

## Example with caret:
rm(list=ls())
library(caret)
# Loading required package: lattice
#
# Attaching package: ‘caret’
#
# The following objects are masked from ‘package:vis2rfi’:
#
#   minDiss, sumDiss
library(forestFloor)
N = 1000
vars = 7
noise_factor = .3
bankruptcy_baserate = 0.2
X = data.frame(replicate(vars,rnorm(N)))
y.signal = with(X,X1^2+X2^2+X3*X4+X5+X6^3+sin(X7*pi)*2) #some non-linear f
y.noise = rnorm(N) * sd(y.signal) * noise_factor
y.total = y.noise+y.signal
y = factor(y.total>=quantile(y.total,1-bankruptcy_baserate))

set.seed(1)
caret_train_obj <- train(
  x = X, y = y,
  method = "rf",
  keep.inbag = TRUE,  #always set keep.inbag=TRUE passed as ... parameter to randomForest
  ntree=50, #speed up this example, if set too low, forestFloor will fail
)

rf = caret_train_obj$finalModel #extract model
if(!all(rf$y==y)) warning("seems like training set have been resampled, using smote?")
ff = forestFloor(rf,X,binary_reg = T)

#... or simply pass train
ff = forestFloor(caret_train_obj,X,binary_reg = T)
plot(ff,1:6,plot_GOF = TRUE)

# Using vis2rfi:
library(vis2rfi)
# Focus on the 6th in order of importance variable, having the lowest R-squared value with the 5th one.
# These are the x3 and x4 variables known to interact:
vis2rfi(impInd = c(6,7), ffObj = ff, scaleXY = FALSE, outer = list(FUN = "+"))
vis2rfi(impInd = c(6,7), ffObj = ff, scaleXY = FALSE, maxDissim = TRUE, outer = list(FUN = "+"),
        smoother = TRUE)
}
