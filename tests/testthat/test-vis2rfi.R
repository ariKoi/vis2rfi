context("vis2rfi function argument values")

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

# indices of random forest important variables from forestFloor -----------

test_that("length of random forest important variables index vector is two", {
  expect_error(vis2rfi(impInd = c(1,2,3), ffObj = ff))
  expect_error(vis2rfi(impInd = 1, ffObj = ff))
})

test_that("index value of random forest important variable is less than 20", {
  expect_error(vis2rfi(impInd = c(1,20), ffObj = ff))
  expect_error(vis2rfi(impInd = c(1,21), ffObj = ff))
  expect_error(vis2rfi(impInd = c(22,10), ffObj = ff))
  expect_error(vis2rfi(impInd = c(25,30), ffObj = ff))
})

test_that("index value of random forest important variable is positive", {
  expect_error(vis2rfi(impInd = c(1,0), ffObj = ff), "Non-positive index value! Indices of random forest important variables should be positive. Select values in the impInd vector that are greater than 0.")
  expect_error(vis2rfi(impInd = c(-1,2), ffObj = ff), "Non-positive index value! Indices of random forest important variables should be positive. Select values in the impInd vector that are greater than 0.")
  expect_error(vis2rfi(impInd = c(0,0), ffObj = ff), "Non-positive index value! Indices of random forest important variables should be positive. Select values in the impInd vector that are greater than 0.")
  expect_error(vis2rfi(impInd = c(2,-5), ffObj = ff), "Non-positive index value! Indices of random forest important variables should be positive. Select values in the impInd vector that are greater than 0.")
})


# existence of class forestFloor object -----------------------------------

test_that("forestFloor has been run", {
  expect_error(vis2rfi())
  expect_error(vis2rfi(ffObj = NA))
  expect_error(vis2rfi(ffObj = data.frame(x=1:3,y=1:3)))
})


# maxDissim is logical ----------------------------------------------------

test_that("maxDissim is logical", {
  expect_error(vis2rfi(ffObj = ff, maxDissim = 1))
  expect_error(vis2rfi(ffObj = ff, maxDissim = NA))
  expect_error(vis2rfi(ffObj = ff, maxDissim = "x"))
})


# seed is integer or numeric ---------------------------------------------------------

test_that("seed is integer or numeric", {
  expect_error(vis2rfi(ffObj = ff, seed = NA), "Seed is non-numeric!")
  expect_error(vis2rfi(ffObj = ff, seed = "x"), "Seed is non-numeric!")
})


# Random subsample percentages have an appropriate value -------------------------------

 test_that("rsf or rsf_sub have appropriate values", {
   expect_error(vis2rfi(ffObj = ff, rsf = 0.5))
   expect_warning(vis2rfi(ffObj = ff, rsf = 0.45))
   expect_error(vis2rfi(ffObj = ff, rsf = 0))
   expect_error(vis2rfi(ffObj = ff, rsf = -1))
   expect_error(vis2rfi(ffObj = ff, rsf = NA))
   expect_error(vis2rfi(ffObj = ff, rsf = "x"))
   expect_error(vis2rfi(ffObj = ff, rsf_sub = "x"))
   expect_error(vis2rfi(ffObj = ff, rsf_sub = NA))
   expect_error(vis2rfi(ffObj = ff, rsf_sub = 0))
   expect_error(vis2rfi(ffObj = ff, rsf_sub = -1))
 })


# scaling is logical and non-missing ------------------------------------------------------

test_that("scaleXY has logical value", {
  expect_error(vis2rfi(ffObj = ff, scaleXY = 1), "scaleXY should be either TRUE or FALSE.")
  expect_error(vis2rfi(ffObj = ff, scaleXY = NA), "scaleXY should be either TRUE or FALSE.")
  expect_error(vis2rfi(ffObj = ff, scaleXY = "x"), "scaleXY should be either TRUE or FALSE.")
})


# feature contribution combining function checks ------------------------------------

test_that("feature contribution combinining is a function and with appropriate name", {
  expect_error(vis2rfi(ffObj = ff, outer = 1))
  expect_error(vis2rfi(ffObj = ff, outer = NA))
  expect_error(vis2rfi(ffObj = ff, outer = "x"))
  expect_error(vis2rfi(ffObj = ff, outer = list(FUN = 1)))
  expect_error(vis2rfi(ffObj = ff, outer = list(FUN = NA)))
  expect_error(vis2rfi(ffObj = ff, outer = list(FUN = "x")))
  expect_error(vis2rfi(ffObj = ff, outer = list(fun = 1)), "The feature contribution combining function does not have the name FUN. Name this function as FUN to proceed.")
})


test_that("feature contribution combining is a function with the correct argument length", {
  expect_error(vis2rfi(ffObj = ff, outer = list(FUN = function(x) x+1)), "FUN in outer should have two arguments corresponding to the function application on two variables i.e. seeking 2 way interaction effects.")
  expect_error(vis2rfi(ffObj = ff, outer = list(FUN = function(x,y,z) x+y+z)), "FUN in outer should have two arguments corresponding to the function application on two variables i.e. seeking 2 way interaction effects.")
  expect_error(vis2rfi(ffObj = ff, outer = list(FUN = function(...) x+y)), "FUN in outer should have two arguments corresponding to the function application on two variables i.e. seeking 2 way interaction effects.")
})

# smoother is logical and non-missing ------------------------------------------------------

test_that("smoother has logical value", {
  expect_error(vis2rfi(ffObj = ff, smoother = 1), "smoother should be either TRUE or FALSE.")
  expect_error(vis2rfi(ffObj = ff, smoother = NA), "smoother should be either TRUE or FALSE.")
  expect_error(vis2rfi(ffObj = ff, smoother = "x"), "smoother should be either TRUE or FALSE.")
})

# smoother df is a vector of length 2 ------------------------------------------------------

test_that("smoother df is a vector of length 2", {
  expect_error(vis2rfi(ffObj = ff, df = c()), "df should be a length two vector corresponding to smoothing over two variables i.e. seeking 2 way interaction effects.")
  expect_error(vis2rfi(ffObj = ff, df = 1), "df should be a length two vector corresponding to smoothing over two variables i.e. seeking 2 way interaction effects.")
  expect_error(vis2rfi(ffObj = ff, df = c(1,3,5)), "df should be a length two vector corresponding to smoothing over two variables i.e. seeking 2 way interaction effects.")
})

# length_out is numeric or NULL ---------------------------------------------------

test_that("length_out is numeric or NULL", {
  expect_error(vis2rfi(ffObj = ff, length_out = NA), "length_out should either be numeric or the NULL default value.")
  expect_error(vis2rfi(ffObj = ff, length_out = "x"), "length_out should either be numeric or the NULL default value.")
})


# alpha is numeric --------------------------------------------------------

test_that("alpha is numeric", {
  expect_error(vis2rfi(ffObj = ff, alpha = NA))
  expect_error(vis2rfi(ffObj = ff, alpha = "x"))
})


# rest args are character type ------------------------------------------------

test_that("rest args are of character type", {
  expect_error(vis2rfi(ffObj = ff, cfcLabel = NA))
  expect_error(vis2rfi(ffObj = ff, cfcLabel = 1))
  expect_error(vis2rfi(ffObj = ff, colors = NA))
  expect_error(vis2rfi(ffObj = ff, colors = 1))
  expect_error(vis2rfi(ffObj = ff, highlightcolor = NA))
  expect_error(vis2rfi(ffObj = ff, highlightcolor = 1))
})


# output vis2rfi object class is correct ----------------------------------

test_that("vis2rfi returned object is of correct class", {
  expect_equal(class(vis2rfi(ffObj = ff)), c("plotly", "htmlwidget"))
})
