#' vis2rfi: Visualise 2Way Random Forest Interactions
#'
#' The main function that leverages packages forestFloor and plotly in order to
#' construct a 3d representation of the interaction effects of two variables.
#'
#' @param impInd Importance index. An integer vector of length 2 containing
#' the variable order of importance as given by the random forest algorithm.
#' Integer values should be less than 20 so that the interaction effect between
#' pairs of features in the top 20 set of random forest importance is shown.
#' The default is 1:2 or the first two most important features.
#' @param ffObj The object of class forestFloor_regression or forestFloor_multiClass
#' (where the latter has not been tested yet) obtained after running the
#' \code{\link[forestFloor]{forestFloor}} function on a random forest object. See the
#' corresponding documentation for details.
#' @param maxDissim Whether to subsample the data using a maximum dissimilarity
#' approach as per the \code{\link{maxDissimProgBar}} function. Logical. Default is
#' FALSE.
#' @param seed An integer value for setting the seed at the subsampling step. Default
#' is 875. Setting a different seed will snatch a different sample of cases from the
#' data, thus experimenting with this parameter can produce various visualizations of
#' from the same pool of data.
#' @param rsf Random sample frequency of the data. Numeric between 0 and 1. Since a
#' subsampling approach is used, this value is expected to be small (depending on
#' the data row size), and if chosen large then an error or a warning message is given.
#' Default is 25\% of the data row size.
#' @param rsf_sub Applicable if maxDissim = TRUE. The percentage of rsf which is set
#' aside for extracting maximally dissimilar cases. Numeric. Default
#' is 33\% of rsf.
#' @param scaleXY Should the variables in the interaction be standardized? Logical.
#' Default is TRUE.
#' @param outer Further arguments passed to outer function, mainly FUN, as default is "*"
#' or to multiply the effects. In addition, since effects of 2way interactions are seeked,
#' this should be a two variable function, else an error is thrown. See the examples section.
#' @param smoother Whether a smoother should be applied to the construction of the 3d 2way
#' interaction surface. Logical with default FALSE. Currently GAMs are used with the built-in
#' nonparametric smoothing terms that are indicated by s for smoothing splines. See \code{\link[gam]{gam}}.
#' Depending on the nature of the data, the smoother can produce 3d planes that more clearly
#' show if the 2 way interaction effect is present and how significant it is i.e. the more
#' vertical the plance the higher the interaction significance.
#' @param df The target equivalent degrees of freedom, used as a smoothing parameter. Similarly
#' as in \code{\link[gam]{s}}. A numeric vector of length two with default c(4,4).
#' @param length_out The number of equally spaced values within the subsampled range of each
#' of the two variables forming the interaction. Same as in \code{\link[base]{seq}}. If NULL
#' the default length is the row size of the subsample. For even faster but coarser 3d interaction
#' surfaces, this parameter can be set to a smaller value, as above, in the spirit of
#' visualisation experimentation.
#' @param cfcLabel A character label of the z-axis in the 3d plot, which corresponds to the
#' combined feature contribution function as given in the outer argument above. Default value
#' is "Combined FCs".
#' @param colors Same as in \code{\link[plotly]{plot_ly}}. Useful for color experimentation.
#' Defaut is "Blues". Consult this \href{https://github.com/EmilHvitfeldt/r-color-palettes}{comprehensive list of colors in R}.
#' A few notable options for this parameter, and taken from the above link, are the following:
#' \itemize{
#'   \item viridis::inferno(n=5)
#'   \item grDevices::heat.colors(n=5) or grDevices::rainbow(n=5)
#'   \item oompaBase::cyanyellow(5)
#'   \item palr::sstPal(5)
#'   \item pals::ocean.curl(5)
#'   \item scico::scico(10,palette = "berlin") or scico::scico(10,palette = "oleron")
#' }
#' @param alpha Same as in \code{\link[plotly]{plot_ly}}. Default is 1.
#' @param highlightcolor Same as in \code{\link[plotly]{plot_ly}}. Default is "#ff0000 ".
#' @param ... Further arguments passed to \code{\link{maxDissimProgBar}} function.
#'
#' @details See \code{vignette("vis2rfi")} for a tutorial/guide
#' on how the package can be used.
#'
#' @return A 3d plotly visualization showing the combined random forest feature
#' contribution of the two-way interaction. If the run is assigned, similarly an
#' object of class "plotly" and "htmlwidget".
#'
#' @export
#'
#' @example ./examples/vis2rfi_examples.R
#'
#' @seealso \code{\link{maxDissimProgBar}}, \code{\link[forestFloor]{forestFloor}}, \code{\link[plotly]{plot_ly}}, \code{\link[plotly]{add_surface}}
vis2rfi <- function(impInd = c(1,2), ffObj = NULL, maxDissim = FALSE, seed = 875, rsf = 0.25, rsf_sub = 1/3, scaleXY = TRUE, outer = list(FUN = "*"), smoother = FALSE, df = c(4,4), length_out = NULL, cfcLabel = "Combined FCs", colors = "Blues", alpha = 1, highlightcolor = "#ff0000",...){

  if (is.null(ffObj)) stop(paste(crayon::red(clisymbols::symbol$bullet), "The forestFloor object is missing. Please run forestFloor::forestFloor to obtain the feature contributions and then pass the resulting object in the ffObj argument."))
  if (!(class(ffObj) %in% c("forestFloor_regression","forestFloor_multiClass"))) stop(paste(crayon::red(clisymbols::symbol$bullet), "ffObj is not of class forestFloor_regression or forestFloor_multiClass"))
  if (length(impInd) != 2) stop(paste(crayon::red(clisymbols::symbol$bullet), "impInd should have length 2 since the interaction between two variables is plotted."))
  if (any(impInd <= 0)) stop(paste(crayon::red(clisymbols::symbol$bullet), "Non-positive index value! Indices of random forest important variables should be positive. Select values in the impInd vector that are greater than 0."))
  if (any(impInd > 20)) stop(paste(crayon::red(clisymbols::symbol$bullet), "The focus is to visualise interactions in the top 20 set of random forest importance. Select values in the impInd vector that are less than 20."))
  if (!is.logical(maxDissim)) stop(paste(crayon::red(clisymbols::symbol$bullet), "maxDissim should either be TRUE or FALSE"))
  if (!is.numeric(seed)) stop(paste(crayon::red(clisymbols::symbol$bullet), "Seed is non-numeric!"))
  if (!is.numeric(rsf) | !is.numeric(rsf_sub)) stop(paste(crayon::red(clisymbols::symbol$bullet), "Either rsf or rsf_sub is non-numeric!"))
  if ((rsf <= 0) | (rsf_sub <= 0)) stop(paste(crayon::red(clisymbols::symbol$bullet), "Either rsf or rsf_sub is non-positive!"))
  if (rsf >= 0.35 & rsf < 0.5) warning(paste(crayon::red(clisymbols::symbol$bullet), "With a data random sample of", rsf, ", result might be difficult to handle or cause a crash depending on the data row size. Consider lowering this threshold."))
  if (rsf >= 0.5) stop(paste(crayon::red(clisymbols::symbol$bullet), "With such a high data random sample of", rsf, ", result might be too difficult to handle or cause a crash depending on the data row size. Consider lowering this threshold. This function does not run for such a high subsample percentage as this would defeat the package's purpose."))
  if ((maxDissim == TRUE & rsf_sub >= 0.99) | (maxDissim == TRUE & rsf_sub <= 0.01)) stop(paste(crayon::red(clisymbols::symbol$bullet), "rsf_sub should have reasonable values between 0 and 1 i.e. the portion of rsf in % that will be filled with maximally dissimilar cases."))
  if (!is.logical(scaleXY) | is.na(scaleXY)) stop(paste(crayon::red(clisymbols::symbol$bullet), "scaleXY should be either TRUE or FALSE."))
  if (!is.list(outer)) stop(paste(crayon::red(clisymbols::symbol$bullet), "outer is not a list."))
  if (names(outer) != "FUN") stop(paste(crayon::red(clisymbols::symbol$bullet), "The feature contribution combining function does not have the name FUN. Name this function as FUN to proceed."))
  FUN <- match.fun(outer$FUN)
  if (!is.function(FUN)) stop(paste(crayon::red(clisymbols::symbol$bullet), "outer argument does not contain a function."))
  if (length(as.list(args(FUN))) != 3) stop(paste(crayon::red(clisymbols::symbol$bullet), "FUN in outer should have two arguments corresponding to the function application on two variables i.e. seeking 2 way interaction effects."))
  if (!is.logical(smoother) | is.na(smoother)) stop(paste(crayon::red(clisymbols::symbol$bullet), "smoother should be either TRUE or FALSE."))
  if (length(df) != 2) stop(paste(crayon::red(clisymbols::symbol$bullet), "df should be a length two vector corresponding to smoothing over two variables i.e. seeking 2 way interaction effects."))
  if (is.character(length_out) | is.logical(length_out)) stop(paste(crayon::red(clisymbols::symbol$bullet), "length_out should either be numeric or the NULL default value."))
  if (any(!is.character(cfcLabel), !is.character(colors), !is.character(highlightcolor))) stop(paste(crayon::red(clisymbols::symbol$bullet), "At least one of cfcLabel, colors or highlightcolor arguments is not a character."))
  if (!is.numeric(alpha)) stop(paste(crayon::red(clisymbols::symbol$bullet), "alpha is non-numeric."))

  fcInt <- tibble::tibble(x = ffObj$X[, ffObj$imp_ind[impInd[1]]], y = ffObj$X[, ffObj$imp_ind[impInd[2]]], fcx = ffObj$FCmatrix[, ffObj$imp_ind[impInd[1]]], fcy = ffObj$FCmatrix[, ffObj$imp_ind[impInd[2]]])

  if (scaleXY) {
    cat(crayon::green(clisymbols::symbol$tick), "Scaling the variables", sep = "\n")
    fcInt <- fcInt %>% dplyr::mutate_at(1:2, ~ as.vector(scale(.)))
  }

  rsf_size <- round(rsf*nrow(fcInt))

  if(maxDissim) {
    rsf_size_maxDissim <- round(nrow(fcInt) * round(rsf - rsf*rsf_sub, digits = 2))
    set.seed(seed)
    idx <- sample(1:nrow(fcInt), rsf_size_maxDissim)
    cat(paste(crayon::green(clisymbols::symbol$tick), "Taking the random subsample."), paste("An rsf value of", rsf, "corresponds to", rsf_size, "cases, out of the", nrow(fcInt), "ones."), sep = "\n")
    cat(paste(crayon::green(clisymbols::symbol$tick), paste0("Taking the random base ", round(1-rsf_sub, digits = 2), " & pool ", round(rsf_sub, digits = 2), " subsample percentage of rsf = ", rsf, " for maxDissim.")), sep = "\n")
    fcIntSampleBase <- fcInt[idx,]
    cat("Base subsample size is :",paste0(nrow(fcIntSampleBase), " cases"), sep = "\n")
    fcIntSamplePool <- fcInt[-idx,]
    cat("Pool subsample size is :",paste0(nrow(fcIntSamplePool), " cases"), sep = "\n")
    cat(paste(crayon::green(clisymbols::symbol$tick), "Computing the maxDissim object."), sep = "\n")

    number_of_additions <- round(nrow(fcInt)*round(rsf*rsf_sub, digits = 2))

    cat(paste(crayon::green(clisymbols::symbol$tick), paste0("Adding to base subsample ", number_of_additions, " cases from the pool subsample, for a total of ", rsf_size, " cases.")), sep = "\n")
    fcIntSamplePoolIdx <- maxDissimProgBar(fcIntSampleBase, fcIntSamplePool, n = number_of_additions, ...)
    fcIntSample <- rbind(fcIntSampleBase, fcIntSamplePool[fcIntSamplePoolIdx,])

  } else {
    set.seed(seed)
    idx <- sample(1:nrow(fcInt), rsf_size)
    cat(paste(crayon::green(clisymbols::symbol$tick), "Taking the random subsample.", paste0("An rsf value of ",rsf," corresponds to ",rsf_size," cases, out of the ", nrow(fcInt), " ones.")), sep = "\n")
    fcIntSample <- fcInt[idx,]
  }

  if (smoother) {
    fcIntSample <- fcIntSample %>% mutate(combinedFC = FUN(fcx,fcy))
    cat(paste(crayon::green(clisymbols::symbol$tick), paste0("Fitting the GAM smoothing spline with ", df[1], " and ", df[2], " degrees of freedom respectively on each of the two interaction variables.")), sep = "\n")
    gam_formula <- as.formula(paste0("combinedFC ~ s(x, df = ", df[1], ") + s(y, df = ", df[2], ")"))
    gamFit <- gam::gam(gam_formula, data=fcIntSample)
    rangeX <- range(fcIntSample$x)
    rangeY <- range(fcIntSample$y)
    seq_length_out <- ifelse(is.null(length_out), nrow(fcIntSample), as.integer(length_out))
    fcIntSample <- data.frame(x=seq(from = rangeX[1], to = rangeX[2], length.out = seq_length_out), y=seq(from = rangeY[1], to = rangeY[2], length.out = seq_length_out))
    newXYgrid <- expand.grid(fcIntSample)
    fcIntZ <- gam::predict.Gam(gamFit, newdata = newXYgrid)
  } else {
    fcIntZ <- outer(fcIntSample$fcx, fcIntSample$fcy, FUN)
  }

  cat(crayon::green(clisymbols::symbol$tick), "Computing the 3d visualization.")
  plotly::plot_ly(x = fcIntSample$x, y = fcIntSample$y, z = fcIntZ, colors = colors, alpha = alpha) %>% plotly::add_surface(contours = list(
    z = list(
      show = TRUE,
      usecolormap = TRUE,
      highlightcolor = highlightcolor,
      project = list(z = TRUE)
    )
  )) %>% plotly::layout(
    title = "Interaction - Combined Marginal RF Contributions",
    scene = list(
      xaxis = list(title = colnames(ff$X[, ff$imp_ind[impInd[1]], drop = FALSE]), automargin = TRUE),
      yaxis = list(title = colnames(ff$X[, ff$imp_ind[impInd[2]], drop = FALSE]), automargin = TRUE),
      zaxis = list(title = cfcLabel, automargin = TRUE)
    ))
}
