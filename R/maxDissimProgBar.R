#' maxDissimProgBar: Maximum Dissimilarity Sampling With A Progress Bar
#'
#' This is the same function as in \code{\link[caret]{maxDissim}} but with the verbose argument replaced by a progress bar.
#'
#' See \code{vignette("vis2rfi")} for a tutorial/guide
#' on how the package can be used.
#'
#' @inheritParams caret::maxDissim
#'
#' @export
#'
#' @seealso \code{\link{vis2rfi}}, \code{\link[caret]{maxDissim}}
maxDissimProgBar <- function (a, b, n = 2, obj = minDiss, useNames = FALSE, randomFrac = 1, ...)
{
  loadNamespace("proxy")
  if (nrow(b) < 2)
    stop("there must be at least 2 samples in b")
  if (ncol(a) != ncol(b))
    stop("a and b must have the same number of columns")
  if (nrow(b) < n)
    stop("n must be less than nrow(b)")
  if (randomFrac > 1 | randomFrac <= 0)
    stop("randomFrac must be in (0, 1]")
  if (useNames) {
    if (is.null(rownames(b))) {
      warning("Cannot use rownames; swithcing to indices")
      free <- 1:nrow(b)
    }
    else free <- rownames(b)
  }
  else free <- 1:nrow(b)
  inSubset <- NULL
  newA <- a

  pb <- progress::progress_bar$new(total = n, format = "  adding cases [:bar] :total :percent elapsed: :elapsedfull eta: :eta", clear = FALSE, width= 80, complete = "=")
  pb$tick(0)
  for (i in 1:n) {
    pb$tick()
    pool <- if (randomFrac == 1)
      free
    else sample(free, max(2, floor(randomFrac * length(free))))

    diss <- proxy::dist(newA, b[pool, , drop = FALSE], ...)
    bNames <- colnames(b)[pool]
    tmp <- pool[which.max(apply(diss, 2, obj))]

    inSubset <- c(inSubset, tmp)
    newA <- rbind(newA, b[tmp, , drop = FALSE])
    free <- free[!(free %in% inSubset)]
  }
  inSubset
}
