# NOT EXPORTED #################################################################

# =============================================================================.
#' PlotContour
# -----------------------------------------------------------------------------.
#'
#' @param xy
#'
#' @param k
#'
#' @param poly
#'
#' @param ...
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @keywords internal
PlotContour <- function(xy, k = 5, poly = F, ...) {

  m <- colMeans(xy)
  p <- t(t(xy) - m)
  a   <- atan2(p[,2], p[,1])
  d   <- sqrt(rowSums(p^2))

  o <- order(a)
  a <- a[o]
  d <- caTools::runmean(d[o], k)
  p <- d * cbind(cos(a), sin(a))
  p <- t(t(p) + m)

  if(poly) {
    polygon(p, ...)
  } else {
    lines(rbind(p, p[1, ]), ...)
  }
}
