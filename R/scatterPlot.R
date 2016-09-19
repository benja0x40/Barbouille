# =============================================================================.
#' Scatter plot with color mapping and group representation
# -----------------------------------------------------------------------------.
#' @author Benjamin Leblanc
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{defineColors},
#'   \link{makeColors},
#'   \link{colorLegend},
#'   \link{defineGroups},
#'   \link{groupLegend}
# -----------------------------------------------------------------------------.
#' @param x
#' horizontal axis coordinate.
#'
#' @param y
#' vertical axis coordinate.
#'
#' @param clr
#' vector of numeric values passed to \link{makeColors} for color mapping
#' (default = none).
#'
#' @param clr.prm
#' list of color mapping parameters defined by \link{defineColors}.
#'
#' @param grp
#' group memberships for group highlighting (default = none).
#' See \link{groupIndex} for documentation on how to specify group memeberships.
#'
#' @param grp.prm
#' list of group representation parameters defined by \link{defineGroups}.
#'
#' @param xlab
#' label for the horizontal axis (default = description of x).
#'
#' @param ylab
#' label for the vertical axis (default = description of x).
#'
#' @param ...
#' optional parameters passed to the \link{plot.default} function.
# -----------------------------------------------------------------------------.
#' @return NULL
# -----------------------------------------------------------------------------.
#' @examples
#' # Angular groups with two normally distributed random variables
#'
#' layout(matrix(1:4, 2, 2, byrow = T))
#'
#' x <- rnorm(2000)
#' y <- rnorm(2000)
#' z <- sqrt(x^2 + y^2)
#' a <- atan2(y, x)
#'
#' grp <- 60 * (180/pi * a) %/% 60
#'
#' grp.ids <- sort(unique(grp))
#' grp.lbl <- paste(grp.ids, "° < a < ", grp.ids + 60, "°", sep="")
#'
#' grp.prm <- defineGroups(
#'   grp.ids, labels = grp.lbl, color = grey(c(0.2, 0.6)), pch = 1:6, cex = 0.7
#' )
#'
#' scatterPlot(x, y, grp = grp, grp.prm = grp.prm, xlab = "x", ylab = "y")
#' groupLegend("tr", grp.prm, xjust = 1, title = "angle from atan2")
#'
#' scatterPlot(a, z, grp = grp, grp.prm = grp.prm, xlab = "a (rad)", ylab = "z")
#' groupLegend("tr", grp.prm, xjust = 1, title = "angle from atan2")
#'
#' scatterPlot(a, x, grp = grp, grp.prm = grp.prm, xlab = "a (rad)", ylab = "x")
#' groupLegend("tr", grp.prm, xjust = 1, title = "angle from atan2")
#'
#' scatterPlot(a, y, grp = grp, grp.prm = grp.prm, xlab = "a (rad)", ylab = "y")
#' groupLegend("br", grp.prm, xjust = 1, title = "angle from atan2")
# -----------------------------------------------------------------------------.
scatterPlot <- function(
  x, y = NULL,
  clr = NULL, clr.prm = NULL, grp = NULL, grp.prm = NULL,
  density = c(NULL, "knn", "hist2D"),
  xlab = "", ylab = "", ...
) {

  args <- c(list(x = x, y = y, xlab = xlab, ylab = ylab), list(...))

  n <- length(xy.coords(x = x, y = y)$x)

  if(! "col" %in% names(args)) {
    clr <- makeColors(
      clr, parameters = clr.prm, grp = grp, grp.prm = grp.prm
    )
    args$col <- clr
  }
  if(! (is.null(grp) | is.null(grp.prm))) {
    grp.prm <- updateDefinition(grp.prm)
    g.i <- groupIndex(grp, grp.prm)
    lst <- colnames(grp.prm)
    lst <- lst[lst %in% c("pch", "cex", "lty", "lwd")]
    if(length(lst) > 0) {
      for(x in lst) {
        if(x %in% names(args)) {
          args[[x]] <- rep(args[[x]], n) # Replicate provided value
        } else {
          args[[x]] <- rep(par(x), n)    # Replicate default value
        }
        # Override default or provided with group value
        g.p <- grp.prm[[x]][g.i]
        idx <- which(! (is.na(g.p) | g.p == ""))
        args[[x]][idx] <- g.p[idx]
      }
    }
  }
  do.call(plot.default, args)
}
# =============================================================================.
#' Scatterplot with points colored according to kNN density estimation
# -----------------------------------------------------------------------------.
#' @param \code{X} matrix with two columns of numeric values
#' @param \code{col} index of color gradient (0 = grey, 1 = red, 2 = green, 3 = blue)
#' @param \code{grp} logical vector defining an highlighted subset of points
#' @param \code{grp.col} index of color gradient used for the grp subset
#' @param \code{zline} logical, activates the drawing of line y = 0 when TRUE
#' @param \code{xyline} logical, activates the drawing of line y = x when TRUE
#' @param \code{k} number of nearest neighbors for kNN density estimation
#' @param \code{...} optional parameters passed to the plot function
#'
#' @return produce a scatter plot in the active graphic device
# -----------------------------------------------------------------------------.
scPlotCounts <- function(X, col=0, grp=NULL, grp.col=0, zline=F, xyline=F, k=256, ...) {

  o <- 0:49/49
  s <- 0.5
  point.colors <- list(
    grey(99:0/132),
    c(
      rgb(o, 0, 0, s),
      rgb(1, o, 0, s)
    ),
    c(
      rgb(0, o, 0, s),
      rgb(o, 1, 0, s)
    ),
    c(
      rgb(0, 0, o, s),
      rgb(0, o, 1, s)
    ),
    rep(rgb(0,0.5,1,0.5), 100)
  )

  Y <- X[finiteValues(X),]
  dens <- densityByKNN(X, k=k)
  clrs <- point.colors[[col+1]][ceiling(dens$prc)]
  plot(X, col=clrs, ...)
  if(! is.null(grp)) {
    dens <- densityByKNN(X[grp,], k=k/2, sbj = Y)
    clrs <- point.colors[[grp.col+1]][ceiling(dens$prc)]
    points(X[grp,], col=clrs, pch=20, cex=0.3)
  }
  if(zline) {
    abline(h=0, col=rgb(0,0,0,0.5), lwd=1)
  }
  if(xyline) {
    abline(a=0, b=1, col=rgb(0,0,0,0.5), lwd=1)
  }
}
