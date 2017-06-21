# =============================================================================.
#' Scatter plot with color mapping and group representation
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
#' @param alpha
#' transparency (numeric value(s) between \code{0} and \code{1}).
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
#' @param add
#' add to existing plot (logical, default = F).
#'
#' @param ...
#' optional parameters passed to the \link{plot.default} function.
#'
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
#' ScatterPlot(x, y, grp = grp, grp.prm = grp.prm, xlab = "x", ylab = "y")
#' groupLegend("tr", grp.prm, xjust = 1, title = "angle from atan2")
#'
#' ScatterPlot(a, z, grp = grp, grp.prm = grp.prm, xlab = "a (rad)", ylab = "z")
#' groupLegend("tr", grp.prm, xjust = 1, title = "angle from atan2")
#'
#' ScatterPlot(a, x, grp = grp, grp.prm = grp.prm, xlab = "a (rad)", ylab = "x")
#' groupLegend("tr", grp.prm, xjust = 1, title = "angle from atan2")
#'
#' ScatterPlot(a, y, grp = grp, grp.prm = grp.prm, xlab = "a (rad)", ylab = "y")
#' groupLegend("br", grp.prm, xjust = 1, title = "angle from atan2")
# -----------------------------------------------------------------------------.
#' @export
ScatterPlot <- function(
  x, y = NULL,
  clr = NULL, clr.prm = NULL, alpha = 1.0,
  grp = NULL, grp.prm = NULL,
  xlab = "", ylab = "",
  add = F,
  ...
) {

  args <- c(list(x = x, y = y, xlab = xlab, ylab = ylab), list(...))

  n <- length(xy.coords(x = x, y = y)$x)

  # Default group parameters
  if(is.null(grp.prm) & is.numeric(grp)) {
    grp.prm <- table(grp)
    grp.prm <- defineGroups(
      ids = as.numeric(names(grp.prm)), colors = SuperRainbow(length(grp.prm))
    )
  }
  # Color parameters
  if(! "col" %in% names(args)) {
    if(! (is.null(clr) & is.null(grp.prm))) {
      if(is.null(clr.prm) & is.null(grp.prm)) {
        clr.prm <- AutoColorParameters()
      }
      clr <- makeColors(
        clr, parameters = clr.prm, grp = grp, grp.prm = grp.prm
      )
    }
    if(is.null(clr)) clr <- grey(0)
    args$col <- clr
  }
  args$col <- ReplaceAlpha(args$col, alpha)

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

  if(add) {
    args[c("xlab", "ylab", "xlim", "ylim", "add")] <- NULL
    do.call(points, args)
  } else {
    do.call(plot.default, args)
  }
}
