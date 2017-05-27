# =============================================================================.
#' Legend for color maps
# -----------------------------------------------------------------------------.
#' @export colorLegend
#' @seealso
#'   \link{makeColors},
#'   \link{defineColors}
# -----------------------------------------------------------------------------.
#' @description
#' colorLegend draws a color legend according to given color mapping parameters.
# -----------------------------------------------------------------------------.
#' @param pos
#' the legend location which can be specified using either a single keyword in
#' \code{"bottomright", "bottom", "bottomleft", "left", "center", "right",
#' "topleft", "top", "topright"}, or the corresponding abbreviation
#' (\code{"br", "b", "bl", "l", "c", "r", "tl", "t", "tr"}),
#' or the corresponding index
#' (from \code{1} for \code{"bottomright"} to \code{9} for \code{"topleft"}).
#'
#' @param parameters
#' list of color mapping parameters defined with \link{defineColors}.
#'
#' @param ticks
#' values of the tick marks for graduation of the color scale
#' (default = \code{thresholds} as defined in \code{parameters}).
#'
#' @param labels
#' text labels shown next to the tick marks
#' (default = \code{ticks}).
#'
#' @param resolution
#' the resolution of the color scale or number of elementary units composing the
#' represented color gradients (default = \code{100}).
#'
#' @param log
#' logical. If \code{TRUE}, the color legend is represented in log scale
#' (default = \code{F}).
#'
#' @param size
#' the size of the color legend given as a list or a vector in the form
#' \code{c(}axial length, width\code{)} which are expressed in percentage of
#' the dimensions of the plot area (default = \code{c(40, 3)}).
#'
#' @param margin
#' numeric vector controlling the spacing between the legend and the borders
#' of the plot area and expressed in percentage of its dimensions
#' (default = \code{c(5, 5, 5, 5)} for left, right, bottom, top).
#'
#' @param horiz
#' logical controlling the legend layout.
#' If \code{TRUE} the color legend spans horizontally from the given location,
#' and vertically otherwise (default = \code{F}, vertical).
#'
#' @param tick.pos
#' relative location of graduations, which is indicated by a value of \code{1}
#' or \code{-1} for a location to the right/above or to the left/below
#' the color scale respectively.
#'
#' @param tick.size
#' length of the tick marks, expressed in percentage of the dimensions of the
#' plot area (default = \code{1.5}).
#'
#' @param offset
#' spacing between tick marks and tick labels.
#'
#' @param lwd
#' line width for tick marks and color scale borders.
#'
#' @param border
#' logical activating the drawing of black borders around the color scale
#' (default = \code{T}).
#'
#' @param ...
#' optional parameters passed to the \link{text} function.
# -----------------------------------------------------------------------------.
#' @return NULL
# -----------------------------------------------------------------------------.
#' @examples
#' # Radial color gradient with two normally distributed random variables
#' x <- rnorm(2000)
#' y <- rnorm(2000)
#
#' z <- 10^sqrt(x^2 + y^2)
#'
#' col.par <- defineColors(name = "bantignies")
#' clrs    <- makeColors(z, parameters = col.par)
#'
#' plot(x, y, xlim = c(-4.5, 4.5), ylim = c(-5, 4), pch = 20, col = clrs)
#' colorLegend(
#'   "bottom", parameters = col.par, log = T, size = c(70, 3), horiz = T
#' )
# -----------------------------------------------------------------------------.
colorLegend <- function(
  pos, parameters, ticks = NULL, labels = NULL, resolution = 100, log = F,
  size = c(40, 3), margin = 5, horiz = F,
  tick.pos = 1, tick.size = 1.5, offset = 0.3, lwd = 1, border = T, ...
  ) {

  if(! is(parameters, "colorParameters")) stop("Class of parameters is invalid")

  if(length(margin) == 1) margin <- rep(margin, 2)
  if(length(margin) == 2) margin <- margin[c(1, 1, 2, 2)]
  if(length(margin) != 4) stop("Incorrect margin")

  # Capture graphic parameters of the current plot area
  gp <- par(c("usr", "xlog", "ylog"))

  # Reset the coordinates system of the current plot area
  par(xlog = F, ylog = F, usr = (1 + margin/100) * c(-1, 1, -1, 1))

  # Extend range to show below and above colors
  if(log) parameters$range <- with(parameters, exp(log(range) * extra))
  else    parameters$range <- with(parameters, range * extra)

  # Use the predefined range with uniform tick marks
  if(is.null(labels) & is.integer(ticks) & length(ticks) == 1) {
    ticks <- with(parameters, {
      x <- (0 : (ticks - 1)) / (ticks - 1)
      x * diff(range) + range[1]
    })
  }
  # Use thresholds for tick marks if none specified
  if(is.null(ticks)) ticks <- parameters$thresholds

  # Use tick values for tick labels if none specified
  if(is.null(labels)) labels <- as.character(ticks)

  if(length(labels) != length(ticks)) {
    stop("The ticks and labels have different numbers of elements")
  }
  if(! tick.pos %in% c(-1, 1)) stop("Unknown tick.pos value")

  # Resolve legend position
  p <- .resolve.legend.position.(pos)

  r <- parameters$range
  if(log) {
    ticks <- log10(ticks)
    r     <- log10(r)
  }
  n <- resolution
  i <- (0 : n) / n
  t <- (ticks - r[1]) / diff(r)
  r <- i * diff(r) + r[1]
  if(log) {
    ticks <- 10^ticks
    r     <- 10^r
  }
  clrs <- makeColors(r, parameters = parameters)

  size <- unlist(size)/100
  l <- size[1]
  w <- size[2]
  s <- tick.size/100

  clc <- function(l.pos, w.pos) {
    li <- l.pos - l + i * 2 * l
    list(
      # coordinates for color intervals
      li1 = li[1:n], li2 = li[(1:n)+1],
      wi1 = w.pos - w, wi2 = w.pos + w,
      # coordinates for tick marks
      l.t = l.pos - l + t * 2 * l,
      wt1 = w.pos + tick.pos * w,
      wt2 = w.pos + tick.pos * (w + 2 * s)
    )
  }

  if(horiz) {
    fx <- strwidth(labels, ...)[ifelse(p$x < 0, 1, length(labels))]
    fy <- max(strheight(labels, ...))
    k <- p$y * (p$y == tick.pos)
    x <- p$x - p$x * (l + fx)
    y <- p$y - p$y * w - k * (2 * s + fy * (1 + 2 * offset))
    r <- clc(l.pos = x, w.pos = y)
    rect(r$li1, r$wi1, r$li2, r$wi2, col = clrs, border = rgb(0,0,0,0))
    if(border) {
      rect(x - l, r$wi1, x + l, r$wi2, lwd = lwd)
    } else {
      segments(x - l, r$wt1, x + l, r$wt1, lwd = lwd)
    }
    segments(r$l.t, r$wt1, r$l.t, r$wt2, lwd = lwd)
    text(
      r$l.t, r$wt2, labels = labels, pos = tick.pos + 2, offset = offset, ...
    )
  } else {
    fx <- max(strwidth(labels, ...))
    fy <- strheight(labels, ...)[ifelse(p$y < 0, 1, length(labels))]
    k <- p$x * (p$x == tick.pos)
    x <- p$x - p$x * w - k * (2 * s + fx * (1 + 2 * offset))
    y <- p$y - p$y * (l + fy)
    r <- clc(l.pos = y, w.pos = x)
    rect(r$wi1, r$li1, r$wi2, r$li2, col = clrs, border = rgb(0,0,0,0))
    if(border) {
      rect(r$wi1, y - l, r$wi2, y + l, lwd = lwd)
    } else {
      segments(r$wt1, y - l, r$wt1, y + l, lwd = lwd)
    }
    segments(r$wt1, r$l.t, r$wt2, r$l.t, lwd = lwd)
    text(
      r$wt2, r$l.t, labels = labels, pos = tick.pos + 3, offset = offset, ...
    )
  }

  # Restore graphics parameters of the current plot area
  with(gp, par(usr = usr, xlog = xlog, ylog = ylog))
}


