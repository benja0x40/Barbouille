# VISIBLE ######################################################################

# =============================================================================.
#' Quick and dirty color mapping
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{autoscale},
#'   \link{AutoColorParameters},
#'   \link{ColorLegend}
# -----------------------------------------------------------------------------.
#' @inheritParams autoscale
#'
#' @param clr.prm
#' a ColorParameter object defined by \link{DefineColorMap}.
#'
#' @inheritParams AutoColorParameters
#'
#' @return
#' \code{colorize} returns a vector of RGBA colors.
# -----------------------------------------------------------------------------.
#' @export
colorize <- function(x, mode = NULL, clr.prm = NULL, ...) {

  if(is.null(clr.prm)) clr.prm <- AutoColorParameters(...)
  clr <- MakeColors(autoscale(x, mode = mode), parameters = clr.prm)

  clr
}

# =============================================================================.
#' As the name suggests...
# -----------------------------------------------------------------------------.
#' @param axes
#' logical, show axes (default = T, yes)
#'
#' @param xlab
#' character, name of the horizontal axis (default = none).
#'
#' @param ylab
#' character, name of the vertical axis (default = none).
#'
#' @param ...
#' optional arguments forwarded to the \link{plot.default} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
EmptyPlot <- function(axes = T, xlab = '', ylab = '', ...) {
  plot(0, type = 'n', axes = axes, xlab = xlab, ylab = ylab, ...)
}

# =============================================================================.
#' Plot a matrix of colors
# -----------------------------------------------------------------------------.
#' @param m
#' matrix of color values.
#'
#' @param x
#' coordinates of the x axis bins.
#'
#' @param y
#' coordinates of the y axis bins.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
PlotImage <- function(m, x = NULL, y = NULL, ...) {
  image(
    x = x,
    y = y,
    z = matrix(1:length(m), nrow(m), ncol(m)),
    col = m, ...
  )
}

# HIDDEN #######################################################################

# =============================================================================.
#' Make plot limits including space for legends
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector or matrix with two columns.
#'
#' @param y
#' numeric vector (default = NULL).
#'
#' @param symetric
#' logical (default = F, no)
#'
#' @param spacing
#' numeric.
#'
#' @param margin
#' numeric.
#'
#' @return
#' \code{xylim} returns a list.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
xylim <- function(x, y = NULL, symetric = F, spacing = 0, margin = 0) {

  if(is.null(y)) {
    y <- x[, 2]
    x <- x[, 1]
  }

  xlim <- range(x[FiniteValues(x)])
  ylim <- range(y[FiniteValues(y)])
  if(symetric) {
    xlim <- ylim <- range(xlim, ylim)
  }

  spacing <- rep(spacing, length.out = 2)
  margin  <- rep(margin,  length.out = 2)

  sgn <- sign(spacing)
  spacing <- abs(spacing)

  xlim <- xlim * (1 + spacing[1]) + sgn[1] * diff(xlim) * spacing[1] / 2
  ylim <- ylim * (1 + spacing[2]) + sgn[2] * diff(ylim) * spacing[2] / 2

  xlim <- xlim * (1 + margin[1])
  ylim <- ylim * (1 + margin[2])

  list(x = xlim, y = ylim)
}

# =============================================================================.
#' Rescale x non-linearly into ]0, 1[
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{S01},
#'   \link{autoscale}
# -----------------------------------------------------------------------------.
#' @description
#' rescale values non-linearly to the unit interval using ranks
#'
#' @param x
#' numeric vector
#'
#' @return
#' \code{rankstat} returns \eqn{q = (rank(x) - 0.5) / N} where N = length(x)
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
rankstat <- function(x) { (rank(x) - 0.5) / length(x) }

# =============================================================================.
#' Rescale x linearly into [0, 1]
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{rankstat},
#'   \link{autoscale}
# -----------------------------------------------------------------------------.
#' @description
#' rescale values linearly to the unit interval.
#'
#' @param x
#' numeric vector.
#'
#' @return
#' \code{S01} returns x linearly rescaled such that range(x) = [0, 1]
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
S01 <- function(x) { (x - min(x)) / diff(range(x)) }

# =============================================================================.
#' As the name suggests...
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{colorize},
#'   \link{S01},
#'   \link{rankstat}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector.
#'
#' @param mode
#' either \code{"rank"} or \code{"01"} (default).
#'
#' @return
#' \code{autoscale} returns a numeric vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
autoscale <- function(x, mode = NULL) {

  if(is.null(mode)) mode <- "01"

  if(mode == "rank") {
    x <- rankstat(x)
  }

  if(mode == "01") {
    x <- S01(x)
    # => prevent use of the "above" color
    x <- x * (1 - .Machine$double.neg.eps)
  }

  x
}

# =============================================================================.
#' Quick and dirty color mapping parameters
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{colorize},
#'   \link{DefineColorMap}
# -----------------------------------------------------------------------------.
#' @param colors
#' vector of colors (optional).
#'
#' @return
#' \code{AutoColorParameters} returns a ColorParameters object.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
AutoColorParameters <- function(colors = NULL) {

  if(is.null(colors)) colors <- grey(c(0.8, 0.7, 0.5, 0))

  chk <- length(colors) == 1
  if(chk & colors[1] == "WB") colors <- grey(1:0)
  if(chk & colors[1] == "BW") colors <- grey(0:1)

  if(chk & colors[1] == "Br") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(1, 0, 0))
  if(chk & colors[1] == "Bg") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(0, 1, 0))
  if(chk & colors[1] == "Bc") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(0, 1, 1))
  if(chk & colors[1] == "By") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(1, 1, 0))
  if(chk & colors[1] == "Bp") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(1, 0, 1))

  if(chk & colors[1] == "rW") colors <- c(grey(c(0.8, 0.4)), rgb(1:1, 0:1, 0:1))
  if(chk & colors[1] == "gW") colors <- c(grey(c(0.8, 0.4)), rgb(0:1, 1:1, 0:1))
  if(chk & colors[1] == "bW") colors <- c(grey(c(0.8, 0.4)), rgb(0:1, 0:1, 1:1))

  if(chk & colors[1] == "ry") colors <- c(grey(c(0.8, 0.4)), rgb(1:1, 0:1, 0:0))
  if(chk & colors[1] == "yr") colors <- c(grey(c(0.8, 0.4)), rgb(1:1, 1:0, 0:0))

  if(chk & colors[1] == "gy") colors <- c(grey(c(0.8, 0.4)), rgb(0:1, 1:1, 0:0))
  if(chk & colors[1] == "yg") colors <- c(grey(c(0.8, 0.4)), rgb(1:0, 1:1, 0:0))

  if(chk & colors[1] == "bc") colors <- c(grey(c(0.8, 0.4)), rgb(0:0, 0:1, 1:1))
  if(chk & colors[1] == "cb") colors <- c(grey(c(0.8, 0.4)), rgb(0:0, 1:0, 1:1))

  n <- length(colors)
  q <- 0:(n-1)/(n-1)

  DefineColorMap(thresholds = q, colors = colors)
}

# =============================================================================.
#' Extract color channel values
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{TransformColors}
# -----------------------------------------------------------------------------.
#' @param x
#' character vector of colors.
#'
#' @param k
#' name of a color channel (e.g. "red", "green", "blue" and "alpha"
#' for transparency). Abbreviated names are allowed (e.g. "r", "g", "b", "a").
#'
#' @return
#' \code{ColorChannel} returns a numeric vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ColorChannel <- function(x, k) {
  chk <- is.na(x)
  x <- t(col2rgb(x, alpha = T) / 255)
  k <- substr(match.arg(tolower(k), colnames(x)), 1, 1)
  if(k == "r") i <- 1
  if(k == "g") i <- 2
  if(k == "b") i <- 3
  if(k == "a") i <- 4
  x <- x[, i]
  x[chk] <- NA
  x
}

# =============================================================================.
#' Replace color channel values
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{TransformColors}
# -----------------------------------------------------------------------------.
#' @inheritParams ColorChannel
#'
#' @param value
#' numeric values between \code{0} and \code{1}.
#'
#' @return
#' \code{ColorChannel} returns a character vector of RGBA colors in hexadecimal.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
`ColorChannel<-` <- function(x, k, value) {
  chk <- is.na(x)
  x <- t(col2rgb(x, alpha = T) / 255)
  k <- substr(match.arg(tolower(k), colnames(x)), 1, 1)
  if(k == "r") i <- 1
  if(k == "g") i <- 2
  if(k == "b") i <- 3
  if(k == "a") i <- 4
  x[, i] <- value
  x <- rgb(x[, 1:3, drop = F], alpha = x[, 4])
  x[chk] <- NA
  x
}

# =============================================================================.
#' Convert colors from HSV matrix to RGB matrix
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{rgb2hsv},
#'   \link{hsv2R},
#'   \link{rgb2R},
#'   \link{R2hsv},
#'   \link{R2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix with 3 columns representing hue (H), saturation (S), and
#' value (V) color components respectively
#'
#' @return
#' numeric matrix with 3 columns representing red (R), green (G) and blue (B)
#' color components respectively
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
hsv2rgb <- function(x) {
  x <- HSV(x[,1], x[,2], x[,3])
  x <- coords(as(x, "RGB"))
  x
}
# =============================================================================.
#' Convert colors from RGB matrix to HSV matrix
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{hsv2rgb},
#'   \link{rgb2R},
#'   \link{hsv2R}
#'   \link{R2rgb},
#'   \link{R2hsv}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix with 3 columns representing red (R), green (G) and blue (B)
#' color components respectively
#'
#' @return
#' numeric matrix with 3 columns representing hue (H), saturation (S), and
#' value (V) color components respectively
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
rgb2hsv <- function(x) {
  x <- RGB(x[,1], x[,2], x[,3])
  x <- coords(as(x, "HSV"))
  x[, "H"] <- (x[, "H"] != 360) * x[, "H"]
  x
}
# =============================================================================.
#' Convert R colors into an RGB matrix
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{R2hsv},
#'   \link{rgb2R},
#'   \link{hsv2R},
#'   \link{rgb2hsv},
#'   \link{hsv2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' vector of R colors
#'
#' @return
#' numeric matrix with 3 columns representing red (R), green (G) and blue (B)
#' color components respectively
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
R2rgb <- function(x) {
  x <- t(sapply(x, col2rgb) / 255)
  x
}
# =============================================================================.
#' Convert an RGB matrix into R colors (hexadecimal)
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{hsv2R},
#'   \link{R2rgb},
#'   \link{R2hsv},
#'   \link{rgb2hsv},
#'   \link{hsv2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix with 3 columns representing red (R), green (G) and blue (B)
#' color components respectively
#'
#' @return
#' character vector of R colors (hexadecimal)
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
rgb2R <- function(x) {
  x <- rgb(x[, 1], x[, 2], x[, 3])
  x
}
# =============================================================================.
#' Convert R colors into an HSV matrix
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{R2rgb},
#'   \link{hsv2R},
#'   \link{rgb2R},
#'   \link{rgb2hsv},
#'   \link{hsv2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' vector of R colors
#'
#' @return
#' numeric matrix with 3 columns representing hue (H), saturation (S), and
#' value (V) color components respectively
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
R2hsv <- function(x) {
  x <- R2rgb(x)
  x <- rgb2hsv(x)
  x
}
# =============================================================================.
#' Convert an HSV matrix into R colors (hexadecimal)
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{rgb2R},
#'   \link{R2hsv},
#'   \link{R2rgb},
#'   \link{rgb2hsv},
#'   \link{hsv2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix with 3 columns representing hue (H), saturation (S), and
#' value (V) color components respectively
#'
#' @return
#' character vector of R colors (hexadecimal)
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
hsv2R <- function(x) {
  x <- HSV(x[,1], x[,2], x[,3])
  x <- hex(x)
  x
}

# NOT EXPORTED #################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
m2v <- function(i, j, nrow) {
  (j - 1) * nrow + i
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
v2m <- function(x, nrow) {
  j <- (x - 1) %/% nrow + 1
  i <- (x - 1) %% nrow + 1
  x <- cbind(i, j)
  attributes(x) <- attributes(x)[1] # remove auto-generated dimnames
  x
}
# =============================================================================.
#' Detect computable values (i.e. not NA nor Inf)
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector or matrix
#'
#' @return
#' \code{FiniteValues} returns a \code{logical} vector
# -----------------------------------------------------------------------------.
#' @keywords internal
FiniteValues <- function(x) {
  if(is.null(dim(x))) {
    x <- sapply(x, FUN = is.finite)
  } else {
    n <- ncol(x)
    x <- t(apply(x, MARGIN = 1, FUN = is.finite))
    x <- rowSums(x) == n
  }
  x
}

# =============================================================================.
# Function for internal use
# -----------------------------------------------------------------------------.
resolve.legend.position <- function(pos) {
  p <- c(-1, 0, 1)
  p <- data.frame(
    id = c("bl", "b", "br", "l", "c", "r", "tl", "t", "tr"),
    name = "", x = rep(p, 3), y = p[gl(3, 3)], stringsAsFactors = F
  )
  p$name <- c(
    "bottomleft", "bottom", "bottomright", "left", "center", "right",
    "topleft", "top", "topright"
  )
  if(pos %in% p$id)   pos <- match(pos, p$id)
  if(pos %in% p$name) pos <- match(pos, p$name)
  if(! (is.numeric(pos) & length(pos) == 1 & pos > 0 & pos < 10)) {
    stop("Unknown pos value")
  }
  p <- p[pos,]
  p
}
