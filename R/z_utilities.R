# COMMON #######################################################################

# =============================================================================.
#' Localise safe numeric observations (i.e. not NA, NaN, Inf)
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{is.finite}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector or matrix.
#'
#' @return
#' \code{FiniteValues} returns a logical vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
FiniteValues <- function(x) {

  x <- is.finite(x)

  if(! is.null(dim(x))) {
    # x <- Rfast::rowsums(x, parallel = TRUE) == ncol(x)
    x <- matrixStats::rowSums2(x) == ncol(x)
  }

  x
}

# =============================================================================.
#' Rescale x linearly into [0, 1]
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{RankScore},
#'   \link{SX2Y}
# -----------------------------------------------------------------------------.
#' @description
#' rescale values linearly to the unit interval.
#'
#' @param x
#' numeric vector or matrix.
#'
#' @return
#' \code{S01} returns x linearly rescaled such that range(x) = [0, 1].
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
S01 <- function(x) {
  (x - min(x)) / diff(range(x))
}

# =============================================================================.
#' SX2Y
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{RankScore},
#'   \link{S01}
# -----------------------------------------------------------------------------.
#' @description
#' rescale x values linearly to match the range of y.
#'
#' @param x
#' numeric vector.
#'
#' @param y
#' numeric vector.
#'
#' @return
#' \code{SX2Y} returns x rescaled such that range(x) = range(y).
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
SX2Y <- function(x, y) {
  S01(x) * diff(range(y)) + min(y)
}

# =============================================================================.
#' Rescale x non-linearly into ]0, 1[
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{S01},
#'   \link{SX2Y}
# -----------------------------------------------------------------------------.
#' @description
#' rescale values non-linearly to the unit interval using rank scores \eqn{q}
#' given by \eqn{q = (rank(x) - 0.5) / N} where \eqn{N} = length(x).
#'
#' @param x
#' numeric vector.
#'
#' @return
#' \code{RankScore} returns a numeric vector of rank scores.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
RankScore <- function(x) {
  d <- dim(x)
  n <- colnames(x)
  x <- (rank(x) - 0.5) / length(x)
  if(! is.null(d)) x <- array(x, dim = d)
  if(! is.null(n)) colnames(x) <- n
  x
}



# HIDDEN #######################################################################

# =============================================================================.
#' Quick and dirty color mapping
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{autoscale},
#'   \link{AutoColorParameters},
#'   \link{ColorLegend}
# -----------------------------------------------------------------------------.
#' @inheritParams autoscale
#' @inheritParams AutoColorParameters
#'
#' @param clr.prm
#' a ColorParameter object defined by \link{DefineColorMap}.
#'
#' @param ...
#' optional arguments forwarded to the \link{AutoColorParameters} function.
#'
#' @return
#' \code{colorize} returns a vector of RGBA colors.
# -----------------------------------------------------------------------------.
#' @keywords internal
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
#' logical, show axes (default = TRUE, yes)
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
#' @keywords internal
#' @export
EmptyPlot <- function(axes = TRUE, xlab = '', ylab = '', ...) {
  # plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  graphics::plot.default(
    0, type = 'n', axes = axes, xlab = xlab, ylab = ylab, ...
  )
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
#' @keywords internal
#' @export
PlotImage <- function(m, x = NULL, y = NULL, ...) {
  image(
    x = x,
    y = y,
    z = matrix(1:length(m), nrow(m), ncol(m)),
    col = m, ...
  )
}

# =============================================================================.
#' SimulateData
# -----------------------------------------------------------------------------.
#' @param p
#' dataframe of mode generators with columns f, a, b (e.g. "rnorm", mu, sigma)
#'
#' @param m
#' matrix of observation groups (rows) where:
#' column  1 = population
#' column  2 = pairing correlation [-1 ; 1] (variable sort and shuffle)
#' columns 3.. path of modes (as indexes within p)
#'
#' @param n
#' population of each group
#'
#' @return
#' \code{SimulateData} returns a \code{list}.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
SimulateData <- function(p, m, n = 10000) {

  n <- round(m[, 1] / sum(m[, 1]) * n)
  r <- m[, 2]
  z <- sign(r)

  m <- m[, -(1:2)]
  v <- ncol(m)

  g <- rep(1:nrow(m), n)
  k <- c(0, cumsum(n))

  X <- matrix(0, sum(n), v)
  for(i in 1:nrow(m)) {
    s <- matrix(0, n[i], v)
    for(j in 1:v) {
      e <- p[m[i, j], ]
      e <- with(e, e$f[[1]](n[i], a, b))
      if(r[i] != 0) {
        e <- sort(e)
        if(r[i] == -1 & ! (j %% 2)) e <- rev(e)
        if(abs(r[i]) < 1) {
          e <- LocalShuffle(e, k = r[i] * z[i]^((j %% 2) + 1))
        }
      }
      s[, j] <- e
    }
    X[k[i] + 1:n[i], ] <- s
  }
  colnames(X) <- LETTERS[1:v]

  list(X = X, g = g)
}

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
#' logical (default = FALSE, no)
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
xylim <- function(x, y = NULL, symetric = FALSE, spacing = 0, margin = 0) {

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
#' As the name suggests...
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{colorize},
#'   \link{S01},
#'   \link{RankScore}
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
    x <- RankScore(x)
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

  # W, B, G => white, black, grey
  # r, g, b => red, green, blue
  # c, m, y => cyan, magenta, yellow

  if(is.null(colors)) colors <- grey(c(0.8, 0.7, 0.5, 0))

  chk <- length(colors) == 1
  if(chk & colors[1] == "WB") colors <- grey(1:0)
  if(chk & colors[1] == "BW") colors <- grey(0:1)

  if(chk & colors[1] == "rW") colors <- c(grey(c(0.8, 0.4)), rgb(1:1, 0:1, 0:1))
  if(chk & colors[1] == "gW") colors <- c(grey(c(0.8, 0.4)), rgb(0:1, 1:1, 0:1))
  if(chk & colors[1] == "bW") colors <- c(grey(c(0.8, 0.4)), rgb(0:1, 0:1, 1:1))

  if(chk & colors[1] == "Br") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(1, 0, 0))
  if(chk & colors[1] == "Bg") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(0, 1, 0))

  if(chk & colors[1] == "Bc") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(0, 1, 1))
  if(chk & colors[1] == "Bm") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(1, 0, 1))
  if(chk & colors[1] == "By") colors <- c(grey(c(0.8, 0.5, 0.2)), rgb(1, 1, 0))

  if(chk & colors[1] == "ry") colors <- c(grey(c(0.8, 0.4)), rgb(1:1, 0:1, 0:0))
  if(chk & colors[1] == "yr") colors <- c(grey(c(0.8, 0.4)), rgb(1:1, 1:0, 0:0))

  if(chk & colors[1] == "gy") colors <- c(grey(c(0.8, 0.4)), rgb(0:1, 1:1, 0:0))
  if(chk & colors[1] == "yg") colors <- c(grey(c(0.8, 0.4)), rgb(1:0, 1:1, 0:0))

  if(chk & colors[1] == "bc") colors <- c(grey(c(0.8, 0.4)), rgb(0:0, 0:1, 1:1))
  if(chk & colors[1] == "cb") colors <- c(grey(c(0.8, 0.4)), rgb(0:0, 1:0, 1:1))

  n <- length(colors)
  q <- 0:(n-1)/(n-1)

  if(chk & colors[1] == "Wry") {
    colors <- c(
      grey(c(1.0, 0.9, 0.4)), rgb(1, 0, 0), rgb(1, 0.5, 0), rgb(1, 1, 0.5)
    )
    n <- 5
    q <- c(0, 0.01, 1:(n-1)/(n-1))
  }
  if(chk & colors[1] == "Wgy") {
    colors <- c(
      grey(c(1.0, 0.9, 0.4)), rgb(0, 1, 0), rgb(0.5, 1, 0.5), rgb(1, 1, 0.5)
    )
    n <- 5
    q <- c(0, 0.01, 1:(n-1)/(n-1))
  }
  if(chk & colors[1] == "WGy") {
    colors <- c(
      grey(c(1.0, 0.9, 0.4)), rgb(0.3, 0.3, 0.1), rgb(0.8, 0.8, 0.2), rgb(1, 1, 0.5)
    )
    n <- 5
    q <- c(0, 0.01, 1:(n-1)/(n-1))
  }
  if(chk & colors[1] == "Wbc") {
    colors <- c(
      grey(c(1.0, 0.9, 0.4)), rgb(0, 0, 1), rgb(0.5, 0.5, 1), rgb(0.5, 1, 1)
    )
    n <- 5
    q <- c(0, 0.01, 1:(n-1)/(n-1))
  }
  if(chk & colors[1] == "WBW") {
    colors <- grey(c(1.0, 0.9, 0.3, 0.5, 0.7, 0.9))
    n <- 5
    q <- c(0, 0.01, 1:(n-1)/(n-1))
  }
  if(chk & colors[1] == "WGB") {
    colors <- grey(c(1.0, 0.9, 0.5, 0.3, 0))
    n <- 4
    q <- c(0, 0.01, 1:(n-1)/(n-1))
  }
  DefineColorMap(thresholds = q, colors = colors)
}

# NOT EXPORTED #################################################################

# =============================================================================.
# Function for internal use
# -----------------------------------------------------------------------------.
resolve.legend.position <- function(pos) {
  p <- c(-1, 0, 1)
  p <- data.frame(
    id = c("bl", "b", "br", "l", "c", "r", "tl", "t", "tr"),
    name = "", x = rep(p, 3), y = p[gl(3, 3)], stringsAsFactors = FALSE
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
