# HIDDEN #######################################################################

# > Plots ######################################################################

# =============================================================================.
#' Make plot limits including space for legends
# -----------------------------------------------------------------------------.
#' @param x numeric
#' @param y numeric
#' @param symetric logical
#' @param spacing percentage
#' @param margin percentage
#' @return list
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
#' empty.plot
# -----------------------------------------------------------------------------.
#' @param axes logical
#' @param xlab character
#' @param ylab character
#' @param ...
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
empty.plot <- function(axes = T, xlab = '', ylab = '', ...) {
  plot(0, type = 'n', axes = axes, xlab = xlab, ylab = ylab, ...)
}

# =============================================================================.
#' Plot matrix of colors as an image
# -----------------------------------------------------------------------------.
#' @param m
#' matrix of color values
#'
#' @param x
#' coordinates of the x axis bins
#'
#' @param y
#' coordinates of the y axis bins
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

# > Colors #####################################################################

# =============================================================================.
#' Replace transparency values
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{transformColors}
# -----------------------------------------------------------------------------.
#' @param x
#' character vector of colors.
#'
#' @param a
#' transparency given as a numeric value between \code{0} and \code{1}.
#'
#' @return
#' \code{ReplaceAlpha} returns a character vector of RGBA colors in hexadecimal.
# -----------------------------------------------------------------------------.
#' @export
ReplaceAlpha <- function(x, a) {
  a <- substr(rgb(0, 0, 0, alpha = a), 8, 9)
  if(length(a) == 1) a <- rep(a, length(x))
  chk <- nchar(x) == 9 & grepl("^#[0-9A-F]+", x, perl = T)
  substr(x[chk], 8, 9) <- a[chk]
  x[! chk] <- rgb(t(col2rgb(x)/255))[! chk]
  x[! chk] <- paste(x[! chk], a[! chk], sep = "")
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

# > Rescaling ##################################################################

# =============================================================================.
#' rankstat
# -----------------------------------------------------------------------------.
#' @description
#' rank statistics
#'
#' @param x
#' numeric vector
#'
#' @return
#' \eqn{(rank(x) - 0.5) / N} where N = length(x)
# -----------------------------------------------------------------------------.
#' @keywords internal
rankstat <- function(x) { (rank(x) - 0.5) / length(x) }

# =============================================================================.
#' S01
# -----------------------------------------------------------------------------.
#' @description
#' rescale x into [0, 1]
#'
#' @param x
#' numeric vector
#'
#' @return
#' \code{S01} returns x rescaled such that range(x) = [0, 1]
# -----------------------------------------------------------------------------.
#' @keywords internal
S01 <- function(x) { (x - min(x)) / diff(range(x)) }

# > Safe values ##############################################################

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

# > Legends ####################################################################

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

# > Matrices & vectors #########################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
.m2v. <- function(i, j, nrow) {
  (j - 1) * nrow + i
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.v2m. <- function(x, nrow) {
  j <- (x - 1) %/% nrow + 1
  i <- (x - 1) %% nrow + 1
  x <- cbind(i, j)
  attributes(x) <- attributes(x)[1] # remove auto-generated dimnames
  x
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.to.matrix. <- function(x) {
  if(is.null(dim(x))) x <- t(as.matrix(x))
  x
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.rep.rows. <- function(x, n) {
  x <- .to.matrix.(x)
  nc <- ncol(x)
  i <- matrix(1:length(x), nrow(x), nc, byrow = T)
  x <- rep(x[as.vector(i)], length.out = nc * n)
  matrix(x, n, nc, byrow = T)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.cbind.args. <- function(..., n = 1, k = 2) {
  x <- list(...)
  v <- sapply(x , function(x) min(1, nrow(x))) == 1
  l <- sapply(x, length)
  s <- max(which(cumsum(l > 0) == 1:length(l)))
  chk <- all(v) & (all(l[1:s] == 1) | all(l[1:s] == k) | s == 1)
  if(chk & sum(l) %in% (k * n)) {
    x <- matrix(unlist(x), 1)
  } else {
    x <- do.call(cbind, x)
  }
  if(! ncol(x) %in% (k * n)) stop("Unexpected parameter dimensions")
  x
}
