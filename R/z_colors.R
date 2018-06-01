# =============================================================================.
#' Mix two colors in RGB space
# -----------------------------------------------------------------------------.
#' @param x
#' vector of R colors.
#'
#' @param y
#' vector of R colors.
#'
#' @param gamma
#' numeric vector with values in [0 ; 1] specifying the proportion of \code{x}
#' when mixing \code{x} and \code{y} colors.
#'
#' @return
#' \code{BlendColors} returns a vector of R colors.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' n <- 50
#'
#' x <- 0:(n^2 - 1) / (n^2 - 1)
#' y <- rep(0:(n - 1) / (n - 1), length.out = length(x))
#'
#' cm1 <- DefineColorMap(thresholds = 0:2/2, colors = c("blue", "yellow", "red"))
#' cm2 <- DefineColorMap(thresholds = 0:2/2, colors = grey(c(1, 0, 1)))
#'
#' c1 <- MakeColors(x, cm1)
#' c2 <- MakeColors(x, cm2)
#'
#' clr <- BlendColors(c1, c2, gamma = y)
#'
#' plot(x, y, col = clr, pch = 20, cex = 2)
#'
#' cm1 <- AutoColorParameters("Wry")
#' cm2 <- AutoColorParameters("WBW")
#'
#' c1 <- MakeColors(x, cm1)
#' c2 <- MakeColors(x, cm2)
#'
#' clr <- BlendColors(c1, c2, gamma = y)
#'
#' plot(x, y, col = clr, pch = 20, cex = 2)
#'
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
BlendColors <- function(x, y, gamma) {

  VectorArgs(c("x", "y", "gamma"))

  x <- t(grDevices::col2rgb(x, alpha = TRUE) / 255)
  y <- t(grDevices::col2rgb(y, alpha = TRUE) / 255)
  x <- gamma * x + (1 - gamma) * y

  grDevices::rgb(x[, 1], x[, 2], x[, 3], x[, 4])
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
  lst <- c("r", "g", "b", "a")
  if(k %in% lst) {
    x <- R2rgb(x)
    k <- substr(tolower(k), 1, 1)
    k <- match(k, lst)
  }
  lst <- c("h", "s", "v")
  if(k %in% lst) {
    x <- R2hsv(x)
    k <- substr(tolower(k), 1, 1)
    k <- match(k, lst)
  }
  x <- x[, k]
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
  lst <- c("r", "g", "b", "a")
  if(k %in% lst) {
    x <- R2rgb(x)
    k <- substr(tolower(k), 1, 1)
    k <- match(k, lst)
    x[, k] <- value
    x <- rgb2R(x)
    x[chk] <- NA
    return(x)
  }
  lst <- c("h", "s", "v")
  if(k %in% lst) {
    x <- R2hsv(x)
    k <- substr(tolower(k), 1, 1)
    k <- match(k, lst)
    x[, k] <- value
    x <- hsv2R(x)
    x[chk] <- NA
    return(x)
  }
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
  x[, 1:3] <- colorspace::coords(
    as(colorspace::HSV(x[, 1], x[, 2], x[, 3]), "RGB")
  )
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
  x[, 1:3] <- colorspace::coords(
    as(colorspace::RGB(x[,1], x[,2], x[,3]), "HSV")
  )
  x[, 1] <- (x[, 1] != 360) * x[, 1]
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
  x <- t(sapply(x, col2rgb, alpha = TRUE) / 255)
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
#' numeric matrix with 4 columns representing
#' red (R), green (G), blue (B) and alpha (A)
#' color components respectively
#'
#' @return
#' character vector of R colors (hexadecimal)
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
rgb2R <- function(x) {
  x <- rgb(x[, 1], x[, 2], x[, 3], x[, 4])
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
  rgb2hsv(R2rgb(x))
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
  rgb2R(hsv2rgb(x))
}
