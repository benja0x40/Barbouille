# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Polynomial sigmoid
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
SmoothStep <- function(x, o = 3) {
  # 3rd order
  if(o == 3) s <- (3 - 2 * x) * x * x
  # 5th order
  if(o == 5) s <- ((x * 6 - 15) * x + 10) * x * x * x
  # 7th order
  if(o == 7) s <- (((70 - 20 * x) * x - 84) * x + 35) * x * x * x * x
  s
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Generalized logistic function
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
glf <- function(x, a = 0, k = 1, b = 1, q = 1, m = 0, v = 1) {
  a + (k - a) / (1 + q * exp(- b * (x - m)))^(1/v)
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Subgroup prevalence score
# -----------------------------------------------------------------------------.
#' @description
#' Preeminence score \eqn{p} is defined as:
#' \deqn{ p = \sqrt{ x * ( x - y ) } }
#' See also the examples below.
#'
#' @param x
#' numeric vector with values in [0 ; 1].
#'
#' @param y
#' numeric vector with values in [0 ; \code{x}].
#'
#' @return
#' \code{PrevalenceScore} returns a numeric vector with values in [0 ; 1]
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
PrevalenceScore <- function(x, y, f = c("linear", "quadratic", "glf"), e = 8) {

  f <- match.arg(f)

  x0 <- (x + (x == 0))
  if(f == "linear")    p <- (x - y) / x0
  if(f == "quadratic") p <- sqrt(x * (x - y)) / x0
  if(f == "glf")       p <- glf(x - y, a = -1, k = 1, b = e / x0, m = 0)

  pmax(0, pmin(p, 1))
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' RenderLayers
# -----------------------------------------------------------------------------.
#' @param LYR
#' numeric array.
#'
#' @param master
#' base color.
#'
#' @param mappers
#' layer colors.
#'
#' @param render
#' rendering method.
#'
#' @return
#' color matrix.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
RenderLayers <- function(
  LYR, master, mappers, render = c("prevalence", "maximum"), scoring = "linear"
) {

  render <- match.arg(render)

  if(length(dim(LYR)) == 2) {
    MAP <- matrix(master$cmf(LYR), nrow(LYR), ncol(LYR))
    return(MAP)
  }

  n.lyr <- dim(LYR)[3]
  if(n.lyr == 1) {
    MAP <- matrix(master$cmf(LYR[, , 1]), dim(LYR)[1], dim(LYR)[2])
    return(MAP)
  }

  MAP <- colSums(aperm(abs(LYR), c(3, 1, 2))) # Total

  maximum <- function(d) { which.max(d) }
  if(render == "maximum") {
    IDS <- apply(abs(LYR), MARGIN = c(1, 2), maximum)
  }

  # TODO: C implementation should be easy and provide significant speed boost
  prevalence <- function(d) {
    x <- which.max(d)
    y <- which.max(d[-x])
    c(x, c(d[x], d[-x][y]))
  }
  if(render == "prevalence") {
    MIX <- array(MAP, dim(LYR))
    chk <- MIX > 0
    MIX[chk] <- abs(LYR[chk]) / MIX[chk]

    MIX <- apply(MIX, MARGIN = c(1, 2), prevalence)
    MIX <- aperm(MIX, c(2, 3, 1))
    IDS <- MIX[, , 1]
    MIX <- PrevalenceScore(MIX[, , 2], MIX[, , 3], f = scoring)
  }

  MAP <- matrix(master$cmf(MAP / n.lyr), dim(LYR)[1], dim(LYR)[2]) # Average

  for(l in 1:n.lyr) {
    chk <- IDS == l
    CLR <- mappers[[l]]$cmf(LYR[, , l][chk])
    if(render == "maximum")    MAP[chk] <- CLR
    if(render == "prevalence") MAP[chk] <- BlendColors(CLR, MAP[chk], MIX[chk])
  }

  MAP
}
