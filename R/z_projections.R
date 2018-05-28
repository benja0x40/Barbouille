# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{UnivariateProjection},
#'   \link{BivariateProjection}
# -----------------------------------------------------------------------------.
#' @description
#' Produce noise to be added to numeric values
#'
#' @param n
#' number of pseudo random values to generate.
#'
#' @param spray
#' defines the distribution used to generate noise values.
#' This can be either \code{"uniform"} (default), \code{"triangle"},
#' \code{"cosine"}, or \code{"normal"}.
#' It is possible to use non-ambiguous abbreviated forms of these keywords.
#'
#' @param fwhm
#' positive numeric value defining the Full Width at Half Maximum of the normal
#' distribution used when \code{spray = "normal"}.
#' The default value is 0.5 and produces normally distributed noise values with
#' a similar span as when using the triangle and cosine distributions which
#' both have the same FWHM of 0.5 by design.
#'
#' @return
#' \code{Atomize} returns a numeric vector of length \code{n} with values
#' centered at 1/2 and strictly within [0 ; 1] when using the uniform, triangle
#' or cosine distributions.
# -----------------------------------------------------------------------------.
#' @examples
#'
#'\dontrun{
#'
#' layout(matrix(1:8, 2, 4, byrow = T))
#' par(pch = ".", col = grey(0, 0.2))
#'
#' lst <- c("uniform", "triangle", "cosine", "normal")
#'
#' n <- 10000
#' x <- y <- rep(1:4, length.out = n)
#'
#' for(j in lst) {
#'   u <- x + Atomize(n, spray = j) - 0.5
#'   v <- y + Atomize(n, spray = j) - 0.5
#'   plot(u, v, main = j)
#' }
#'
#' par(pch = 20, col = grey(0, 0.5))
#'
#' n <- 25000
#'
#' for(j in lst) {
#'   x <- Atomize(n, spray = j)
#'   d <- ASH1D(x, k = 10)
#'   plot(x, d, main = j, xlim = c(-0.5, 1.5))
#' }
#' }
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
Atomize <- function(
  n, spray = c("uniform", "triangle", "cosine", "normal"), fwhm = 1/2
) {

  cospdf <- function(x) { (x >= 0 & x <= 1) * (1 - cos(2 * pi * x)) / (2 * pi) }

  spray <- match.arg(spray)

  if(spray == "uniform") {
    x <- stats::runif(n)
  }
  if(spray == "triangle") {
    x <- triangle::rtriangle(n, a = 0, b = 1, c = 0.5)
  }
  if(spray == "cosine") {
    cosine <- distr::AbscontDistribution(
      d = cospdf, Symmetry = distr::SphericalSymmetry(0.5)
    )
    x <- distr::r(cosine)(n)
  }
  if(spray == "normal") {
    sigma <- fwhm / sqrt(2 * log(2))
    x <- stats::rnorm(n, mean = 0, sd = sigma) / 2 + 0.5
  }

  x
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{BivariateProjection},
#'   \link{Atomize}
# -----------------------------------------------------------------------------.
#' @description
#' Univariate projection
#'
#' @inheritParams Atomize
#'
#' @param X
#' numeric vector or matrix with positive values, for instance representing
#' empirical densities or weights. When providing a matrix, each column of X
#' corresponds to a group, and the membership of each observation (rows) is
#' being defined by the \code{grp} argument.
#'
#' @param grp
#' vector representing group memberships, required only when the \code{X}
#' argument is a matrix with more than 1 column.
#' In this case, the length of \code{grp} must be equal to \code{nrow(X)}
#' and its values must be integers between 1 and \code{ncol(X)}.
#'
#' @param proportions
#' either "static", "global", "local" or NULL (default, no proportions).
#' In static mode, the areas representing each group are equal.
#' In global mode, these areas are proportional to the global population of
#' each group and in local mode, these areas are proportional to the local
#' population of each group.
#'
#' @param ordering
#' either "static", "global" or "local" or NULL  (default, no ordering).
#' In static mode, groups are ordered by their numeric identifier.
#' In global mode, groups are ordered by their global population and in local
#' mode, groups are ordered by their local population.
#'
#' @param rounding
#' integer (default = 2).
#'
#' @param violin
#' logical (default = F, no).
#'
#' @param vmode
#' either "linear" (default) or "rank".
#'
#' @return
#' \code{UnivariateProjection} returns a numeric vector representing 1D
#' coordinates.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
UnivariateProjection <- function(
  X, Y = NULL, grp = NULL, proportions = NULL, ordering = NULL, rounding = 2,
  spray = "uniform", fwhm = 1/2, violin = F, vmode ="linear"
) {

  # 2D (row, col) indices to 1D indices for a matrix with given number of rows
  # m2v <- function(i, j, nrow) {
  #   (j - 1) * nrow + i
  # }

  # Cumulative shift functions (z[1] => group id, z[-1] => group proportions)
  cs_by_global_population <- function(z) {
    sum(z[1 + which(g.pop > g.pop[z[1]])])
  }
  cs_by_local_proportion <- function(z) {
    sum(z[1 + which(order(z[-1]) > z[1])])
  }

  # Initializations
  if(is.null(dim(X))) X <- matrix(X, nrow = length(X), ncol = 1)
  n.obs <- nrow(X)
  n.grp <- ncol(X)

  if(is.null(grp)) grp <- matrix(1, n.obs, 1)
  g.pop <- tabulate(grp)

  if(is.null(proportions)) proportions <- ""
  if(is.null(ordering))    ordering    <- ""

  # Atomizeing factors (depends on group proportions)
  # TODO: add the total density as parameter to avoid recomputing it
  rsm <- Rfast::rowsums(X, parallel = T)
  SPD <- matrix(1, n.obs, n.grp)
  if(n.grp > 1 & proportions == "static") {
    SPD <- matrix(1 / n.grp, n.obs, n.grp)
  }
  if(n.grp > 1 & proportions == "global") {
    SPD <- matrix(g.pop, n.obs, n.grp, byrow = T) / sum(g.pop)
  }
  # TODO: smooth proportions using 1D binning Y??? (may be redundant with ASH)
  if(n.grp > 1 & proportions == "local") {
    SPD <- X / rsm
    SPD[which(rsm == 0), ] <- 0
  }

  # TODO: correct noisy variance of proportions when values are very low
  if(n.grp > 1 & rounding > 0 & ! proportions %in% c("", "static")) {
    SPD <- Rfast::Round(SPD, rounding)
    tot <- Rfast::rowsums(SPD, parallel = T)
    SPD <- SPD / tot
    SPD[which(tot == 0), ] <- 0
  }

  # Cumulative position shift (depends on group ordering)
  s <- 0
  # TODO: user defined static order (equivalent to renumbering groups)
  if(n.grp > 1 & ordering == "static") {
    s <- rep(0, n.obs)
    i <- which(grp > 1)
    j <- m2v(1:length(i), grp[i] - 1, nrow = length(i))
    s[i] <- t(apply(SPD[i, ], MARGIN = 1, cumsum))[j]
  }
  # TODO: same optimization as for static ordering
  if(n.grp > 1 & ordering == "global") {
    s <- apply(cbind(grp, SPD), MARGIN = 1, cs_by_global_population)
  }
  # TODO: smooth/optimize group order to remove high frequency oscillations
  if(n.grp > 1 & ordering == "local") {
    s <- apply(cbind(grp, SPD), MARGIN = 1, cs_by_local_proportion)
  }

  # Index each observation in the X matrix (depends on group membership)
  k <- m2v(i = 1:n.obs, j = grp, nrow = n.obs)

  # Position parameter in [0, 1]
  p <- Atomize(n.obs, spray = spray, fwhm = fwhm)

  # Multiply position parameter by spraying factors
  p <- p * SPD[k]

  # Add cumulative position shifts
  p <- p + s

  # Scaling by local proportions (optional)
  if(violin) {
    if(vmode == "rank") {
      rsm <- RankScore(rsm)
    } else {
      rsm <- rsm / max(rsm)
    }
    p <- 0.5 + (p - 0.5) * rsm^violin
  }

  p
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{UnivariateProjection},
#'   \link{Atomize}
# -----------------------------------------------------------------------------.
#' @inheritParams Atomize
#'
#' @description
#' Bivariate projection
#'
#' @param V
#' numeric matrix with two columns representing bivariate observations.
#'
#' @param stencil
#' either "linear", "cosine" or "sigmoid".
#' It is possible to use non-ambiguous abbreviated forms of these keywords.
#'
#' @return
#' \code{BivariateProjection} returns a numeric matrix representing 2D
#' coordinates.
# -----------------------------------------------------------------------------.
#' @examples
#'
#'\dontrun{
#'
#' n <- 50000
#' V <- cbind(
#'   c(rnorm(n, 10, 5), rnorm(n, -10, 1)),
#'   c(rnorm(n, 10, 1), rnorm(n, -10, 5))
#' )
#'
#' P <- BivariateProjection(V)
#' r <- BivariateDensity(P)
#' }
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
BivariateProjection <- function(
  V, stencil = c("linear", "cosine", "sigmoid"), spray = "uniform", fwhm = 1/2
) {

  # Position parameter in [0, 1]
  x <- Atomize(nrow(V), spray = spray, fwhm = fwhm)

  stencil <- match.arg(stencil)

  if(stencil == "linear") {
    y <- x
  }
  if(stencil == "cosine") {
    y <- (1 + cos(pi * (x + 1))) / 2
  }
  if(stencil == "sigmoid") {
    k <- 15
    y <- (1 / (1 + exp(- k * (x - 0.5))))
  }

  y <- V[, 1] + y * (V[, 2] - V[, 1])

  # Coordinates
  cbind(x, y)
}
