# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Binning2D
# -----------------------------------------------------------------------------.
#' @param V
#' matrix with two columns (1st = x, 2nd = y).
#'
#' @param z
#' Not yet implemented.
#'
#' @param n
#' number of bins (1st = x, 2nd = y).
#'
#' @param k
#' smoothing (1st = x, 2nd = y).
#'
#' @param xlim
#' numeric range.
#'
#' @param ylim
#' numeric range.
#'
#' @return
#' When \code{breaks = FALSE}, \code{Binning2D} returns a \code{matrix}.
#' Otherwise it returns a \code{list} with the following elements:
#' \item{x}{numeric vector}
#' \item{y}{numeric vector}
#' \item{z}{numeric matrix}
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
Binning2D <- function(
  V, z = NULL, n = 100, k = 0, xlim = NULL, ylim = NULL, breaks = TRUE, safe = FALSE
) {

  # Initializations
  n <- rep(n, length.out = 2)
  k <- rep(k, length.out = 2)

  if(! safe) V <- V[FiniteValues(V), ]

  # Ranges
  if(is.null(xlim)) xlim <- range(V[, 1])
  if(is.null(ylim)) ylim <- range(V[, 2])
  r <- rbind(xlim, ylim)

  # Breaks
  if(breaks) {
    x <- seq(xlim[1], xlim[2], by = diff(xlim) / n[1])
    y <- seq(ylim[1], ylim[2], by = diff(ylim) / n[2])
  }

  # Histogram (note: bin2 skips out of scope values)
  s <- t(ash::bin2(V, ab = r, nbin = n)$nc)

  # TODO: C or gpuR implementation may improve smoothing speed and quality
  # Smoothing
  if(k[1] > 0 & ncol(s) >= k[1]) s <- t(apply(s, MARGIN = 1, caTools::runmean, k = k[1]))
  if(k[2] > 0 & nrow(s) >= k[2]) s <-   apply(s, MARGIN = 2, caTools::runmean, k = k[2])
  # Fix round-off errors
  s[s < 0] <- 0

  # Binning coordinates of V
  # q <- S01(V)
  # q <- floor(t(n * t(q)) + (q < 1)

  if(breaks) s <- list(x = x, y = y, z = s)

  s
}
