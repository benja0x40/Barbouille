# =============================================================================.
#' A visually cleaned version of the graphics::boxplot function
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix or data.frame.
#'
#' @param whiskers
#' numeric vector.
#'
#' @param medpch
#' see \link{bxp}.
#'
#' @param medcex
#' see \link{bxp}.
#'
#' @param ...
#' optional arguments forwarded to the \link{bxp} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
BoxPlot <- function(
  x, whiskers = c(0.05,0.25, 0.5,0.75,0.95), medpch = 20, medcex = 0.8, ...
) {
  bp <- graphics::boxplot(x, plot = F)
  if(is.null(dim(x))) {
    bp$stats <- sapply(x, FUN = stats::quantile, probs = whiskers, na.rm = T)
  } else {
    bp$stats <- apply(
      x, MARGIN = 2, FUN = stats::quantile, probs = whiskers, na.rm = T
    )
    bp$names <- colnames(x)
  }
  graphics::bxp(
    bp, outline = F, show.names = T, axes = F, xaxs = 'i',
    pars = list(
      medlty = 1, boxlty = 1, whisklty = 1, medlwd = 1,
      medpch = medpch, medcex = medcex
    ),
    ...
  )
  graphics::axis(2)
  graphics::grid(nx = NA, ny = NULL)
}
