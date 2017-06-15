# =============================================================================.
#' Quick and dirty color mapping
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector
#'
#' @param mode
#' either \code{"rank"} or \code{"0_1"} (default).
#'
#' @param colors
#' vector of colors (optional).
#'
#' @return
#' colorize returns a vector of colors
# -----------------------------------------------------------------------------.
#' @export
colorize <- function(x, mode = "0_1", colors = NULL) {

  mode <- strsplit(mode, "\\.")[[1]]

  q <- NULL
  if(! is.null(colors)) {
    n <- length(colors)
    q <- 0:(n-1)/(n-1)
  }

  if(! is.na(charmatch(mode[1], "rank"))) {
    x <- rankstat(x)
    if(is.null(q)) {
      q <- c(0, 1/3, 2/3, 1.0)
      colors <- c("grey", "black", "red", "yellow")
    }
  }
  if(mode[1] == "0_1") {
    x <- S01(x)
    if(is.null(q)) {
      q <- 0:1
      colors <- grey(0:1)
    }
  }

  clr_prm <- defineColors(thresholds = q, colors = colors)
  clr <- makeColors(x, parameters = clr_prm)

  clr
}
