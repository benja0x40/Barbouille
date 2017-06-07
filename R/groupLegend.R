# =============================================================================.
#' Legend for group representations
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{defineGroups}
#'   \link{groupIndex}
#'   \link{scatterPlot}
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
#' data.frame of group representation parameters defined by \link{defineGroups}.
#'
#' @param N
#' number of elements in each group. The legend will indicate these numbers
#' if they are provided (default = none).
#'
#' @param ...
#' optional parameters passed to the \link{legend} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @examples
#' # Simultaneous use of color mapping and group membership
#'
#' layout(matrix(1:4, 2, 2, byrow = T))
#'
#' #  uniformely distributed random variables
#' x <- runif(2000, -1, 1)
#' y <- runif(2000, -1, 1)
#' z <- sqrt(x^2 + y^2)
#'
#' clr.prm <- defineColors(seq(-2, 2, 0.5), grey(c(0.2, 0.8)))
#' grp.prm <- defineGroups(
#'   ids = 1:2, colors = rgb(1:2/2,0,0), cex = c(NA, 1), pch = c(15, 18)
#' )
#' grp.prm$label <- c("border", "center")
#'
#' # 1st possibility to provide the group memberships: vector of group ids
#' grp <- 1 * (abs(z - 0.5) < 0.05) + 2 * (z < 0.15)
#'
#' scatterPlot(
#'   x, y, pch = 20,
#'   clr = x + y, clr.prm = clr.prm,
#'   grp = grp, grp.prm = grp.prm
#' )
#' groupLegend("br", parameters = grp.prm, N = table(grp)[c("1", "2")])
#'
#' # 2nd possibility to provide the group memberships: list of elements
#' grp <- list(
#'   `1` = which(abs(z - 0.5) < 0.05),
#'   `2` = which(z < 0.15)
#' )
#'
#' scatterPlot(
#'   x, y, pch = 20,
#'   clr = x + y, clr.prm = clr.prm,
#'   grp = grp, grp.prm = grp.prm
#' )
#' groupLegend("br", parameters = grp.prm, N = sapply(grp, length))
#'
#' # 3rd possibility to provide the group memberships: matrix of booleans
#' grp <- cbind(
#'   `1` = abs(z - 0.5) < 0.05,
#'   `2` = z < 0.15
#' )
#' scatterPlot(
#'   x, y, pch = 20,
#'   clr = x + y, clr.prm = clr.prm,
#'   grp = grp, grp.prm = grp.prm
#' )
#' groupLegend("br", parameters = grp.prm, N = apply(grp, MAR = 2, FUN = sum))
# -----------------------------------------------------------------------------.
#' @export
groupLegend <- function(pos, parameters, N = NA, ...) {

  # Resolve legend position
  pos <- .resolve.legend.position.(pos)$name

  if(! is(parameters, "groupParameters")) stop("Class of parameters is invalid")

  # Support manually updated parameters and verify consistency
  parameters <- updateDefinition(parameters)

  clr <- parameters$colors
  txt <- parameters$label

  idx <- which(! is.na(N))
  if(length(idx) > 0) {
    if(length(N) != length(txt)) stop("Inconsistent number of elements")
    txt <- str_pad(txt, width = max(nchar(txt)), side = "right", pad = " ")
    txt[idx] <- paste(txt[idx], " (N = ", as.character(N[idx]), ")", sep="")
  }

  args <- c(
    list(x = pos, y = NULL, legend = txt, col = clr), list(...)
  )

  lst <- colnames(parameters)
  lst <- lst[lst %in% c("pch", "cex", "lty", "lwd", "density")]
  if(length(lst) > 0) {
    for(x in lst) {
      if(x == "cex")  args[["pt.cex"]] <- parameters[[x]]
      else args[[x]] <- parameters[[x]]
    }
  }
  if(is.null(args$pch)) args$fill <- clr

  do.call(legend, args)
}
