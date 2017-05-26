# =============================================================================.
#' Group representation definition
# -----------------------------------------------------------------------------.
#' @author Benjamin Leblanc
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{groupIndex},
#'   \link{groupLegend},
#'   \link{scatterPlot},
#'   \link{updateDefinition},
#'   \link{defineColors}
# -----------------------------------------------------------------------------.
#' @param ids
#' group identifiers that are referred to when resolving group memberships
#' with \link{groupIndex}.
#'
#' @param labels
#' readable names for the groups, which are used as text labels by
#' \link{groupLegend}.
#'
#' @param colors
#' The color representing each group.
#'
#' @param ...
#' Optional parameters controlling the representation of each group.
#' At the moment, only \code{pch} and \code{cex} are supported in combination
#' with the \link{scatterPlot} function.
# -----------------------------------------------------------------------------.
#' @return
#' defineGroups returns a \code{data.frame} including at least the following
#' elements: \code{id}, \code{label}, \code{colors}.
# -----------------------------------------------------------------------------.
#' @examples
#' # Simultaneous use of color mapping and group membership ////////////////////
#'
#' layout(matrix(1:4, 2, 2, byrow = T))
#'
#' # Test data /////////////////////////////////////////////////////////////////
#' x <- runif(2000, -1, 1)
#' y <- runif(2000, -1, 1)
#' z <- sqrt(x^2 + y^2)
#'
#' # Color mapping parameters for z
#' clr.prm <- defineColors(seq(-2, 2, 0.5), grey(c(0.2, 0.8)))
#'
#' # Define 2 groups: 1 = border, 2 = center ///////////////////////////////////
#' grp.prm <- defineGroups(
#'   ids = 1:2, colors = rgb(1:2/2,0,0), cex = c(NA, 1), pch = c(15, 18),
#'   label <- c("border", "center")
#' )
#'
#' # 1. Group membership = vector of group ids /////////////////////////////////
#' grp <- 1 * (abs(z - 0.5) < 0.05) + 2 * (z < 0.15)
#'
#' scatterPlot(
#'   x, y, pch = 20,
#'   clr = x + y, clr.prm = clr.prm,
#'   grp = grp, grp.prm = grp.prm
#' )
#' groupLegend("br", parameters = grp.prm, N = table(grp)[c("1", "2")])
#'
#' # 2. Group membership = list of indices /////////////////////////////////////
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
#' # 3. Group membership = matrix of booleans //////////////////////////////////
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
defineGroups <- function(ids, labels = NULL, colors = "",  ...) {
  if(missing(ids) & ! is.null(labels)) ids <- 1:length(labels)
  if(any(duplicated(ids))) stop("Group identifiers are not unique")
  if(is.null(labels)) labels <- as.character(ids)
  if(is.null(colors)) colors <- "black"
  prm <- data.frame(
    id     = ids,
    label  = labels,
    colors = colors,
    stringsAsFactors = F
  )
  lst <- names(par())
  parameters <- list(...)
  for(p in names(parameters)) {
    if(p %in% lst) {
      chk <- is.na(parameters[[p]])
      parameters[[p]][chk] <- par(p)
    }
    prm[[p]] <- parameters[[p]]
  }
  class(prm) <- c("groupParameters", "data.frame")
  prm
}
