# =============================================================================.
#' Resolve group membership
# -----------------------------------------------------------------------------.
# TODO: indexes from a factor
# -----------------------------------------------------------------------------.
#' @author Benjamin Leblanc
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{defineGroups}
# -----------------------------------------------------------------------------.
#' @param v
#' group memberships, which can be specified using either a logical matrix,
#' a list composed of vectors defining each group, or a single vector defining
#' memberships directly. See the Examples section below for an illustration of
#' these 3 possibilities.
#'
#' @param parameters
#' data.frame of group representation parameters defined by \link{defineGroups}.
# -----------------------------------------------------------------------------.
#' @return integer vector
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
groupIndex <- function(v, parameters) {

  idx <- NULL
  n.v <- length(v)

  if(! is(parameters, "groupParameters")) stop("Class of parameters is invalid")

  # Build indexes from a logical matrix => convert to data.frame
  if(is.matrix(v) & is.logical(v)) {
    # If matrix is unnammed try default binding with group parameters
    if(is.null(colnames(v)) & ncol(v) == nrow(parameters)) {
      colnames(v) <- parameters$id
    }
    if(is.null(colnames(v))) stop("Missing group identifiers")
    v <- as.data.frame(v, stringsAsFactors = F)
  }
  # Build indexes from a list or data.frame
  if(is.list(v) & n.v > 0) {
    # If list is unnammed try default binding with group parameters
    if(is.null(names(v)) & length(v) == nrow(parameters)) {
      names(v) <- parameters$id
    }
    if(is.null(names(v)) | ! all(names(v) %in% parameters$id)) {
      stop("Unknown group identifier")
    }
    if(any(duplicated(names(v)))) {
      stop("Group identifiers are not unique")
    }

    idx <- rep(NA, n.v)
    for(id in names(v)) {
      idx[v[[id]]] <- match(id, parameters$id)
    }
  }
  # Build indexes from a factor
  if(is.factor(v) & n.v > 0) {

  }
  # Build indexes from a vector
  if(is.null(idx) & n.v > 0) {
    idx <- match(v, parameters$id)
  }

  if(is.null(idx)) stop("Unknown groups")

  idx
}
