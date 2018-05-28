# =============================================================================.
#' Add text labels at a set of point coordinates
# -----------------------------------------------------------------------------.
#' @author Benjamin Leblanc, Itys Comet
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{ScatterPlot}
# -----------------------------------------------------------------------------.
#' @description
#' PointLabels overlays text labels and arrows to annotate a specific set of
#' points within a 2D scatterplot.
#'
#' @param x
#' the x axis coordinate of points to be labelled.
#'
#' @param y
#' the y axis coordinate of points to be labelled.
#'
#' @param centroid.shift
#' displacement vector used to control the focus point of labels and associated
#' arrows.
#'
#' @param distance
#' the distance between points and corresponding labels
#' (e.g. arrows length).
#'
#' @param pts.offset
#' the spacing distance between points and arrow ends.
#'
#' @param txt.offset
#' the spacing distance between text labels and arrow ends.
#'
#' @param labels
#' character vector with text labels, one for each point to be annotated.
#'
#' @param col
#' vector of colors for text labels and arrows (default = \code{"black"}).
#'
#' @param plot
#' logical activating the drawing of text labels and arrows
#' (default = \code{T}).
#'
#' @param show.centroid
#' logical activating the visualization of the focus point of labels and
#' arrows (default = \code{F}).
#'
#' @param ...
#' optional arguments forwarded to the \link{text} function.
#'
#' @return
#' \code{PointLabels} returns a \code{list} with the following elements:
#' \item{labels}{
#'   character vector with text labels
#' }
#' \item{offset}{
#'   value of the text \code{offset} parameter
#' }
#' \item{pos}{
#'   value of the text \code{pos} parameter
#' }
#' \item{lab.x, lab.y}{
#'   the x and y coordinates for text labels
#' }
#' \item{pts.x, pts.y}{
#'   the x and y coordinates for arrows between text labels and corresponding
#'   points
#' }
#' \item{centroid}{
#'   coordinates of the centroid
#' }
# -----------------------------------------------------------------------------.
#' @export
PointLabels <- function(
  x, y,
  centroid.shift = c(0,0), distance = 1, pts.offset = 0.1, txt.offset = 0.2,
  labels = NULL, col = "black", plot = T, show.centroid = F, ...
) {

  calc.xy <- function(pts, ctr, distance, delta, a, b) {
    # Explanations?
    B <- a * ctr[1] + b - ctr[2]
    Z <- (distance^2 - B^2) / (1 + a^2)
    A <-          - (a * B) / (1 + a^2)
    dx <- (Z + A^2)^0.5 - A
    dy <- (distance^2 - dx^2)^0.5

    xy <- pts + sign(delta) * rbind(dx, dy)
    xy
  }

  # Point coordinates (1st row = x, 2nd row = y, columns = points)
  pts <- rbind(x, y)

  # Centroid coordinates with optional centroid shift vector
  if(nrow(pts) == 1 & all(centroid.shift == 0)) {
    centroid.shift <- c(0.1, 0.1)
    warning("centroid.shift was set to c(0.1, 0.1)")
  }
  ctr <- rowMeans(pts) + centroid.shift

  # Compute delta x, delta y and the corresponding slopes and intercepts
  delta <- pts - ctr
  a <- delta[2,] / delta[1,]
  b <- ctr[2] - a * ctr[1]

  # Resolve placement of labels and arrows
  lab.pos <- calc.xy(pts, ctr, distance,   delta, a, b)
  pts.pos <- calc.xy(pts, ctr, pts.offset, delta, a, b)
  pos     <- sign(delta[1,]) + 3

  # Draw arrows and show text labels if the plot option is activated
  if(plot) {
    # Show the centroid location (optional)
    points(ctr[1], ctr[2], pch = 3, col = rgb(0, 0, 0, show.centroid * 0.25))
    arrows(
      x0 = pts.pos[1,], y0 = pts.pos[2,], x1 = lab.pos[1,], y1 = lab.pos[2,],
      length = 0, angle = 30, code = 1, col = col
    )
    text(
      t(lab.pos), labels, pos = pos, offset = txt.offset,
      col = col, ...
    )
  }

  list(
    # Allows external/custom use of text() and arrows()
    labels = labels,
    offset = txt.offset,
    pos    = pos,
    lab.x = lab.pos[1,],
    lab.y = lab.pos[2,],
    pts.x = pts.pos[1,],
    pts.y = pts.pos[2,],
    centroid = ctr
  )
}
