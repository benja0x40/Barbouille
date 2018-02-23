# =============================================================================.
#' Update color mapping or group highlighting parameters
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{DefineColorMap},
#'   \link{DefineGroupStyles}
# -----------------------------------------------------------------------------.
#' @param prm
#' a ColorParameters or GroupParameters object previously defined using
#' \link{DefineColorMap} or \link{DefineGroupStyles} respectively.
#'
#' @param ...
#' list of parameter values replacing the existing ones (see the arguments of
#' ColorParameters and GroupParameters definition functions).
#'
#' @return
#' \code{UpdateDefinition} returns the updated ColorParameters or
#' GroupParameters object.
# -----------------------------------------------------------------------------.
#' @export
UpdateDefinition <- function(prm, ...) {
  f <- NULL
  if(is(prm, "GroupParameters")) {
    f <- DefineGroupStyles
  }
  if(is(prm, "ColorParameters")) {
    f <- DefineColorMap
  }
  if(is.null(f)) stop("Class of prm is invalid")
  args <- list(...)
  if(length(args) > 0) {
    for(a in names(args)) {
      prm[[a]] <- args[[a]]
    }
  }
  prm <- do.call(f, prm)
  prm
}
