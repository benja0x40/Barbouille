# =============================================================================.
#' Update color and group parameters
# -----------------------------------------------------------------------------.
#' @seealso
#' \link{defineColors},
#' \link{defineGroups}
# -----------------------------------------------------------------------------.
#' @param prm
#' list of color or group parameters, defined with \link{defineColors} or
#' \link{defineGroups} respectively.
#'
#' @param ...
#'parameters to be updated.
# -----------------------------------------------------------------------------.
#' @return updated color or group parameters
# -----------------------------------------------------------------------------.
updateDefinition <- function(prm, ...) {
  f <- NULL
  if(is(prm, "groupParameters")) {
    f <- defineGroups
  }
  if(is(prm, "colorParameters")) {
    f <- defineColors
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
