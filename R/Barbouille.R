# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' List of global options for the Barbouille package
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
DefaultOptions <- function() {
  list(
    # rootpath  = "",            # MakePath
    # # Images generation
    # path     = "",
    # name     = "img",
    # counter  = 0,
    # relative = T,
    # type     = "png",
    #
    # makedir  = T,
    # rebuild  = F,
    #
    # resolution = 300,
    # width      = 4,
    # height     = 4.25,
    # units      = "in",
    #
    # par = list(pch = 20, mar = c(4.5, 4.5, 4.0, 2.0)),

    # Plot parameters
    extend     = 1.0,
    bins       = 200,
    db         = 50,
    vb         = 50,
    smoothing  = c(5, 5),
    sampling   = c(2E5, 5E6), # disabled using c(0, Inf)
    spray      ="uniform",
    fwhm       = 1/2,
    stencil    = "linear",
    scales     = "absolute",
    ranking    = F,
    render     = "prevalence",
    scoring    = "glf",
    gradient   = "bright",
    saturation = 1.0,
    contrast   = 0.4,
    layout     = "horizontal",
    spacing    = 0,
    grid       = grey(0.5, alpha = 0.5),
    axes       = T,
    box        = T,
    names      = T,
    las        = 1,
    label      = NA,

    # Traceability
    messages = T,
    history  = "Barbouille"
  )
}

# =============================================================================.
#' Global options for Barbouille functions
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{ScatterPlot},
#'   \link{SideBySide}
# -----------------------------------------------------------------------------.
#' @description
#' This function sets the default value of arguments used by the main functions
#' of the Barbouille package.
#'
#' @param ...
#' Any of the following arguments:
# -----------------------------------------------------------------------------.
#' @export
Barbouille <- function(...) {

  opt <- names(Barbouille::DefaultOptions())

  cfg <- list(...)
  cfg <- cfg[names(cfg) %in% opt]
  cfg <- cfg[! sapply(cfg, is.null)]

  if(length(cfg) > 0) {
    names(cfg) <- paste0("Barbouille.", names(cfg))
    options(cfg)
  } else {
    cfg <- options()[paste0("Barbouille.", opt)]
    if(any(is.na(names(cfg)))) {
      stop("missing global options")
    } else {
      names(cfg) <- gsub("^Barbouille\\.", "", names(cfg))
    }
    cfg
  }
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Remove global options of the Barbouille package from the R environment
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
RemoveOptions <- function() {
  cfg <- options()
  cfg <- cfg[grepl( "^Barbouille\\.", names(cfg))]
  cfg[] <- vector("list", length(cfg))
  options(cfg)
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Reinitialize global options of the Barbouille package
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ResetOptions <- function() {
  do.call(Barbouille, Barbouille::DefaultOptions())
}
