# =============================================================================.
#' Make a destination path creating folders when necessary
# -----------------------------------------------------------------------------.
#' @param x
#' character value indicating a file system path.
#'
#' @return
#' see \link{system}.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
mkdir <- function(x) {
  system(paste0('mkdir -p "', x, '"'))
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
#' @keywords internal
img_path <- function(img = NULL) {
  if(is.null(img)) img <- CFG$images$name
  nbr <- CFG$images$counter
  img <- paste0(img, "_Fig", stringr::str_pad(nbr, width = 2, pad = "0"))
  img <- paste0(CFG$images$path, img, ".png")
  img
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
#' @keywords internal
img_open <- function(
  img = NULL, w = NULL, h = NULL, u = NULL, r = NULL, p = NULL, build = NULL
) {

  if(is.null(build)) build <- CFG$images$build

  if(is.null(w)) w <- CFG$images$width
  if(is.null(h)) h <- CFG$images$height
  if(is.null(u)) u <- CFG$images$units
  if(is.null(r)) r <- CFG$images$resolution
  if(is.null(p)) p <- CFG$images$par

  CFG$images$counter <- CFG$images$counter + 1

  img <- img_path(img)

  if(! dir.exists(dirname(img))) trash <- mkdir(dirname(img))

  if(file.exists(img)) msg <- "[updating]" else msg <- "[creating]"
  if(build | ! file.exists(img)) {
    png(img, width = w, height = h, units = u, res = r)
    do.call(par, p)
    build <- T
  } else{
    msg <- "[passing]"
  }

  message(msg, " ", img)

  list(path = img, build = build)
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
#' @keywords internal
img_close <- function() {
  if(CFG$images$counter > 0) trash <- dev.off()
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
mkimg <- function(x, Rmd = TRUE) {
  img <- globalenv()$img
  img <- if(is.list(img)) do.call(img_open, img) else img_open(img)
  if(img$build) {
    p <- eval(expression(x))
    if(is(p, "gg")) {
      p <- p + theme(plot.margin = unit(par()$mar * 51/75, units = "lines"))
      print(p)
    }
    trash <- dev.off() # i.e. img_close()
  }
  if(Rmd) include_graphics(img$path)
}

