# LittleThumb ##################################################################

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Provide default values to unspecified arguments
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
DefaultArgs <- function(
  default, ignore = NULL, from = NULL, to = NULL, dots = NULL
) {

  lst <- names(default)

  if(is.null(from)) from <- parent.frame()
  if(is.null(to)) to <- parent.frame()

  if(is.function(from)) {
    lst <- methods::formalArgs(from)
    from <- parent.frame()
  }

  lst <- setdiff(lst, ignore)

  for(a in lst) {
    if(! (is.null(from[[a]]) | identical(from, to))) {
      to[[a]] <- from[[a]]
    }
    if(is.null(to[[a]]) & ! is.null(default[[a]])) {
      to[[a]] <- default[[a]]
    }
  }
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Standardize the value of clonal arguments
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ClonalArg <- function(u, a, d) { # user value, arg names, default value

  n <- length(a)
  r <- rep(list(d), n)
  names(r) <- a

  if(is.null(names(u))) {
    d[] <- rep(u, length.out = length(d))
    r[] <- rep(list(d), n)
  } else {
    u <- lapply(u, rep, length.out = length(d))
    for(k in names(u)) r[[k]][] <- u[[k]]
  }

  r
}

# Barbouille ###################################################################

# =============================================================================.
#' Compose default atomic values merged with user values
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{ReferenceArgs},
#'   \link{ComposeArgs},
#'   \link{AssignArgs},
#'   \link{FormatVectors}
# -----------------------------------------------------------------------------.
#' @param u
#' user argument values.
#'
#' @param a
#' default argument values.
#'
#' @return
#' \code{AtomicArgs} returns an R object of the same type as \code{a}.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' a <- list(x = "a", y = "b", z = "c")
#' AtomicArgs(u = NULL, a)
#' AtomicArgs(u = "d", a)
#' AtomicArgs(u = c(y = "d"), a)
#' AtomicArgs(u = list(y = "d"), a)
#' AtomicArgs(u = c(x = "d", z = "d"), a)
#' AtomicArgs(u = list(x = "d", z = "d"), a)
#'
#' a <- list(x = 0, y = 0, z = 0)
#' m <- matrix(1:4, 2, 2)
#' AtomicArgs(u = m, a)
#' AtomicArgs(u = list(x = m), a)
#' AtomicArgs(u = list(y = m), a)
#' AtomicArgs(u = list(z = m), a)
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
AtomicArgs <- function(u, a) {
  v <- a
  n_a <- ! is.null(names(a))
  n_u <- ! is.null(names(u))
  e_u <- ! is.null(u)
  if(e_u &   n_a &   n_u) v[names(u)] <- u
  if(e_u &   n_a & ! n_u) v[names(v)] <- rep(list(u), length(v))
  if(e_u & ! n_a)         v <- u
  v
}

# =============================================================================.
#' Compose empty references merged with user values
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{AtomicArgs},
#'   \link{ComposeArgs},
#'   \link{AssignArgs},
#'   \link{FormatVectors}
# -----------------------------------------------------------------------------.
#' @param u
#' user argument values.
#'
#' @param r
#' default argument names.
#'
#' @return
#' \code{ReferenceArgs} returns a \code{list}.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' r <- letters[1:5]
#'
#' ReferenceArgs(u = NULL, r)
#' ReferenceArgs(u = 1, r)
#' ReferenceArgs(u = 1:2, r)
#' ReferenceArgs(u = matrix(0, 2, 2), r)
#' ReferenceArgs(u = c(b = 1), r)
#' ReferenceArgs(u = list(d = 1), r)
#' ReferenceArgs(u = c(b = 1, d = 2), r)
#' ReferenceArgs(u = list(b = 1:2, d = 3:4), r)
#' ReferenceArgs(u = list(z = 0), r)
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ReferenceArgs <- function(u, r) {
  v <- vector("list", length(r))
  names(v) <- r
  n_u <- ! is.null(names(u))
  e_u <- ! is.null(u)
  if(e_u &   n_u) v[names(u)] <- u
  if(e_u & ! n_u) v[names(v)] <- rep(list(u), length(v))
  v
}

# =============================================================================.
#' Compose default arguments merged with user values
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{AtomicArgs},
#'   \link{ReferenceArgs},
#'   \link{AssignArgs},
#'   \link{FormatVectors}
# -----------------------------------------------------------------------------.
#' @param u
#' list of user argument.
#'
#' @param d
#' list of default argument.
#'
#' @return
#' \code{ComposeArgs} returns a \code{list}.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ComposeArgs <- function(u, d) {

  if(is.null(u)) u <- list()

  # Resolve atomic values (defined as: name = value, ...)
  for(x in names(d$a)) {
    d$a[[x]] <- u[[x]] <- AtomicArgs(u[[x]], d$a[[x]])
  }

  # Resolve references: (defined as: a = atomic values, r = names, n = length)
  for(x in names(d$r)) {

    a <- d$r[[x]]$a
    r <- d$r[[x]]$r
    n <- d$r[[x]]$n

    if(is.null(r)) r <- names(a)
    if(is.null(n)) n <- 0

    if(is.null(u[[x]])) u[[x]] <- list()
    if(n > 0) u[[x]] <- rep(u[[x]], length.out = n)
    if(is.list(u[[x]])) {
      u[[x]] <- lapply(u[[x]], ReferenceArgs, r = r)
    } else {
      u[[x]] <- ReferenceArgs(u[[x]], r = r)
    }

    # Reference to other references
    lst <- names(d$r)[! sapply(sapply(d$r, "[[", "a"), is.null)]
    lst <- lst[lst %in% r & ! lst %in% names(a)]
    if(length(lst) > 0) a <- c(lapply(d$r[lst], "[[", "a"), a)

    # Reference to atomic values
    lst <- names(d$a)
    lst <- lst[lst %in% r & ! lst %in% names(a)]
    if(length(lst) > 0) a <- c(d$a[lst], a)

    if(n == 0) u[[x]] <- ComposeArgs(u[[x]], d = list(a = a))
    if(n >= 1) u[[x]] <- lapply(u[[x]], ComposeArgs, d = list(a = a))
  }

  u
}

# =============================================================================.
#' Compose default arguments merged with user values
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{AtomicArgs},
#'   \link{ReferenceArgs},
#'   \link{ComposeArgs},
#'   \link{FormatVectors}
# -----------------------------------------------------------------------------.
#' @description
#' \code{AssignArgs} modifies user arguments in its parent environment.
#'
#' @param u
#' list of user argument.
#'
#' @param d
#' list of default argument.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @examples
#'
#' def <- list(
#'   a = list(x = 1, y = 2, b = c(0, 0), s = list(x = 0, y = 0)),
#'   r = list(
#'     f = list(a = list(x = "a", y = "b")),
#'     m = list(r = c("x", "y", "f"), n = 5)
#'   )
#' )
#'
#' x <- y <- b <- s <- f <- m <- NULL
#' AssignArgs(NULL, def)
#'
#' usr <- list(b = c(5, 5))
#' AssignArgs(usr, def)
#'
#' usr <- list(s = 5)
#' AssignArgs(usr, def)
#'
#' usr <- list(f = ".")
#' AssignArgs(usr, def)
#'
#' usr <- list(f = c(x = "+", y = "-"))
#' AssignArgs(usr, def)
#'
#' M <- matrix(1:4, 2, 2)
#' usr <- list(
#'   x = M,
#'   m = list(
#'     list(y = 1:2, f = c(x = "A")),
#'     list(x = 3:4, f = list(y = "B"))
#'   )
#' )
#' AssignArgs(usr, def)
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
AssignArgs <- function(u, d) {

  obj <- deparse(substitute(u))
  env <- parent.frame() # parent.env(environment())

  u <- ComposeArgs(u, d)
  for(lbl in names(u)) env[[lbl]] <- u[[lbl]]

  suppressWarnings(rm(list = obj, pos = env))
}

# =============================================================================.
#' Format simple vector arguments
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{AtomicArgs},
#'   \link{ReferenceArgs},
#'   \link{ComposeArgs},
#'   \link{AssignArgs}
# -----------------------------------------------------------------------------.
#' @param u
#' user values.
#'
#' @param n
#' required vector length.
#'
#' @param names
#' optional names.
#'
#' @return
#' \code{FormatVectors} returns an R vector of the same type as \code{u}.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' nx <- ny <- 0
#' u <- list(nx = nx, ny = ny)
#' FormatVectors(u, 3)
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
FormatVectors <- function(u, n, names = NULL) {

  obj <- deparse(substitute(u))
  env <- parent.frame() # parent.env(environment())

  for(lbl in names(u)) {
    u[[lbl]] <- rep(u[[lbl]], length.out = n)
    if(! is.null(names)) names(u[[lbl]]) <- names
    env[[lbl]] <- u[[lbl]]
  }

  suppressWarnings(rm(list = obj, pos = env))
}
