# COMMON #######################################################################

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Convert matrix indices into vector indices such that:
#'
#' x <- m2v(i, j) => M[i, j] = M[x] for each pair of indices
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
m2v <- function(i, j, nrow) {
  (j - 1) * nrow + i
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Convert vector indices into matrix indices such that:
#'
#' ij <- v2m(x) => M[x] = M[ij[, 1], ij[, 2]] for each pair of indices
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
v2m <- function(x, nrow) {
  j <- (x - 1) %/% nrow + 1
  i <- (x - 1) %% nrow + 1
  x <- cbind(i, j)
  attributes(x) <- attributes(x)[1] # remove auto-generated dimnames
  x
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Add a column vector to a numeric matrix.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
AddByRow <- function(m, v) {
  m + rep(v, rep(dim(m)[1], dim(m)[2]))
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Multiply a numeric matrix by a column vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
MulByRow <- function(m, v) {
  m * rep(v, rep(dim(m)[1], dim(m)[2]))
}

# SPECIFIC #####################################################################

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Random sampling of row indices.
#'
#' @param M
#' matrix.
#'
#' @param min
#' minimum number of samples (default = 0).
#'
#' @param max
#' maximum number of samples (default = Inf).
#'
#' @return
#' \code{RowSampler} returns an integer vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
RowSampler <- function(M, min = 0, max = Inf) {
  n <- nrow(M)
  i <- 1:n
  if(n < min) i <- sample.int(n, size = min, replace = TRUE)
  if(n > max) i <- sample.int(n, size = max, replace = FALSE)
  i
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Find column names from associated metadata.
#'
#' @param meta
#' data.frame.
#'
#' @param cols
#' list.
#'
#' @param M
#' not implemented.
#'
#' @return
#' \code{MetaSelect} returns a \code{list}.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' meta <- data.frame(
#'   x = gl(2, 4, 8, labels = LETTERS[1:2]),
#'   y = gl(2, 2, 8, labels = LETTERS[1:2]),
#'   z = gl(2, 1, 8, labels = LETTERS[1:2])
#' )
#' rownames(meta) <- apply(meta, 1, paste, collapse = "")
#'
#' MetaSelect(meta, cols = list(y == "A"))
#' MetaSelect(meta, cols = list(x == "A" & z == "A"))
#' MetaSelect(meta, cols = list(x == "A" | z == "A"))
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
MetaSelect <- function(meta, cols, M = NULL) {

  cols <- lazyeval::lazy(cols)$expr

  cols <- eval(cols, envir = meta, enclos = parent.frame())
  chk  <- ! sapply(cols, is.character)
  cols[chk] <- lapply(cols[chk], function(x) rownames(meta)[x])

  cols
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Extract and rename matrix columns.
#'
#' @param M
#' matrix.
#'
#' @param cols
#' list.
#'
#' @param rows
#' optional.
#'
#' @param meta
#' optional data.frame.
#'
#' @return
#' \code{ExtractSelection} returns a \code{matrix}.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' meta <- data.frame(
#'   antibody  = gl(2, 2, 8, labels = c("H3", "IgG")),
#'   genotype  = gl(2, 1, 8, labels = c("WT", "mutant")),
#'   replicate = gl(2, 4, 8, labels = c("R1", "R2"))
#' )
#' rownames(meta) <- apply(meta, 1, paste, collapse = "_")
#' M <- matrix(1:80, 10, 8, dimnames = list(NULL, rownames(meta)))
#' n <- ncol(M)
#'
#' # Column selection by name or with logical expression on metadata
#' ExtractSelection(
#'   M, meta = meta, cols = list(
#'     x = antibody == "H3"  & genotype == "WT", y = c("IgG_WT_R1", "IgG_WT_R2")
#'   )
#' )
#' ExtractSelection(
#'   M, meta = meta, cols = list(
#'     x = c("H3_WT_R1", "H3_WT_R2"), y = antibody == "IgG" & genotype == "WT"
#'   )
#' )
#'
#' # Internal order of extracted columns is determined by metadata
#' ExtractSelection(
#'   M[, n:1], meta = meta[1:n, ], cols = list(
#'     x = antibody == "H3"  & genotype == "WT",
#'     y = antibody == "IgG" & genotype == "WT"
#'   )
#' )
#' ExtractSelection(
#'   M[, 1:n], meta = meta[n:1, ], cols = list(
#'     x = antibody == "H3"  & genotype == "WT",
#'     y = antibody == "IgG" & genotype == "WT"
#'   )
#' )
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ExtractSelection <- function(M, cols, rows = NULL, meta = NULL) {

  if(! is.null(meta)) cols <- MetaSelect(meta, cols)

  S <- c() # TODO: use matrix instead of cbind
  for(lbl in names(cols)) S <- cbind(S, M[, cols[[lbl]]])
  colnames(S) <- rep(names(cols), sapply(cols, length))

  if(! is.null(rows)) S <- S[rows, ]

  S
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Combine matrix columns.
#'
#' @param M
#' numeric matrix.
#'
#' @param f
#' function.
#'
#' @return
#' \code{ReCombine} returns a numeric \code{matrix}.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' M <- matrix(1:40, 10, 4, dimnames = list(NULL, c("x", "x", "y", "y")))
#'
#' ReCombine(M, f = c(x = "mean"))
#' ReCombine(M, f = c(y = "mean"))
#' ReCombine(M, f = list(x = "mean", y = mean))
#' ReCombine(M, f = list(x =  mean,  y = "mean"))
#' ReCombine(M, f = c(x = "merge"))
#' ReCombine(M, f = c(y = "merge"))
#' ReCombine(M, f = list(x = "merge", y = "merge"))
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ReCombine <- function(M, f = NULL) {

  replace <- function(M, V, chk) {
    i <- which(chk)[1]
    k <- which(! chk)
    M[, i] <- V
    k <- c(k[k < i], i, k[k > i])
    M[, k, drop = FALSE]
  }

  f <- ReferenceArgs(f, unique(colnames(M)))

  for(cln in names(f)) {

    if(is.null(f[[cln]])) next

    chk <- colnames(M) == cln
    ncl <- sum(chk)

    # Missing column
    if(ncl == 0) {
      err <- paste0("column not found ", cln, "\n")
      msg <- paste0("Available columns : ", paste(colnames(M), collapse = ", "))
      stop(err, msg)
    }

    # Multiple columns
    if(ncl >= 1) {
      if(is.function(f[[cln]])) { # User function
        V <- apply(M[, chk, drop = FALSE], MARGIN = 1, FUN = f[[cln]])
        M <- replace(M, V, chk)
      } else {
        lst <- c("merge", "min", "max", "mean", "sd", "var", "median", "mad")
        k <- match(f[[cln]], lst)
        if(is.na(k)) {
          err <- paste0("unknown function name ", f[[cln]], "\n")
          msg <- paste0("Accepted names : ", paste(lst, collapse = ", "))
          stop(err, msg)
        } else {
          k <- k - 1
          if(ncl == 1) { # Bypass computations
            if(k == 4) M <- replace(M, NA, chk)
            if(k == 5) M <- replace(M, NA, chk)
            if(k == 7) M <- replace(M,  0, chk)
          }
          if(ncl > 1) {
            if(k == 0) { # "merge"
              A <- c() # TODO: use matrix instead of rbind
              for(i in which(chk)) A <- rbind(A, replace(M, M[, i], chk))
              M <- A
            } else{
              V <- M[, chk, drop = FALSE]
              if(k == 1) V <- matrixStats::rowMins(V)
              if(k == 2) V <- matrixStats::rowMaxs(V)
              if(k == 3) V <- matrixStats::rowMeans2(V)
              if(k == 4) V <- matrixStats::rowSds(V)
              if(k == 5) V <- matrixStats::rowVars(V)
              if(k == 6) V <- matrixStats::rowMedians(V)
              if(k == 7) V <- matrixStats::rowMads(V)
              M <- replace(M, V, chk)
            }
          }
        }
      }
    }
  }

  M
}

# NOT USED #####################################################################

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Convert a multi-column numeric matrix into data.frame with two columns.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
Matrix2Table <- function(X, grp = NULL) {
  tbl <- data.frame(
    x   = as.vector(X),
    id  = gl(ncol(X), nrow(X), labels = colnames(X))
  )
  if(! is.null(grp)) tbl$grp <- grp
  tbl
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Convert a list into a data.table.
# -----------------------------------------------------------------------------.
#' @param x
#' list containing table values.
#'
#' @param key
#' name of the key column.
#'
#' @param columns
#' column names.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
List2DataTable <- function(x, key, columns, ...) {
  nc <- length(columns)
  nx <- length(x)
  nr <- nx / nc
  d <- list()

  for(k in columns) {
    j <- match(k, columns)
    d[[k]] <- unlist(x[(1:nr - 1) * nc + j])
  }
  d <- c(d, ...)
  d <- do.call(data.frame, d)
  colnames(d) <- columns
  data.table(d, key = key)
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Convert a list into a data.frame.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
List2Dataframe <- function(x, lbl) {
  nc <- length(lbl)
  nx <- length(x)
  nr <- nx / nc
  d <- list()

  for(k in lbl) {
    j <- match(k, lbl)
    d[[k]] <- unlist(x[(1:nr - 1) * nc + j])
  }
  d <- do.call(data.frame, d)
  colnames(d) <- lbl
  d
}
