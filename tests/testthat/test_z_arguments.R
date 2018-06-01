# > Arguments ==================================================================
context("Arguments")

# + DefaultArgs -----------------------------------------------------------------
test_that("DefaultArgs", {

  cfg <- list(x = 1, y = 2, z = 3)

  src <- new.env()
  dst <- new.env()

  DefaultArgs(cfg, from = src)
  expect_identical(names(src), character(0))

  DefaultArgs(cfg, to = dst)
  expect_identical(as.list(dst), cfg)

  dst <- new.env()
  dst$y <- 0
  DefaultArgs(cfg, to = dst)
  expect_identical(as.list(dst), list(x = 1, y = 0, z = 3))

  alt <- list(x = 3, y = 2, z = 1)
  dst <- as.environment(alt)

  DefaultArgs(cfg, to = dst)
  expect_identical(as.list(dst, sorted = TRUE), alt)

  src <- new.env()
  src$y <- 0

  DefaultArgs(cfg, from = src, to = dst)
  expect_identical(as.list(dst, sorted = TRUE), alt)

  dst <- new.env()
  DefaultArgs(cfg, from = src, to = dst)
  expect_identical(as.list(dst), list(x = 1, y = 0, z = 3))

  f <- function(x = NULL, y = NULL, z = NULL, ...) {
    DefaultArgs(cfg)
    list(x = x, y = y, z = z)
  }

  expect_identical(f(), cfg)
  expect_identical(f(x = 0)$x, 0)
  expect_identical(f(y = 0)$y, 0)
  expect_identical(f(z = 0)$z, 0)
  expect_identical(f(i = 1:10), cfg)

  f <- function(x = NULL, y = NULL, z = NULL, ...) {
    DefaultArgs(cfg, ignore = "...", from = f)
    list(x = x, y = y, z = z)
  }

  expect_identical(f(), cfg)
  expect_identical(f(x = 0)$x, 0)
  expect_identical(f(y = 0)$y, 0)
  expect_identical(f(z = 0)$z, 0)
  expect_identical(f(i = 1:10), cfg)

})

# + VectorArgs ------------------------------------------------------------------
test_that("VectorArgs", {

  x <- 0
  y <- 1:10

  VectorArgs(c("x", "y"))
  expect_identical(x, rep(0, 10))
  expect_identical(y, 1:10)

  VectorArgs(c("x", "y"), size = 15)
  expect_identical(x, rep(0, 15))
  expect_identical(y[11:15], 1:5)

  a <- list(x = 0, y = 1:10)

  r <- VectorArgs(c("x", "y"), from = a)
  expect_identical(r$x, rep(0, 10))
  expect_identical(r$y, 1:10)

  r <- VectorArgs(c("x", "y"), from = a, size = 15)
  expect_identical(r$x, rep(0, 15))
  expect_identical(r$y[11:15], 1:5)

})


# + ClonalArg ------------------------------------------------------------------
test_that("ClonalArg", {

  a <- c("x", "y")

  d <- "i"
  v <- "j"
  r <- ClonalArg(u = v, a, d)
  expect_identical(r, list(x = v, y = v))

  d <- LETTERS[1:5]
  v <- LETTERS[5:1]
  r <- ClonalArg(u = v, a, d)
  expect_identical(r, list(x = v, y = v))

  d <- matrix("I", 2, 2)
  v <- matrix("J", 2, 2)
  r <- ClonalArg(u = v, a, d)
  expect_identical(r, list(x = v, y = v))

  d <- 1
  v <- 0
  r <- ClonalArg(u = 0, a, d)
  expect_identical(r, list(x = v, y = v))

  r <- ClonalArg(u = c(x = 0), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = c(y = 0), a, d)
  expect_identical(r, list(x = d, y = v))

  r <- ClonalArg(u = c(x = v, y = v), a, d)
  expect_identical(r, list(x = v, y = v))

  r <- ClonalArg(u = list(x = v), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = list(y = v), a, d)
  expect_identical(r, list(x = d, y = v))

  r <- ClonalArg(u = list(x = v, y = v), a, d)
  expect_identical(r, list(x = v, y = v))

  d <- 1:2
  v <- c(0, 0)

  r <- ClonalArg(u = 0, a, d)
  expect_identical(r, list(x = v, y = v))

  r <- ClonalArg(u = c(x = 0), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = c(y = 0), a, d)
  expect_identical(r, list(x = d, y = v))

  r <- ClonalArg(u = list(x = v), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = list(y = v), a, d)
  expect_identical(r, list(x = d, y = v))

  d <- matrix(1, 2, 2)
  v <- matrix(0, 2, 2)

  r <- ClonalArg(u = 0, a, d)
  expect_identical(r, list(x = v, y = v))

  r <- ClonalArg(u = c(x = 0), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = c(y = 0), a, d)
  expect_identical(r, list(x = d, y = v))

  r <- ClonalArg(u = list(x = v), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = list(y = v), a, d)
  expect_identical(r, list(x = d, y = v))

})

# Barbouille ###################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
elm_eq <- function(x, v) {
  all(sapply(x, all.equal, current = v, check.attributes = FALSE))
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
default <- function(n, r) {
  v <- vector("list", n)
  names(v) <- r
  v
}

# + AtomicArgs -----------------------------------------------------------------
test_that("AtomicArgs", {

  a <- list(x = "a", y = "b", z = "c")

  v <- a
  r <- AtomicArgs(u = NULL, a)
  expect_identical(r, v)

  v[] <- "d"
  r <- AtomicArgs(u = "d", a)
  expect_identical(r, v)

  v <- a
  v$y <- "d"
  r <- AtomicArgs(u = c(y = "d"), a)
  expect_identical(r, v)
  r <- AtomicArgs(u = list(y = "d"), a)
  expect_identical(r, v)

  v <- a
  v[c(1, 3)] <- "d"
  r <- AtomicArgs(u = c(x = "d", z = "d"), a)
  expect_identical(r, v)
  r <- AtomicArgs(u = list(x = "d", z = "d"), a)
  expect_identical(r, v)

  a <- list(x = 0, y = 0, z = 0)
  m <- matrix(1:4, 2, 2)

  v <- list(x = m, y = m, z = m)
  r <- AtomicArgs(u = m, a)
  expect_identical(r, v)

  v <- list(x = m, y = 0, z = 0)
  r <- AtomicArgs(u = list(x = m), a)
  expect_identical(r, v)

  v <- list(x = 0, y = m, z = 0)
  r <- AtomicArgs(u = list(y = m), a)
  expect_identical(r, v)

  v <- list(x = 0, y = 0, z = m)
  r <- AtomicArgs(u = list(z = m), a)
  expect_identical(r, v)
})

# + ReferenceArgs --------------------------------------------------------------
test_that("ReferenceArgs", {
  n <- 5
  r <- letters[1:n]

  a <- ReferenceArgs(u = NULL, r)
  expect_identical(names(a), r)

  v <- 1
  a <- ReferenceArgs(u = v, r)
  expect_identical(names(a), r)
  expect_true(elm_eq(a, v))

  v <- 1:2
  a <- ReferenceArgs(u = v, r)
  expect_identical(names(a), r)
  expect_true(elm_eq(a, v))

  v <- matrix(0, 2, 2)
  a <- ReferenceArgs(u = v, r)
  expect_identical(names(a), r)
  expect_true(elm_eq(a, v))

  v <- default(n, r)
  v$b <- 1
  a <- ReferenceArgs(u = c(b = 1), r)
  expect_identical(a, v)

  v <- default(n, r)
  v$d <- 1
  a <- ReferenceArgs(u = list(d = 1), r)
  expect_identical(a, v)

  v <- default(n, r)
  v[c("b", "d")] <- c(1, 2)
  a <- ReferenceArgs(u = c(b = 1, d = 2), r)
  expect_identical(a, v)

  v <- default(n, r)
  v[c("b", "d")] <- list(1:2, 3:4)
  a <- ReferenceArgs(u = list(b = 1:2, d = 3:4), r)
  expect_identical(a, v)

  v <- default(n, r)
  v$z <- 0
  a <- ReferenceArgs(u = list(z = 0), r)
  expect_identical(a, v)
})

# + ComposeArgs ----------------------------------------------------------------
test_that("ComposeArgs", {

  def <- list(
    a = list(x = 1, y = 2, b = c(0, 0), s = list(x = 0, y = 0)),
    r = list(
      f = list(a = list(x = "a", y = "b")),
      m = list(r = c("x", "y", "f"), n = 5)
    )
  )

  r <- ComposeArgs(NULL, def)
  expect_identical(r$x, 1)
  expect_identical(r$y, 2)
  expect_identical(r$b, c(0, 0))
  expect_identical(r$s$x, 0)
  expect_identical(r$s$y, 0)
  expect_identical(r$f$x, "a")
  expect_identical(r$f$y, "b")
  expect_true(length(r$m) == 5)

  usr <- list(b = c(5, 5))
  r <- ComposeArgs(usr, def)
  expect_identical(r$b, c(5, 5))

  usr <- list(s = 5)
  r <- ComposeArgs(usr, def)
  expect_identical(r$s$x, 5)
  expect_identical(r$s$y, 5)

  usr <- list(f = ".")
  r <- ComposeArgs(usr, def)
  expect_identical(r$f$x, ".")
  expect_identical(r$f$y, ".")

  usr <- list(f = c(x = "+", y = "-"))
  r <- ComposeArgs(usr, def)
  expect_identical(r$f$x, "+")
  expect_identical(r$f$y, "-")

  M <- matrix(1:4, 2, 2)
  usr <- list(
    x = M,
    m = list(
      list(y = 1:2, f = c(x = "A")),
      list(x = 3:4, f = list(y = "B"))
    )
  )
  r <- ComposeArgs(usr, def)
  expect_identical(r$x, M)
  expect_identical(r$y, 2)
  expect_identical(r$b, c(0, 0))
  expect_identical(r$s$x, 0)
  expect_identical(r$s$y, 0)
  expect_identical(r$f$x, "a")
  expect_identical(r$f$y, "b")
  for(i in c(1, 3, 5)) {
    expect_identical(r$m[[i]]$x, M)
    expect_identical(r$m[[i]]$y, 1:2)
    expect_identical(r$m[[i]]$f$x, "A")
    expect_identical(r$m[[i]]$f$y, "b")
  }
  for(i in c(2, 4)) {
    expect_identical(r$m[[i]]$x, 3:4)
    expect_identical(r$m[[i]]$y, 2)
    expect_identical(r$m[[i]]$f$x, "a")
    expect_identical(r$m[[i]]$f$y, "B")
  }

  def <- list(
    a = list(
      x = 1, y = 2, b = c(0, 0), s = list(x = 0, y = 0),
      f = list(x = "a", y = "b")
    ),
    r = list(m = list(r = c("x", "y", "f"), n = 5))
  )
  r <- ComposeArgs(usr, def)
  expect_identical(r$x, M)
  expect_identical(r$y, 2)
  expect_identical(r$b, c(0, 0))
  expect_identical(r$s$x, 0)
  expect_identical(r$s$y, 0)
  expect_identical(r$f$x, "a")
  expect_identical(r$f$y, "b")
  for(i in c(1, 3, 5)) {
    expect_identical(r$m[[i]]$x, M)
    expect_identical(r$m[[i]]$y, 1:2)
    expect_identical(r$m[[i]]$f$x, "A")
    expect_identical(r$m[[i]]$f$y, "b")
  }
  for(i in c(2, 4)) {
    expect_identical(r$m[[i]]$x, 3:4)
    expect_identical(r$m[[i]]$y, 2)
    expect_identical(r$m[[i]]$f$x, "a")
    expect_identical(r$m[[i]]$f$y, "B")
  }

})

# + AssignArgs -----------------------------------------------------------------
test_that("AssignArgs", {

  def <- list(
    a = list(x = 1, y = 2, b = c(0, 0), s = list(x = 0, y = 0)),
    r = list(
      f = list(a = list(x = "a", y = "b")),
      m = list(r = c("x", "y", "f"), n = 5)
    )
  )
  M <- matrix(1:4, 2, 2)
  x <- y <- b <- s <- f <- m <- NULL
  usr <- list(
    x = M,
    m = list(
      list(y = 1:2, f = c(x = "A")),
      list(x = 3:4, f = list(y = "B"))
    )
  )
  AssignArgs(usr, def)

  expect_identical(x, M)
  expect_identical(y, 2)
  expect_identical(b, c(0, 0))
  expect_identical(s$x, 0)
  expect_identical(s$y, 0)
  expect_identical(f$x, "a")
  expect_identical(f$y, "b")
  for(i in c(1, 3, 5)) {
    expect_identical(m[[i]]$x, M)
    expect_identical(m[[i]]$y, 1:2)
    expect_identical(m[[i]]$f$x, "A")
    expect_identical(m[[i]]$f$y, "b")
  }
  for(i in c(2, 4)) {
    expect_identical(m[[i]]$x, 3:4)
    expect_identical(m[[i]]$y, 2)
    expect_identical(m[[i]]$f$x, "a")
    expect_identical(m[[i]]$f$y, "B")
  }
})
