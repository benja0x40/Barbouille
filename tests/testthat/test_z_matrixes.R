# COMMON #######################################################################

# > matrixes ===================================================================
context("matrixes")

# + m2v ------------------------------------------------------------------------
test_that("m2v", {
  a <- m2v(1:3, 1:3, nrow = 3)
  b <- c(1, 5, 9)
  expect_equal(a, b)
  a <- m2v(3:1, 1:3, nrow = 3)
  b <- c(3, 5, 7)
  expect_equal(a, b)
})

# + v2m ------------------------------------------------------------------------
test_that("v2m", {
  a <- v2m(c(1, 5, 9), nrow = 3)
  b <- cbind(1:3, 1:3)
  expect_equal(a, b)
  a <- v2m(c(3, 5, 7), nrow = 3)
  b <- cbind(3:1, 1:3)
  expect_equal(a, b)
})

# + AddByRow -------------------------------------------------------------------
test_that("AddByRow", {
  v <- 5:1
  m <- matrix(1:50, 10, 5)
  r <- t(t(m) + v)
  expect_equal(AddByRow(m, v), r)
})

# + MulByRow -------------------------------------------------------------------
test_that("MulByRow", {
  v <- 5:1
  m <- matrix(1:50, 10, 5)
  r <- t(t(m) * v)
  expect_equal(MulByRow(m, v), r)
})

# SPECIFIC #####################################################################

# + RowSampler -----------------------------------------------------------------
test_that("RowSampler", {
  M <- matrix(0, 10, 5)

  i <- RowSampler(M)
  expect_identical(i, 1:10)

  i <- RowSampler(M, min = 10)
  expect_identical(i, 1:10)

  i <- RowSampler(M, max = 10)
  expect_identical(i, 1:10)

  i <- RowSampler(M, min = 100)
  expect_equal(length(i), 100)
  expect_true(max(i) <= nrow(M))
  expect_true(min(i) >= 1)

  i <- RowSampler(M, max = 5)
  expect_equal(length(i), 5)
  expect_true(max(i) <= nrow(M))
  expect_true(min(i) >= 1)
})

# + MetaSelect -----------------------------------------------------------------
test_that("MetaSelect", {

  meta <- data.frame(
    x = gl(2, 4, 8, labels = LETTERS[1:2]),
    y = gl(2, 2, 8, labels = LETTERS[1:2]),
    z = gl(2, 1, 8, labels = LETTERS[1:2])
  )
  rownames(meta) <- apply(meta, 1, paste, collapse = "")

  v <- list(u = rownames(meta)[c(TRUE, FALSE)], v = rownames(meta)[c(FALSE, TRUE)])
  r <- MetaSelect(meta, cols = v)
  expect_identical(r, v)

  v <- with(meta, rownames(meta)[y == "A"])
  r <- MetaSelect(meta, cols = list(y == "A"))
  expect_identical(r[[1]], v)

  v <- with(meta, rownames(meta)[x == "A" & z == "A"])
  r <- MetaSelect(meta, cols = list(x == "A" & z == "A"))
  expect_identical(r[[1]], v)

  v <- with(meta, rownames(meta)[x == "A" | z == "A"])
  r <- MetaSelect(meta, cols = list(x == "A" | z == "A"))
  expect_identical(r[[1]], v)

})

# + ExtractSelection -----------------------------------------------------------
test_that("ExtractSelection", {

  meta <- data.frame(
    antibody  = gl(2, 2, 8, labels = c("H3", "IgG")),
    genotype  = gl(2, 1, 8, labels = c("WT", "mutant")),
    replicate = gl(2, 4, 8, labels = c("R1", "R2"))
  )
  rownames(meta) <- apply(meta, 1, paste, collapse = "_")
  M <- matrix(1:80, 10, 8, dimnames = list(NULL, rownames(meta)))
  n <- ncol(M)

  v <- c(1:2, 5:6, 3:4, 7:8)
  a <- ExtractSelection(
    M, meta = meta, cols = list(
      x = antibody == "H3",
      y = antibody == "IgG"
    )
  )
  expect_identical(colnames(a), c(rep("x", 4), rep("y", 4)))
  expect_identical(as.vector(a[ 1, ]), as.vector(M[ 1, v]))
  expect_identical(as.vector(a[10, ]), as.vector(M[10, v]))

  idx <- seq(1, nrow(M), 3)
  a <- ExtractSelection(
    M, meta = meta, rows = idx, cols = list(
      x = antibody == "H3"  & genotype == "WT",
      y = c("IgG_WT_R1", "IgG_WT_R2")
    )
  )
  b <- ExtractSelection(
    M, meta = meta, rows = idx, cols = list(
      x = c("H3_WT_R1", "H3_WT_R2"),
      y = antibody == "IgG" & genotype == "WT"
    )
  )
  expect_identical(a, b)

  a <- ExtractSelection(
    M[, 1:n], meta = meta, cols = list(
      x = antibody == "H3"  & genotype == "WT",
      y = antibody == "IgG" & genotype == "WT"
    )
  )
  b <- ExtractSelection(
    M[, n:1], meta = meta, cols = list(
      x = antibody == "H3"  & genotype == "WT",
      y = antibody == "IgG" & genotype == "WT"
    )
  )
  expect_identical(a, b)

  a <- ExtractSelection(
    M, meta = meta[1:n, ], cols = list(
      x = antibody == "H3"  & genotype == "WT",
      y = antibody == "IgG" & genotype == "WT"
    )
  )
  b <- ExtractSelection(
    M, meta = meta[n:1, ], cols = list(
      x = antibody == "H3"  & genotype == "WT",
      y = antibody == "IgG" & genotype == "WT"
    )
  )
  expect_identical(a, b[, c(2, 1, 4, 3)])

})

# + ReCombine ------------------------------------------------------------------
test_that("ReCombine", {

  M <- matrix(1:20, 10, 2, dimnames = list(NULL, c("x", "y")))

  expect_error(
    ReCombine(M, f = list(A_Non_Existent_Column = "mean")),
    regexp = "column not found"
  )
  expect_error(
    ReCombine(M, f = list(x = "A_Non_Existent_Function")),
    regexp = "unknown function name"
  )

  for(fun in c("mean", "max", "min", "median")) {
    A <- ReCombine(M, f = list(x = fun))
    expect_identical(A, M)
    A <- ReCombine(M, f = list(y = fun))
    expect_identical(A, M)
  }
  for(fun in c("mad")) {
    A <- ReCombine(M, f = list(x = fun))
    expect_identical(A[, "x"], rep(0, nrow(A)))
    A <- ReCombine(M, f = list(y = fun))
    expect_identical(A[, "y"], rep(0, nrow(A)))
  }
  for(fun in c("var", "sd")) {
    A <- ReCombine(M, f = list(x = fun))
    expect_true(all(is.na(A[, "x"])))
    A <- ReCombine(M, f = list(y = fun))
    expect_true(all(is.na(A[, "y"])))
  }

  M <- matrix(1:60, 10, 6)
  colnames(M) <- c("x", "x", "x", "y", "y", "y")

  A <- ReCombine(M, f = list(x = "mean", y =  mean))
  B <- ReCombine(M, f = list(x =  mean,  y = "mean"))
  expect_identical(A, B)
  A <- ReCombine(M, f = list(x = "min", y =  max))
  B <- ReCombine(M, f = list(x =  min,  y = "max"))
  expect_identical(A, B)
  A <- ReCombine(M, f = list(x = "var", y =  sd))
  B <- ReCombine(M, f = list(x =  var,  y = "sd"))
  expect_identical(A, B)
  A <- ReCombine(M, f = list(x = "median", y =  mad))
  B <- ReCombine(M, f = list(x =  median,  y = "mad"))
  expect_identical(A, B)

  M <- matrix(1:40, 10, 4, dimnames = list(NULL, c("x", "x", "y", "y")))

  A <- ReCombine(M, f = c(x = "mean"))
  expect_identical(colnames(A), colnames(M)[-1])
  expect_identical(nrow(A), nrow(M))

  A <- ReCombine(M, f = c(y = "mean"))
  expect_identical(colnames(A), colnames(M)[-3])
  expect_identical(nrow(A), nrow(M))

  A <- ReCombine(M, f = list(x = "mean", y = mean))
  expect_identical(colnames(A), c("x", "y"))
  expect_identical(nrow(A), nrow(M))

  A <- ReCombine(M, f = list(x =  mean,  y = "mean"))
  expect_identical(colnames(A), c("x", "y"))
  expect_identical(nrow(A), nrow(M))

  A <- ReCombine(M, f = c(x = "merge"))
  expect_identical(colnames(A), colnames(M)[-1])
  expect_equal(nrow(A), 2 * nrow(M))

  A <- ReCombine(M, f = c(y = "merge"))
  expect_identical(colnames(A), colnames(M)[-3])
  expect_equal(nrow(A), 2 * nrow(M))

  A <- ReCombine(M, f = list(x = "merge", y = "merge"))
  expect_identical(colnames(A), c("x", "y"))
  expect_equal(nrow(A), 4 * nrow(M))

})

# NOT USED #####################################################################

# + Matrix2Table ---------------------------------------------------------------
test_that("Matrix2Table", {

  M <- matrix(1:30, 10, 3, dimnames = list(NULL, LETTERS[1:3]))
  g <- sample(1:2, nrow(M), replace = T)

  r <- Matrix2Table(M, grp = g)
  expect_identical(r$id, as.factor(rep(colnames(M), each = nrow(M))))
  expect_identical(r$grp, rep(g, ncol(M)))

  expect_identical(r$x[r$id == "A"], M[, "A"])
  expect_identical(r$x[r$id == "B"], M[, "B"])
  expect_identical(r$x[r$id == "C"], M[, "C"])
})

# + List2Dataframe -------------------------------------------------------------
test_that("List2Dataframe", {

  lst <- list(
    "x", 0, "u", 1,
    "y", 0, "v", 2,
    "z", 0, "w", 3
  )
  r <- List2Dataframe(lst, LETTERS[1:4])

  expect_true(is.data.frame(r))
  expect_identical(colnames(r), LETTERS[1:4])
  expect_identical(r$A, c("x", "y", "z")) # as.factor no longer needed
  expect_identical(r$C, c("u", "v", "w")) # as.factor no longer needed
  expect_identical(r$B, rep(0, 3))
  expect_equal(r$D, 1:3)
})
