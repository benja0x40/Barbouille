plotImage <- function(m, x = NULL, y = NULL) {
  image(
    x = x,
    y = y,
    z = matrix(1:length(m), ncol(m), nrow(m)),
    col = t(m[nrow(m):1, ])
  )
}
