# =============================================================================.
# Function for internal use
# -----------------------------------------------------------------------------.
.resolve.legend.position <- function(pos) {
  p <- c(-1, 0, 1)
  p <- data.frame(
    id = c("bl", "b", "br", "l", "c", "r", "tl", "t", "tr"),
    name = "", x = rep(p, 3), y = p[gl(3, 3)], stringsAsFactors = F
  )
  p$name <- c(
    "bottomleft", "bottom", "bottomright", "left", "center", "right",
    "topleft", "top", "topright"
  )
  if(pos %in% p$id)   pos <- match(pos, p$id)
  if(pos %in% p$name) pos <- match(pos, p$name)
  if(! (is.numeric(pos) & length(pos) == 1 & pos > 0 & pos < 10)) {
    stop("Unknown pos value")
  }
  p <- p[pos,]
  p
}
