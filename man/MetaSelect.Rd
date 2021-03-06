% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_matrixes.R
\name{MetaSelect}
\alias{MetaSelect}
\title{** RESERVED FOR INTERNAL USE **}
\usage{
MetaSelect(meta, cols, M = NULL)
}
\arguments{
\item{meta}{data.frame.}

\item{cols}{list.}

\item{M}{not implemented.}
}
\value{
\code{MetaSelect} returns a \code{list}.
}
\description{
Find column names from associated metadata.
}
\examples{

meta <- data.frame(
  x = gl(2, 4, 8, labels = LETTERS[1:2]),
  y = gl(2, 2, 8, labels = LETTERS[1:2]),
  z = gl(2, 1, 8, labels = LETTERS[1:2])
)
rownames(meta) <- apply(meta, 1, paste, collapse = "")

MetaSelect(meta, cols = list(y == "A"))
MetaSelect(meta, cols = list(x == "A" & z == "A"))
MetaSelect(meta, cols = list(x == "A" | z == "A"))
}
\keyword{internal}
