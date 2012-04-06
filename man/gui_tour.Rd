\name{gui_tour}
\alias{gui_tour}
\title{Tour GUI
Displays all types of Tour GUI in different tabs}
\usage{
gui_tour(data = flea, ...)
}
\arguments{
  \item{data}{matrix, or data frame containing numeric
  columns, defaults to flea dataset}

  \item{...}{other arguments passed on to
  \code{\link{animate}} and \code{\link{display_xy}}}
}
\description{
  Combines all of the tour gui's into one, putting a
  separate tab for each.
}
\examples{
\dontrun{
gui_tour(flea)
}
}
\author{
  Bei Huang\email{beihuang@iastate.edu}, Di Cook
  \email{dicook@iastate.edu}, and Hadley Wickham
  \email{hadley@rice.edu}
}
\keyword{display}
