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
Combines all of the tour gui's into one, putting a separate
tab for each.
}
\examples{
\dontrun{gui_tour(flea)}
}
\author{
Bei Huang\email{beihuang@iastate.edu}, Di Cook
\email{dicook@iastate.edu}, and Hadley Wickham
\email{hadley@rice.edu}
}
\references{
Bei Huang, Dianne Cook, Hadley Wickham (2012).  tourrGui: A
gWidgets GUI for the Tour to Explore High-Dimensional Data
Using Low-Dimensional Projections. Journal of Statistical
Software, 49(6), 1-12.
\url{http://www.jstatsoft.org/v49/i06/}.
}
\keyword{display}

