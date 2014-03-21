\name{gui_image}
\alias{gui_image}
\title{Image Tour GUI
Displays an Image Tour GUI}
\usage{
gui_image(data = ozone, ...)
}
\arguments{
  \item{data}{a 3d array, the first two dimensions are
  locations on a grid, and the 3rd dimension gives the
  observations to be mixed with the tour.defaults to ozone
  dataset}

  \item{...}{other arguments passed on to
  \code{\link{animate}} and \code{\link{display_xy}}}
}
\description{
This GUI allows users to control an image tour plot by
simply moving and clicking their mouses. The Tour Type
radio buttons contains four different tour types. They are
the Grand Tour, Little Tour, Local Tour and Guided Tour. We
can only choose one type a time. For the Guided Tour, we
need to choose an index from the droplist to specify which
particular search type is desired. The default index would
be holes. The Speed slider can control the speed of the 1D
tour. Simply dragging the mouse along the slider, changes
the speed from slow to fast. The Pause check box allow
users to pause the dynamic 1D tour and have a close
examination on the details. The Apply button allows users
to update the 1D tour, when it doesn't automatically
update. The Quit button allows users to close thie GUI
window. The Help button provides information about the tour
and also what this GUI can do. Tooltips will pop up when
the mouse is moved over the GUI, which give hints about the
functionality of the different GUI elements.
}
\examples{
\dontrun{gui_image(ozone)}
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
\keyword{display_image}

