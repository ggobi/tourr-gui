\name{gui_image}
\alias{gui_image}
\title{Image Tour GUI...}
\usage{gui_image(data=ozone, ...)}
\description{Image Tour GUI                                   
Displays an Image Tour GUI}
\details{This GUI allows users to control a 1d image tour plot by simply moving and clicking their mouses. 
Tour Type radio buttons contains four different tour types. They are the Grand Tour, Little Tour, Local Tour and Guided Tour. We can 
choose one type a time. For the Guided Tour, we need to choose an index from the droplist to specify which particular search type is desired. 
default index would be holes. For tour type Guided(lda_pp) and Guided(pda_pp), we also need to specify class variable first, and the Guided(pda_pp) 
also controlled by another parameter, lambda. Lambda ranges from 0 to 1, with default at 0.02. A value of 0 will make the tour operate like Guided(lda_pp). 
higher dimensional data a value closer to 1 would be advised.
Speed slider can control the speed of the 1D tour. Simply dragging the mouse along the slider, changes the speed from slow to fast.
Pause check box allow users to pause the dynamic 1D tour and have a close examination on the details.
Apply button allows users to update the 1D tour, when it doesn't automatically update.
Quit button allows users to close thie GUI window.
Help button provides information about the tour and also what this GUI can do.
will pop up when the mouse is moved over the GUI, which give hints about the functionality of the different GUI elements.}
\author{Bei Huang\email{beihuang@iastate.edu}, Di Cook \email{dicook@iastate.edu}, and Hadley Wickham \email{hadley@rice.edu}}
\keyword{display_image}
\arguments{\item{data}{a 3d array, the first two dimensions are locations on a grid, and the 3rd dimension gives the observations to be mixed with the tour.defaults to ozone dataset}
\item{...}{other arguments passed on to \code{\link{animate}} and \code{\link{display_xy}}}}
\examples{\dontrun{gui_image(ozone)}}
