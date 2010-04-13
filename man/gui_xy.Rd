\name{gui_xy}
\alias{gui_xy}
\title{Scatterplot Tour GUI...}
\usage{gui_xy(data=flea, ...)}
\description{Scatterplot Tour GUI
A graphical user interface enabling interactive control of a scatterplot tour.}
\details{GUI allows users to control the scatterplot tour by simply moving and clicking their mouses.
Variable Selection checkboxes contains all the numeric variables, and at least three of them need to be checked to make the display work.
the categorical variables go to the Class Seclection box. We should select the class variable by double clicking the variable names. 
users don't specify the class variable, the selected numeric variables will be considered as one class, and the default displaying color 
black. After users specify the class variable, the selected numeric variables will be considered as different classes according to this 
variable, and the scatterplot tour will have different rainbow colors.
Tour Type radio buttons contains four different tour types. They are the Grand Tour, Little Tour, Local Tour and Guided Tour. We can 
choose one type a time. For the Guided Tour, we need to choose an index from the droplist to specify which particular search type is desired. 
default index would be holes. For tour type Guided(lda_pp) and Guided(pda_pp), we also need to specify class variable first, and the Guided(pda_pp) 
also controlled by another parameter, lambda. Lambda ranges from 0 to 1, with default at 0.02. A value of 0 will make the tour operate like Guided(lda_pp). 
higher dimensional data a value closer to 1 would be advised.
Axes Locations column contains three types. Users can specify where tour axes will be displayed. The choices are center, bottomleft and off.
Speed slider can control the speed of the 2D tour. Simply dragging the mouse along the slider, changes the speed from slow to fast.
Pause check box allow users to pause the dynamic 2D tour and have a close examination on the details.
Apply button allows users to update the 2D tour, when it doesn't automatically update.
Quit button allows users to close thie GUI window.
Help button provides information about the tour and also what this GUI can do.
will pop up when the mouse is moved over the GUI, which give hints about the functionality of the different GUI elements.}
\author{Bei Huang\email{beihuang@iastate.edu}, Di Cook \email{dicook@iastate.edu}, and Hadley Wickham \email{hadley@rice.edu}}
\keyword{hplot}
\arguments{\item{data}{matrix, or data frame containing numeric columns, defaults to flea dataset}
\item{...}{other arguments passed on to \code{\link{animate}} and \code{\link{display_xy}}}}
\examples{\dontrun{gui_xy(flea)}}
