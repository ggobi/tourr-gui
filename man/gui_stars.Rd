\name{gui_stars}
\alias{gui_stars}
\title{Stars Tour GUI
Displays an Stars Tour GUI}
\usage{
gui_stars(data = flea, ...)
}
\arguments{
  \item{data}{matrix, or data frame containing numeric
  columns, defaults to flea dataset}

  \item{...}{other arguments passed on to
  \code{\link{animate}} and \code{\link{display_xy}}}
}
\description{
  This GUI allows users to control the stars tour by simply
  moving and clicking their mouses. The Variable Selection
  checkboxes contains all the numeric variables, and at
  least three of them need to be checked to make the
  display work. All the categorical variables go to the
  Class Seclection box. We should select the class variable
  by double clicking the variable names. Color isn't
  implemented with the stars tour yet. The Tour Type radio
  buttons contains four different tour types. They are the
  Grand Tour, Little Tour, Local Tour and Guided Tour. We
  can only choose one type a time. For the Guided Tour, we
  need to choose an index from the droplist to specify
  which particular search type is desired. The default
  index would be holes. For tour type Guided(lda_pp) and
  Guided(pda_pp), we also need to specify class variable
  first, and the Guided(pda_pp) is also controlled by
  another parameter, lambda. Lambda ranges from 0 to 1,
  with default at 0.02. A value of 0 will make the tour
  operate like Guided(lda_pp). The Choose Dimension radio
  buttons allow us to choose the dimension number n to
  animate a nD tour path with Andrews' curves. Dimension n
  ranges from 3 to the total number of the numeric
  variables of this dataset. The maximum dimension is 18.
  The Choose Star Number slider allows users to specify how
  many stars to display. The number ranges from 2 to the
  number of observations in this dataset. Default star
  number is 4. The Speed slider can control the speed of
  the nD tour. Simply dragging the mouse along the slider,
  changes the speed from slow to fast. The Pause check box
  allow users to pause the dynamic nD tour and have a close
  examination on the details. The Apply button allows users
  to update the nD tour, when it doesn't automatically
  update. The Quit button allows users to close thie GUI
  window. The Help button provides information about the
  tour and also what this GUI can do. Tooltips will pop up
  when the mouse is moved over the GUI, which give hints
  about the functionality of the different GUI elements.
}
\examples{
\dontrun{
gui_stars(flea)
}
}
\author{
  Bei Huang\email{beihuang@iastate.edu}, Di Cook
  \email{dicook@iastate.edu}, and Hadley Wickham
  \email{hadley@rice.edu}
}
\keyword{display_stars}
