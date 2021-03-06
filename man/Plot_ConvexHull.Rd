% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot_ConvexHull.R
\name{Plot_ConvexHull}
\alias{Plot_ConvexHull}
\title{Plot_ConvexHull}
\usage{
Plot_ConvexHull(
  xcoord,
  ycoord = NULL,
  border.line = TRUE,
  lcolor = "black",
  shade = FALSE,
  scolor = NULL,
  alpha.f = 0.5,
  ...
)
}
\arguments{
\item{xcoord}{vector of values defining the x-coordinates OR data frame of x
and y coordinates.}

\item{ycoord}{vector of values defining the y-coordinates. If missing, the
function will take the second column of the xcoord input.}

\item{border.line}{if TRUE, will include a solid line around the convex hull.}

\item{lcolor}{the color of the border line}

\item{shade}{if TRUE will shade in the convex hull. By default, the shade
color is the color of the border line (lcolor)}

\item{scolor}{Define the color of the shaded area}

\item{alpha.f}{defines transparency of the shaded area, by default 0.5.}
}
\value{
adds a convex hull as described, to the plot
}
\description{
Plot a convex hull around a group of specified points on a scatterplot
Useful for visualizing NMDS output.
}
\examples{
Plot_ConvexHull(nmds, border.line = TRUE, lcolor = 'red', shade = TRUE)

}
