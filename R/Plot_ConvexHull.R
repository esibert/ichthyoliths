#' Plot_ConvexHull
#'
#' Plot a convex hull around a group of specified points on a scatterplot
#' Useful for visualizing NMDS output.
#'
#' @param xcoord vector of values defining the x-coordinates OR data frame of x
#' and y coordinates.
#'
#' @param ycoord vector of values defining the y-coordinates. If missing, the
#' function will take the second column of the xcoord input.
#'
#' @param border.line if TRUE, will include a solid line around the convex hull.
#'
#' @param lcolor the color of the border line
#'
#' @param shade if TRUE will shade in the convex hull. By default, the shade
#' color is the color of the border line (lcolor)
#'
#' @param scolor Define the color of the shaded area
#'
#' @param alpha.f defines transparency of the shaded area, by default 0.5.
#'
#' @return adds a convex hull as described, to the plot
#'
#' @examples
#' Plot_ConvexHull(nmds, border.line = TRUE, lcolor = 'red', shade = TRUE)
#'
#' @export


Plot_ConvexHull<-function(xcoord, ycoord = NULL, border.line=TRUE, lcolor = 'black',
   shade=FALSE, scolor=NULL, alpha.f = 0.5, ...){
   if(missing(ycoord)) {
      ycoord<-xcoord[,2]
      xcoord<-xcoord[,1]
   }
   hpts <- chull(x = xcoord, y = ycoord)
   hpts <- c(hpts, hpts[1])
   if(border.line==T) {
      lines(xcoord[hpts], ycoord[hpts], col = lcolor, ...)
   }
   if(shade==T) {
      if(missing(scolor)) {scolor = lcolor}
      if(missing(alpha.f)) {alpha.f = 0.5}
      polygon(xcoord[hpts], ycoord[hpts], col=adjustcolor(scolor, alpha.f=alpha.f), border=NA)
   }
}
