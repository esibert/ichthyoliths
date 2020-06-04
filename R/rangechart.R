#' rangechart
#'
#' The rangechart function takes an input table or matrix of ages (rows) and taxa (columns),
#' populated by occurrance or count values. This function does not create a legend, but as
#' it creates an active graphics device, you can add one based on the parameters defined here.
#'
#' @import viridis
#'
#' @param counts is the table, matrix, or data.frame of occurrances for each taxa (columns)
#' by age (rows). NOTE: Rows and Columns MUST have names for this to work, unless ages is specified.
#'
#' @param ages is a vector giving the age for each sample depth. Default is to pull rownames of counts
#'
#' @param taxa is a vector giving the name for each taxa. Default is to pull column-names of counts
#'
#' @param tax.cat is an optional numerical vector that splits the taxa into categories or groups.
#' Must be equal to the length of the taxa vector. Must be numeric. Default is NULL
#'
#' @param reorder is an optional command that allows the reordering of taxa to show extinction or
#' origination. It currently has two options:
#'
#' a) 'fad.by.lad': Sort first by First Appearence Datum (FAD) then by Last Appearence Datum (LAD);
#' Highlights orrigination
#'
#' b) 'lad.by.fad': Sort first by Last Appearence Datum (LAD) then by First Appearence Datum (FAD)
#' Highlights extinctions.
#'
#'
#' @param normalize.counts is a logical. When TRUE, the counts matrix is recalculated to
#' represent each taxa's occurrance counts as a percentage for each sample (age).
#' When FALSE, the raw count values are used. Default is FALSE.
#'
#' @param count.breaks is a numerical input vector which gives the breaks between grouped
#' count values, as whole integer numbers. These can be percentages if normalize.counts=TRUE
#' or count values if normalize.counts = FALSE. If left blank, defaults to NULL. Note that
#' vector should take the form c(0, breakpoints). An example of this is
#' count.breaks = c(0, 2, 5, 10) will yield groups of <2%, 2-5%, 5-10%, and 10+%, or
#' raw counts of 1-2, 2-5, 5-10, and 10+.
#'
#' @param cex.xaxis is a single numerical value scaling the x-axis labels
#'
#' @param cex.yaxis is a single numerical value scaling the y-axis labels
#'
#' @param yaxis.ticks is a logical. When TRUE, small ticks are added to the y-axis for
#' each sample's precise age. When FALSE, these ticks are not included.
#'
#' @param llwd is the lwd (line width) parameter passed to the line segments connecting
#' the first and last occurrances points for each taxa.
#'
#' @param llcol is the col (line color) parameter passed to the line segments connecting
#' the first and last  occurrances points for each taxa.
#'
#' @param llty is the lty (line type) parameter passed to the line segments connecting
#' the first and last  occurrances points for each taxa.
#'
#' @param baselines is a logical determining whether to draw line connecting the first
#' occurrance point to the x-axis. Default is FALSE (no baselines).
#'
#' @param blwd is the lwd (line width) parameter passed to the baselines segment function.
#'
#' @param blcol is the col (line color) parameter passed to the baselines segment function.
#'
#' @param blty is the lty (line type) parameter passed to the baselines segment function.
#'
#' @param col.points tells the function what color scheme to use:
#'
#' a) specify a single color for all of the points. Do not specify a cols.vec
#'
#' b) 'by.count' - the color will change based on the number or percentage of objects
#' in each age bin. Must specify a cols.vec.
#'
#' c) 'by.category' - the color will change based on the tax.cat vector. Must specify a cols.vec
#'
#'
#' @param cols.vec is the color vector used to color the points. This can be internally defined
#' using a pre-existing color pallette (currently can use 'rainbow' from base or 'viridis' if
#' you have that package installed). If NULL and col.points is unspecified, the default color
#' of the points is gray70.
#'
#' This can also be a user-defined vector of colors, but must be the length of the maximum
#' count value (if no count.breaks specified) or the number of groups defined by count.breaks
#' or tax.cat, if 'by.category' is selected in col.points. If insufficient colors are specified,
#' not all points will be plotted on the figure
#'
#' @param pch.points tells the function what point symbol to use:
#'
#' a) can be a single value defining the symbol for all points. Do not specify a pch.vec
#'
#' b) 'by.count' - the point symbol will change based on the number or percentage of objects.
#' Must specify a pch.vec
#'
#' c) 'by.category' - the point symbol will change based on the tax.cat vector. Must speciy a pch.vec
#'
#'
#' @param pch.vec is a vector used to define point shape. Must be the length of the number
#' of groups defined by count.breaks or tax.cat depending on what is selected for pch.points.
#'
#' @param cex.points tells the function what point size to use:
#'
#' a) can be a single value for point size, default is 1
#'
#' b) 'by.count' - the size of the points will vary based on the number or percentage of objects.
#' Scales with count values.
#'
#' c) 'by.category' - the size of points will be related to the category number specified in tax.cat
#'
#'
#' @param largesize is a scaling value to increase or decrease the size of the points.
#' Useful for scaling up large range charts. Default is 1.
#'
#' @param xaxis.labels defines the values that will be put on the x-axis to denote taxa.
#'
#' a) 'names' means that the names of the types will be the x-axis labels
#'
#' b) 'numeric' means that they will be numbered 1-n on the x-axis.
#'
#' c) 'alphanum' means that the original taxa order is preserved, and the numbers on
#' the x-axis refer to the original order of taxa in the counts matrix.
#'
#'
#' @param print.xaxis is a logical. If print.axis = TRUE, the function will return a list
#' of the corresponding morphotype names for each x-axis numeric value in the console.
#'
#' @return Returns a range chart style plot summarizing the counts table
#'
#' @export

rangechart <- function(counts, ages = NULL, taxa = NULL, tax.cat = NULL, reorder = NULL,
                        normalize.counts = FALSE, count.breaks = NULL,
                        cex.xaxis = 1, cex.yaxis = 1, yaxis.ticks = FALSE,
                        llwd = 1, llcol = 'gray70', llty = 3,
                        baselines = FALSE, blwd = 0.5, blcol = 'lightblue', blty = 3,
                        col.points = 'gray70', cols.vec = NULL,
                        pch.points = 16, pch.vec = NULL,
                        cex.points = 1, largesize = 1,
                        xaxis.labels = c('names', 'numeric', 'alphanum'), print.xaxis = FALSE, ...) {

   ##### set up the dataset #####
   # turn a data.frame into a matrix if necessary.
   if(class(counts) == 'data.frame') {
      counts <- as.matrix(counts)
   }


   # Ages should be rownames of the counts table, and in increasing order
   if(missing(ages)) {
      ages <- as.numeric(rownames(counts))
   }
   else {
      rownames(counts) <- ages
   }

   # if the ages are not in increasing order, sort them and the counts table to be so
   if(is.unsorted(ages) == TRUE) {
      age.increasing <- sort(ages, index.return = TRUE)$ix
      counts <- counts[age.increasing, ]
      ages <- as.numeric(rownames(counts))
   }

   # taxa should be column-names of the counts table, order doesn't matter.
   if(missing(taxa)) {
      taxa <- as.character(colnames(counts))
   }
   else {
      colnames(counts) <- taxa
   }

   original.taxa <- taxa #useful for matching tax-cat later too.


   # clear NA values, if any, by replacing with zeros
   if (sum(is.na(counts)) > 0) {
      warning(paste(sum(is.na(counts)), "missing values in count matrix replaced with zeros"))
      counts[is.na(counts)] <- 0
   }

   # FAD: First (oldest) occurance datum calls the maximum index (mapped to the ages values)
   # of a non-zero count value for each taxa column in the counts matrix
   fad <- ages[apply(counts, 2, function(x) {max(which (x!=0))})]

   # LAD: Last (youngest) occurance datum calls the minimum index (mapped to the ages values)
   # of a non-zero count value for each taxa column in the counts matrix
   lad <- ages[apply(counts, 2, function(x) {min(which (x!=0))})]


   ### Normalize the counts if they want to be normalized
   if(normalize.counts == TRUE) {
      norm.row<-function(row) {
         row/sum(row)
      }
      norm.counts<-apply(counts, 1, norm.row)  #normalize the matrix by rows
      counts<-t(norm.counts)   #for some reason I have to transpose the output back to the normal "counts" form
      counts <- 100 * counts #make this a percentage instead of a decimal value.
   }

   ### Group the counts into bins if you'd like them to be binned.
   if(!is.null(count.breaks)) {
      count.breaks <- c(count.breaks, max(counts)+1)

      for(i in 1:length(count.breaks)-1) {
         counts[counts > count.breaks[i] & counts <= count.breaks[i+1] ] = i
      }

   }

   ##### reorder counts #####
   if(!is.null(reorder)) {

      # fad.by.lad (origination)
      if(reorder == 'fad.by.lad') {
         # First reorder the counts by LAD and recalculate FAD
         reorder.vect <- sort(lad, decreasing = TRUE, index.return = TRUE)$ix #pulls index of order by fads
         counts <- counts[, reorder.vect]
         fad <- ages[apply(counts, 2, function(x) {max(which (x!=0))})]

         # Next, reorder the counts by FAD
         reorder.vect <- sort(fad, decreasing = TRUE, index.return = TRUE)$ix #pulls index of order by fads
         counts <- counts[, reorder.vect]

      }

      # lad.by.fad (extinction)
      else if(reorder == 'lad.by.fad') {
         # First reorder the counts by FAD and recalculate LAD
         reorder.vect <- sort(fad, decreasing = TRUE, index.return = TRUE)$ix #pulls index of order by fads
         counts <- counts[, reorder.vect]
         lad <- ages[apply(counts, 2, function(x) {min(which (x!=0))})]

         # Next, reorder the counts by LAD
         reorder.vect <- sort(lad, decreasing = TRUE, index.return = TRUE)$ix #pulls index of order by fads
         counts <- counts[, reorder.vect]
      }
   }


   ### re-generate 'taxa', 'ages', 'fad' and 'lad' and 'tax.cat' from the updated counts table

   taxa <- as.character(colnames(counts))
   ages <- as.numeric(rownames(counts))
   fad <- ages[apply(counts, 2, function(x) {max(which (x!=0))})]
   lad <- ages[apply(counts, 2, function(x) {min(which (x!=0))})]

   if(!is.null(tax.cat)) { tax.cat <- tax.cat[match(taxa, original.taxa)] }

   ##### set up the graphical parameters #####
   ## xaxis.labels
   if(missing(xaxis.labels)) { xaxis.labels <- 'names' }
   if(xaxis.labels == 'names') { xaxis.lab <- taxa }
   if(xaxis.labels == 'numeric') { xaxis.lab <- 1:length(taxa) }
   if(xaxis.labels == 'alphanum') { xaxis.lab <- match(taxa, original.taxa) }

   ## colors of points
   if(missing(cols.vec)) {cols.vec = 'gray'}

   if(cols.vec[1] == 'rainbow') {
      colors<-rainbow(max(counts), end=5/6)
   }
   else if (cols.vec[1] == 'viridis') {
      colors <- viridis::viridis(max(counts))
   }
   else colors<-cols.vec

   ## sizes of points (and scale of the whole thing...)
   sizes<-seq(0,(max(counts)-1), 1)
   sizes<-((sizes/(max(sizes)/largesize)) + largesize)


   ##### Actually make the plot #####
   ### make blank plot with appropriate dimensions, suppress x- and y- axes
   plot(1:ncol(counts), ylim = c(max(ages), min(ages)),
        type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "Age (Ma)", ...)

   ### add segments to the plot
   segments(1:ncol(counts), lad, 1:ncol(counts), fad,
            lwd = llwd, col = llcol, lty = llty, ...)

   if (baselines == TRUE) {
      segments(1:ncol(counts), fad, 1:ncol(counts), rep(par()$usr[3], ncol(counts)),
               col = blcol, lty = blty, lwd = blwd, ...)
   }

   ### Add points to the plot
   for (i in 1:ncol(counts)) {
      num.val<-c(counts[,i][counts[,i]>0])

      # point characters
      if (pch.points == 'by.count') {
         pts.pch<-pch.vec[num.val]
      }
      else if (pch.points == 'by.category') {
         if(class(tax.cat)=='factor') {tax.cat <- as.numeric(tax.cat)}
         pts.pch <- pch.vec[tax.cat[i]]
      }
      else { pts.pch<-pch.points }

      # point colors
      if (col.points == 'by.count') {
         pts.cols<-c(colors[num.val])
      }
      else if (col.points == 'by.category') {
         pts.cols <- colors[tax.cat[i]]
      }
      else { pts.cols<-col.points }

      # point size
      if (cex.points == 'by.count') {
         pts.cex<-c(sizes[num.val])
      }
      else if(cex.points == 'by.category') {
         pts.cex <- sizes[tax.cat[i]]
      }
      else { pts.cex<-cex.points }

      # point y-values
      plocs <- ages[(counts > 0)[, i]]

      #actually add the points
      points(rep(i, length(plocs)), plocs, cex=pts.cex, pch=pts.pch, col=pts.cols,
             ...)
   }

   ### add axes
   axis(1, at = 1:ncol(counts), cex.axis = cex.xaxis, labels = xaxis.lab,
        las = 3)
   axis(2, las = 1, cex.axis = cex.yaxis)
   if(yaxis.ticks == TRUE) {axis(2, at = ages, labels = FALSE, tck = -0.01)}


   ##### print taxa in list #####
   # this has to be the last thing that the function does, because R stops after a return value
   if(print.xaxis == TRUE) {
      if(xaxis.labels == 'alphanum') {
         return(original.taxa)
      }
      else {
         return(taxa)
      }
   }

}
