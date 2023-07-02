#' distmat
#'
#' The distmat function takes output from the \code{\link{distances_clust}} or
#' \code{\link{distances_no_clust}} function and coerces it into a distance matrix.
#'
#' @param distpairs an output data frame from distances_clust or distances_no_clust
#'
#' @param type a string either c("avg", "sum") tells the function which calculated
#' distance value to populate the matrix with.
#'
#' @return distance matrix of dimensions n x n where n is the number of teeth considered.
#' Row and column names are the numbers corresponding to each tooth in the original
#' dataset (columns 3 and 4 from output of the \code{\link{distances_clust}} and
#' \code{\link{distances_no_clust}} functions).
#'
#' @export


distmat<-function(distpairs, type='avg') {
   # Make Distance Matrix
   objs<-unique(c(distpairs[,3], distpairs[,4]))  #make a list of all the unique objects considered
   dist.mat<-matrix(data=0, nrow=length(objs), ncol=length(objs))  #matrix of 0-values of dimensions [objs x objs]

   if(type=='avg') {
      for (i in 1:length(distpairs[,3])) {
         dat<-as.numeric(distpairs[i,1:7])        # call the first row of the data frame and coerce it into numeric values
         dist.mat[dat[1], dat[2]]<-dat[6] # 6th column is dist.avg; [1] and [2] are matrix locations
         dist.mat[dat[2], dat[1]]<-dat[6]
      }
   }
   else if(type == 'sum') {
      for (i in 1:length(distpairs[,3])) {
         dat<-as.numeric(distpairs[i,1:7])        # call the first row of the data frame and coerce it into numeric values
         dist.mat[dat[1], dat[2]]<-dat[5] # 5th column is dist.sum; [1] and [2] are matrix locations
         dist.mat[dat[2], dat[1]]<-dat[5]
      }
   }

   rownames(dist.mat)<-objs
   colnames(dist.mat)<-objs
   return(dist.mat)
}
