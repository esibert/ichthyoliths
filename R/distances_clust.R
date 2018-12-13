#' distances_clust
#'
#' The distances_clust function is the heart of the ichthyoliths package. It
#' calculates the morphological disparity of teeth by summing the distances
#' between each character/trait for each pair of teeth in the dataset.
#' It relies on cluster computing, and is dependent on the doParallel package.
#' To run this calculation without using parallelization, use the
#' \code{\link{distances_no_clust}} function
#'
#' @import doParallel
#'
#' @import foreach
#'
#' @import iterators
#'
#' @param morph a data frame of the form returned by toothdat_cleanup
#'
#' @param traits a list of trait connectivity matices (for standard datasets
#' use data(traits) included in this package
#'
#' @param weights a vector of how much each trait is weighted in the analysis
#' can use data(weights) included in this package, or define your own.
#'
#' @param startCol a numeric value referring to the column which contains the first
#' character trait in the morph matrix. Allows for differeing numbers of columns
#' of metadata for each object
#'
#' @param endCol a numeric value referring to the column which contains the last
#' character trait in the morph matrix. If no value, this is assumed to be the last
#' column in the morph matrix.
#'
#' @param conTraits ######### ideally a character vector containing the column names
#' of the continuous traits to consider, or a numeric vector containing the column numbers. ########
#'
#' @param coresFree a numerical value denoting how many cores to leave free
#' on your machine when running in parallel. Defaults to 2.
#' Use detectCores() to determine the number of cores available on the computer
#' to use. The number of cores in the cluster created in this function is
#' detectCores()-coresFree
#'
#' @return Returns a dataframe of each pairwise comparison with the
#' following columns: \cr
#' "1" and "2" - the combination of teeth being considered \cr
#' "ToothA" and "ToothB", - the row number for each tooth from the original input dataset \cr
#' "dist.sum" - the summed distance between the two teeth \cr
#' "dist.avg" - the averaged distance between the two teeth \cr
#' "traits.length" - the number of traits actually considered in the comparison \cr
#' "objID.A" and "objID.B" - the unique identifiers for each tooth considered.
#'
#' @export

distances_clust<-function(morph, traits, weights, startCol = 7, endCol = NULL, contTraits = TRUE, coresFree=2) {
   #call traits distance matrices directly from the working directory
   if(missing(traits)) {
      traits<-data(traits)
   }

   #assign equal weight to each trait if no weights
   if(missing(weights)) {
      weights<-rep(1, length(traits)+3)
   }

  if(missing(endCol)) {
    endCol <- length(morph)
  }

   # Create matrix of just traits, so that names can be maintained in the function properly
   morph.mat<-morph[,startCol:endCol]  # startCol is the column to start pulling morphological trait data from

   # Create sets of pairs:
   species<-c(1:length(morph.mat[,4])) #doesn't matter what column this calls, b/c just giving integers 1-n
   combs.rows<-t(combn(species,2)) #2xn Matrix of all possible pairwise combinations of species
   toothID<-as.numeric(rownames(morph))
   objID<-as.character(morph$ID)
   combs.toothID<-t(combn(toothID,2))

   #Set up cores and cluster to run loop
   cores<-detectCores()
   cl<-makeCluster(cores-coresFree)  #detect the number of cores and leave some open
   registerDoParallel(cl)

   #Set up parallel loop:
   dat<-foreach(i=1:length(combs.rows[,1]), .combine='rbind') %dopar% { #for all pairwise comparisons, in parallel
      cc<-combs.rows[i,]  #Call the pairwise comparison to look at
      Traitscomb<-as.vector(c())     #create empty vector to fill with comparissons of whichever 2 species are specified for the loop

      for(j in 1:length(traits)) {   #For however many traits there are...
         w<-weights[j]              #trait weight value
         tt<-data.frame(traits[j])    #calls relevant trait matrix and turns it into dataframe for easier handling
         foo<-as.numeric(tt[morph.mat[cc[1],j], morph.mat[cc[2],j]]) #calls the distance value of trait[i] by looking up first Dent A's trait[i] and then Dent B's Trait[i]
         foo.w<-foo*w
         Traitscomb<-c(Traitscomb, foo.w)   #Puts the trait[i] distance value in the comparisson vector
      }


      if(contTraits == TRUE) {
        #add in continuous distances calculated based on **normalized** input vectors;
        #Must be last columns in trait matrix (cannot be mixed into the discrete traits)
        #Called: AR, LEN, WID

        if(morph$AR[cc[1]] == 0 ) { #| morph$AR[cc[2]] == 0 ) {
           Traitscomb<-Traitscomb } #else {
        else if(morph$AR[cc[2]] == 0) {
           Traitscomb<-Traitscomb }
        else {
           ar.dist<-dist(morph$AR) # distance calculation of aspect ratio distances for teeth considered in this analysis
           ar.dist<-ar.dist/max(ar.dist) #normalized
           ar<-ar.dist[i]*weights[length(traits)+1]
           Traitscomb<-c(Traitscomb, ar) }

        if(morph$LEN[cc[1]] == 0) {
           Traitscomb<-Traitscomb }
        else if(morph$LEN[cc[2]] == 0) {
           Traitscomb<-Traitscomb }
        else {
           len.dist<-dist(morph$LEN) # distance calculation of length differences for teeth considered in this analysis
           len.dist<-len.dist/max(len.dist) #normalized
           len<-len.dist[i]*weights[length(traits)+2]
           Traitscomb<-c(Traitscomb, len) }

        if(morph$WID[cc[1]] == 0) {
           Traitscomb<-Traitscomb }
        else if(morph$WID[cc[2]] == 0) {
           Traitscomb<-Traitscomb }
        else {
           wid.dist<-dist(morph$WID) # distance calculation of width differences for teeth considered in this analysis
           wid.dist<-wid.dist/max(wid.dist) #normalized
           wid<-wid.dist[i]*weights[length(traits)+3]
           Traitscomb<-c(Traitscomb, wid) }
      }

      Traitscomb<-unlist(Traitscomb)
      Traitscomb<-as.numeric(Traitscomb)
      Dist<-sum(Traitscomb)  #Adds up distances calculated in inner loop
      Dist.avg<-mean(Traitscomb) #Mean of distance between 2 species
      Num.traits<-length(Traitscomb)
      to.dat<-data.frame(cc[1], cc[2], combs.toothID[i,1], combs.toothID[i,2], Dist, Dist.avg, Num.traits, objID[cc[1]], objID[cc[2]])
      to.dat #output of foreach loop
   }

   #stop cluster and remove traces of it (otherwise the comptuer hangs)
   stopCluster(cl)
   rm(cl, cores)

   #Get final data table to put into distmat
   #col<-length(unlist(dat[1]))
   #dist.df<-data.frame(matrix(unlist(dat), ncol=col, byrow=T))
   dist.df<-data.frame(dat)
   names(dist.df)<-c("1", "2", "ToothA","ToothB","dist.sum","dist.avg", "traits.length", 'objID.A', 'objID.B')
   return(dist.df)
}
