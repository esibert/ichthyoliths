#' distances_no_clust
#'
#' The distances_no_clust function is a variation on the distances_clust function,
#' the heart of the ichthyoliths package. It calculates the morphological disparity
#' of teeth by summing the distances between each character/trait for each pair
#' of teeth in the dataset. This function relies on the foreach loop, but does
#' \strong{NOT} create a cluster or run the loop in parallel, unlike
#' \code{\link{distances_clust}}.
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

distances_no_clust<-function(morph, traits, weights) {
   #call traits distance matrices directly from the working directory
   if(missing(traits)) {
      traits<-data(traits)
   }

   #assign equal weight to each trait if no weights
   if(missing(weights)) {
      weights<-rep(1, length(traits)+3)
   }

   # Create matrix of just traits, so that names can be maintained in the function properly
   morph.mat<-morph[,7:length(morph)]

   # Create sets of pairs:
   #species<-teeth$Obj_num  # length should match number of teeth in analysis - uses object numbers from subset
   species<-c(1:length(morph.mat[,4])) #doesn't matter what column this calls, b/c just giving integers 1-n
   combs.rows<-t(combn(species,2)) #2xn Matrix of all possible pairwise combinations of species
   toothID<-as.numeric(rownames(morph))
   objID<-as.character(morph$ID)
   combs.toothID<-t(combn(toothID,2))

   #Set up parallel loop:
   dat<-foreach(i=1:length(combs.rows[,1]), .combine='rbind') %do% { #for all pairwise comparisons, in parallel
      cc<-combs.rows[i,]  #Call the pairwise comparison to look at
      Traitscomb<-as.vector(c())     #create empty vector to fill with comparissons of whichever 2 species are specified for the loop

      for(j in 1:length(traits)) {   #For however many traits there are...
         w<-weights[j]              #trait weight value
         tt<-data.frame(traits[j])    #calls relevant trait matrix and turns it into dataframe for easier handling
         foo<-as.numeric(tt[morph.mat[cc[1],j], morph.mat[cc[2],j]]) #calls the distance value of trait[i] by looking up first Dent A's trait[i] and then Dent B's Trait[i]
         foo.w<-foo*w
         Traitscomb<-c(Traitscomb, foo.w)   #Puts the trait[i] distance value in the comparisson vector
      }

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

      Traitscomb<-unlist(Traitscomb)
      Traitscomb<-as.numeric(Traitscomb)
      Dist<-sum(Traitscomb)  #Adds up distances calculated in inner loop
      Dist.avg<-mean(Traitscomb) #Mean of distance between 2 species
      Num.traits<-length(Traitscomb)
      to.dat<-data.frame(cc[1], cc[2], combs.toothID[i,1], combs.toothID[i,2], Dist, Dist.avg, Num.traits, objID[cc[1]], objID[cc[2]])
      to.dat #output of foreach loop
   }

   #Get final data table to put into distmat
   dist.df<-data.frame(dat)
   names(dist.df)<-c("1", "2", "ToothA","ToothB","dist.sum","dist.avg", "traits.length", 'objID.A', 'objID.B')
   return(dist.df)
}
