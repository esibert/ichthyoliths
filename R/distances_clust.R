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
#' @param morph a data frame with (at least) columns for unique tooth identifiers
#' (IDCol specified below) and columns containing morphometric data to input into
#' the disparity calculation. Other columns can also be included in this data frame,
#' but will be ignord by the function, as specified below.
#'
#' @param traits a list of trait connectivity matices. (For ichthyolith analyses based
#' after Sibert et al 2019, you can use use the 'traits' object included in this package,
#' which the function will include if you do not spcify a traits list.)
#'
#' @param weights a vector of how much each trait is weighted in the analysis
#' can use data(weights) included in this package, or define your own.
#'
#' @param morphCols a vetor stating the columns with morphometric data to pull for analysis.
#' This can be a continous list (e.g.c(1:9) to take columns 1-9 as morphometric data) or it
#' can be a subset (e.g. c(1:3, 5, 7:9) to exclude columns 6 and 8). It can be any range
#' as long as the columns exist within the morph matrix (e.g. c(35:58)).
#'
#' @param traitsStartCol is the numeric value of the column in the morph data frame
#' that matches to the first character in the traits list of connectivity matrices. In most
#' cases, this will be the value of the morphCols object, however if you are running an
#' analysis which does not use the first trait (e.g. omits Trait A1 from the morphCols list),
#' you must specify the column of the morph matrix that is Trait A1 here. If left blank,
#' this defaults to the first value of morphCols.
#'
#' @param IDCol is a numeric value referring to the column in the morph data frame which
#' contains the unique identifiers for each object.
#'
#' @param subsetWeights is a logical for whether the weights vector applies to the whole
#' list of traits you have fed into the function (FALSE) or just the subset called by
#' the morphCols vector (TRUE) from a larger list of weights that apply to all characters
#' even if not all of them are included in the morphCols vector. Defaults to FALSE.
#'
#' @param conTraits ######### ideally a character vector containing the column names
#' of the continuous traits to consider, or a numeric vector containing the column numbers.
#' Currently it is just a logical for default continuous traits of LEN, WID, and AR.
#' Default value is FALSE.########
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
#' "ToothA" and "ToothB", - the row number for each tooth from the original input dataset (will be different from the 1 and 2 column only if objects were previously removed from the dataset) \cr
#' "dist.sum" - the summed distance between the two teeth \cr
#' "dist.avg" - the averaged distance between the two teeth \cr
#' "traits.length" - the number of traits actually considered in the comparison \cr
#' "objID.A" and "objID.B" - the unique identifiers for each tooth considered.
#'
#' @export

distances_clust<-function(morph, traits, weights, morphCols, traitsStartCol, IDCol, subsetWeights, contTraits = FALSE, coresFree=2) {
   #call traits distance matrices directly from the working directory
   if(missing(traits)) {
      traits<-data(traits)
   }

   #assign equal weight to each trait if no weights
   if(missing(weights)) {
      weights<-rep(1, length(traits)+3)
   }

   if(missing(traitsStartCol)) {
      traitsStartCol <- morphCols[1]
   }

   if(missing(subsetWeights)) {
      subsetWeights <- FALSE
   }


   # Create matrix of just traits, so that names can be maintained in the function properly
   morph.mat<-morph[,morphCols]  # startCol is the column to start pulling morphological trait data from

   # Coerce morph.mat to numeric (Shouldn't really be necessary but sometimes text-->character instead of number happens)
   morph.mat <- apply(morph.mat, 2, as.numeric)

   # Create sets of pairs:
   species<-c(1:length(morph.mat[,1])) #doesn't matter what column this calls, b/c just giving integers 1 to n
   combs.rows<-t(combn(species,2)) #2xn Matrix of all possible pairwise combinations of species
   toothID<-as.numeric(rownames(morph))
   objID<-as.character(morph[,IDCol])
   combs.toothID<-t(combn(toothID,2))

   # Select the subset of traits to use in the traits list.
   trait_list_positions <- morphCols-(traitsStartCol-1)
   selected_traits <- traits[trait_list_positions]

   # if necessary, select the subset of weights to be used in the loop as well.
   if(subsetWeights == TRUE) {
      weights <- weights[trait_list_positions]
   }

   #Set up cores and cluster to run loop
   cores<-detectCores()
   cl<-makeCluster(cores-coresFree)  #detect the number of cores and leave some open
   registerDoParallel(cl)

   #Set up parallel loop:
   dat<-foreach(i=1:length(combs.rows[,1]), .combine='rbind') %dopar% { #for all pairwise comparisons, in parallel
      cc<-combs.rows[i,]  #Call the pairwise comparison to look at
      Traitscomb<-as.vector(c())     #create empty vector to fill with comparissons of whichever 2 species are specified for the loop

      for(j in 1:length(selected_traits)) {   #For however many traits there are in this analysis...
         w<-weights[j]              #trait weight value
         tt<-data.frame(selected_traits[j]) #calls relevant trait matrix and turns it into dataframe for easier handling
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

      if(length(Traitscomb > 0)) { #Mean of distance between 2 species
         Dist.avg<-mean(Traitscomb)
         }
      else Dist.avg <- 0 # fix a divide by zero error

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
   names(dist.df)<-c("1", "2", "ObjectA","ObjectB","dist.sum","dist.avg", "traits.length", 'objID.A', 'objID.B')
   return(dist.df)
}
