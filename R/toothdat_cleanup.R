#' toothdat_cleanup
#'
#' Provides some code to clean up and sort an input data frame containing
#' individual tooth morphology into a dataframe usable by the distances_clust
#' function. Works best if input dataframe has the following columns:
#' Column 1: sample id - this can be anything and is not used by the function
#' Column 2: a numeric value (usually age of the sample) - used for sortby = 'age'
#' Column 3: an object number used for sortby = 'morph'.
#'   note that sortby = 'morph' should not be used for datasets that have
#'   multiple samples, as this number can be repeated
#' Column 4: an alphanumeric code correlating to the sample id (col 1) and an
#'   object identification (col 3). Used for sortby = "age-obj"
#'
#'
#' @param toothdat a data frame of teeth including columns for identification
#' of the teeth, as well as character/state values for each tooth and trait.
#' This function is most useful for use if the input csv file has a precise
#' format, detailed below.
#'
#' @param fix_dat a logical value, defaults to false. If true, the values of
#' trait K1 and trait O will have 1 added to them. Use fix_dat=T if, when
#' entering data, you considered absence of a flange (trait K1) or absence of
#' a pulp cavity (trait O)  to have value of 0 (intuitive), rather than 1
#' (as defined in the paper).
#'
#' @param sortby a string with value "age", "morph", "original", "age-obj".
#' Tells the function what order you would like the cleaned data frame to return
#' the individual tooth objects in. Assumes dataframe includes columns 1:4 as
#' detailed below. If "original", returns the data frame in its original order
#'
#' @param toothdims a logical value that indicates whether the input dataframe
#' includes length, width, and aspect ratio measurements. Note that if this
#' value is true, in the case where some teeth have measurements and others do
#' not, a dummy value of AR=99 should be used to denote any teeth which have
#' no length/width measurements (e.g. are preserved as fragmens so measurements
#' do not have any significance), but which have been described using the
#' morphological characters.
#'
#' @param mindim used only if toothdims = TRUE; a numerical value indicating
#' the minimum size of teeth to be considered in the dataset. Acts as a filter
#' to remove small objects.
#'
#' @return a reduced dataframe of just the tooth objects which have sufficient
#' morphological description to be analyzed. Includes all the original columns
#'
#' @examples
#' toothdat <- toothdat_cleanup(toothdat, fix_dat=FALSE,
#'   sortby='age-obj', toothdims = TRUE, mindim = 100)
#'
#' @export

toothdat_cleanup<-function(toothdat, fix_dat=FALSE, sortby='age-obj', toothdims = TRUE, mindim = 100) {

   teeth_all<-subset(toothdat, A==1)  #Clean up data to be just the teeth (TraitA = 1)
   teeth<-teeth_all[complete.cases(teeth_all),]  #clean up data to include just teeth that have been described
   teeth<-subset(teeth, B!=4) #Get rid of poor quality teeth if anything is still there...

   if(fix_dat == TRUE) {  #for spreadsheets I compiled which don't use the numerical values in the published figure
      teeth$K1 <- teeth$K1 + 1   #flange presence/absence trait
      teeth$O <- teeth$O + 1 #Root presence/absence
   }

   if(toothdims == TRUE) {
      teeth.g106<-subset(teeth, WID >= mindim)  #Only ones with at least one dimension >100 um; note that here wid was by definition the smaller dimension
      teeth.nowid<-subset(teeth, WID == 0 & AR != 0) # Samples where I've put in AR (including AR=99) but no wid/len measurements
      teeth.dat<-rbind(teeth.g106, teeth.nowid)
   }

   #sort the samples...
   if(sortby=='age-obj') {
      teeth.dat<-teeth.dat[order(teeth.dat[,4]), ]  #age/object order (!)
   }
   else if(sortby == 'age') {
      teeth.dat<-teeth.dat[order(teeth.dat[,2]), ] } #age-only order, objects may be scrambled
   else if(sortby == 'morph') {
      teeth.dat<-teeth.dat[order(teeth.dat[,3]), ] } #sort by morphotype number
   else if(sortby == 'original') {
      teeth.dat <- teeth.dat[order(as.numeric(row.names(teeth.dat))),] }  #put them in original (csv) order again
   else teeth.dat <- teeth.dat  #no ordering

   teeth.dat$AR[teeth.dat$AR == 99]<-0  #reset dummy AR to 0 for diststances.clust analyses.

   return(teeth.dat)
}
