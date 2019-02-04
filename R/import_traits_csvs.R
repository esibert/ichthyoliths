#' import_traits_csvs
#'
#' Imports connectivity matrices and builds list of all traits to include in
#' analysis. Use this if you are using connectivity matrices that are \strong{not}
#' described in the current version of ichthyolith classification for the package.
#' If using the standard ichthyolith coding scheme, simply call the included
#' traits data: \code{data(traits)}
#'
#' @param csvname a regular expression, Wildcard search term to find all
#' connectivity matrix csv files. Defaults to "Trait*". Case sensitive.
#' @param csvpath a character vector of full path names. Directory to look for
#' the trait connectivity csv files. Default correxponds to the  working
#' directory, \code{\link[base]{getwd()}}
#' @param recurs a logical value. Determines whether the search should recurse
#' into the directory. Defaults to TRUE.
#' @param ... arguments to be passed to the list.files() function.
#' @return A list of normalized connectivity matrices. Names of objects in the
#' list are the csv names without the .csv part
#' @examples
#' trait.matrices <- import_traits_csvs(csvname = "Trait*", csvpath = ".", recurs = T)
#'
#' @export

import_traits_csvs<-function(csvname = "Trait*", csvpath = ".", recurs = T, ...) {
   #Find the relevant files
   files <- list.files(path = csvpath, pattern = csvname, recursive = recurs, full.names = TRUE, ...)

   # call in .csv distance matrix files, normalize, and build traits list
   traits <- list()                        #Make empty list for traits list

   for (i in 1:length(files)) {           #Start loop to call in .csv files
      foo <- read.csv(files[i], header = F)   #read in the file as data frame?J
      norm <- foo/max(foo)                  #normalize the matrix so max distance is 1
      traits[[i]] <- norm                    #Generate list of all trait matrices
   }

   # Name the traits matrices within the traits list
   split <- as.character(strsplit(files, ".csv"))   #call name of text before '.csv'
   split_path <- function(x) {
      if (dirname(x)==x) x else c(basename(x), split_path(dirname(x)))
   }
   split <- sapply(split, split_path)[1, ]
   names(split) <- split
   names(traits) <- split

   #output
   return(traits)
}
