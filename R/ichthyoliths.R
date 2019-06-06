#' ichthyoliths
#'
#' A package for calculating morphological disparity of fish teeth
#'
#' The ichthyolith package provides functions to define trait disparity matrices,
#' calculate tooth disparity, and create range charts and figures through time.
#'
#' The disparity method used in this version of the ichthyoliths package (0.0.0.9000)
#' is described in detail in the submitted manuscript: "A framework for
#' quantifying morphological variation in microfossil fish teeth and associated
#' R package, ichthyoliths" by Elizabeth Sibert, Matt Friedman, Pincelli Hull,
#' and Richard Norris. For details please contact Elizabeth Sibert \email{esibert@@fas.harvard.edu}.
#'
#' At the heart of the ichthyoliths package are functions to calculate
#' tooth disparity ichthyoliths included in the analysis. They take advantage
#' of parallel computing, as pairwise-comparisons can grow quickly.This relies
#' on the package doParallel, and uses a foreach loop.
#'
#' @docType package
#'
#' @name ichthyoliths
#'
#' @import doParallel
#'
NULL
