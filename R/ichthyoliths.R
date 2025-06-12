#' ichthyoliths
#'
#' A package for calculating morphological disparity of ichthyoliths (fish teeth and shark scales)
#'
#' The ichthyolith package provides functions to define trait disparity matrices,
#' calculate tooth disparity, and create range charts and figures through time.
#'
#' The disparity method used in this version of the ichthyoliths package (currently v0.5,
#' is described in detail in a pair of manuscripts, detailed below.
#'
#' Denticles:
#' The full Denticles v0.5 (current) code is fully described in the open source publication:
#' Leah D Rubin, Gareth J Fraser, Molly K Gabler-Smith, George V Lauder,
#'    Whitney V Ribeiro, Diego F B Vaz, Nicholas Wallis-Mauro, Elizabeth C Sibert,
#'    Quantifying the denticle multiverse: a standardized coding system to capture
#'    three dimensional morphological variation for quantitative evolutionary and
#'    ecological studies of elasmobranch denticles,
#'    Integrative Organismal Biology, 2025;, obaf021, https://doi.org/10.1093/iob/obaf021
#' Available here: https://academic.oup.com/iob/advance-article/doi/10.1093/iob/obaf021/8129726
#'
#' Teeth:
#' The tooth manuscript describing v0.5 is still undergoing review as of 6/12/2025,
#'    will update here when it is published.
#'
#' The original version of the code (v0.1) is described in detail in
#' Sibert et al (2018), "Two pulses of morphological diversification in Pacific
#' pelagic fishes following the Cretaceous–Palaeogene mass extinction" (2018)
#' Proceedings of the Royal Society B: Biological Sciences
#' http://doi.org/10.1098/rspb.2018.1194.
#'
#' For details and questions please contact Elizabeth Sibert \email{esibert@@whoi.edu}.
#'
#' At the heart of the ichthyoliths package are functions to calculate
#' tooth disparity ichthyoliths included in the analysis. They take advantage
#' of parallel computing, as pairwise-comparisons can grow quickly.This relies
#' on the package doParallel, and uses a foreach loop.
#'
#' @docType _PACKAGE
#'
#' @name ichthyoliths
#'
#' @import doParallel
#' @import viridis
#'
NULL
