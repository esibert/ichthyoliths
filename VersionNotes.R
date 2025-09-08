##############################################
#                                            #
#               Version Log                  #
#            Started 5/9/2019                #
#          better late than never            #
#                                            #
##############################################

##### Denticle Code v1.0 is live! - June 2025 #####
# To go along with the formal publication of the Denticle Morphology MS,
# denticles_v0.5 has been upgraded to denticles_v1.0.
# There are no changes from v0.5 to v1.0, however, we feel that it is important to
#     include a formal version update with the publication.

# "Quantifying the denticle multiverse: a standardized coding system to capture three dimensional morphological variation for quantitative evolutionary and ecological studies of elasmobranch denticles",
# Leah D Rubin, Gareth J Fraser, Molly K Gabler-Smith, George V Lauder, Whitney V Ribeiro, Diego F B Vaz, Nicholas Wallis-Mauro, Elizabeth C Sibert,
# Integrative Organismal Biology, 2025;, obaf021,
# https://doi.org/10.1093/iob/obaf021

##### Validation Tools - June 2025 #####
# This version contains no changes to the morphological character codes (hence v0.5.0),
# But does contain a new function, validate_code.R, which checks whether there are issues
# with the coded tooth and denticle values,

# validate_code function checks for errors in tooth_v0.5 and dent_v0.5.
#  Usage:
# invalid.df <- validate_code(tooth.df, code.version = "tooth_v0.5")
# invalid.df <- validate_code(denticle.df, code.version = "denticle_v0.5")


##### Tooth v0.5 - May 2025 #####
# Substantial update to code, updates include: code for multi-cusp teeth, expansion of options for blades, and additional small changes to harmonize observed coding issues.

# tooth_traits_v0.5 <- import_traits_csvs(csvname = "Trait*", csvpath = "data/v0.5/traitCSV_teeth_v0.5/", recurs = F)
# save(tooth_traits_v0.5, file = 'data/tooth_traits_v0.5.RData')
#
# tooth_weights_v0.5 <- c(
#    1, 1, 1, 0.5, #A
#    1, 1, 0.5, #B
#    1, 0.5, 0.5, #C
#    1, 1, #D
#    1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, #E
#    1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, #F
#    0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, #G
#    1, 0.5, 0.5, 0.5, #H
#    1, 0.5, 0.5, 0.5, #I
#    1, 0.5, 0.5, 0.5, #J
#    1, #K
#    0.5, 0.5, #L
#    1, 1 #M
# )
# names(tooth_weights_v0.5) <- names(tooth_traits_v0.5)
# save(tooth_weights_v0.5, file = 'data/tooth_weights_v0.5.RData')


##### Tooth v0.4.1 - March 2024 #####
# Update to include 'multi-pronged' tooth option in trait D2.
# tooth_traits_v0.4.1 <- import_traits_csvs(csvname = "Trait*", csvpath = "data/v0.4/traitCSV_teeth_v0.4.1/", recurs = F)
# # save with new versions
# save(tooth_traits_v0.4.1, file = 'data/tooth_traits_v0.4.1.RData')


##### Denticle v0.5 - Summer 2024 #####
# Major overhaul to denticle code, included rewrites of character groups B, C, I, J, and N to better accommodate newly discovered morphologies.
# Major overhaul to tooth code, added character states to include blade features and incorporate additional multi-cuspate tooth characteristics.
# Developed vignettes for publication of both tooth and denticle morph manuscripts

# ##  Denticle Objects:
# denticle_traits_v0.5 <- import_traits_csvs(csvname = "Trait*", csvpath = "data/v0.5/traitCSV_denticles_v0.5/", recurs = F)
# save(denticle_traits_v0.5, file = 'data/denticle_traits_v0.5.RData')
#
# denticle_weights_v0.5 <- c(2,0.5,1,1,1,1,1,1,1,1,0.5,0.5,0.5,2,1,1,1,0.5,0.5,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,1,0.5,0.5,1,1,1,1,1,1,1,1,1,1)
# names(denticle_weights_v0.5) <- names(denticle_traits_v0.5)
# save(denticle_weights_v0.5, file = 'data/denticle_weights_v0.5.RData')

## Tooth Objects


##### v0.4.1 - Jan 2024 #####
# Added Tooth matrices and weights for v0.4
# Cleaned up old functions



##### v0.4 - July 2023 #####
## Major overhaul of coding system
# Updated to include both denticles and teeth as separate versions and objects

# # Denticles objects:
# denticle_traits_v0.4 <- import_traits_csvs(csvname = "Trait*", csvpath = "data/v0.4/traitCSV_denticles_v0.4/", recurs = F)
# save(denticle_traits_v0.4, file = 'data/denticle_traits_v0.4.RData')
# denticle_weights_v0.4 <- c(2,1,1,1,1,1,1,1,0.5,0.5,0.5,2,1,1,1,1,1,0.5,0.5,1,1,1,1,1,1,1,1,1,1,1)
# save(denticle_weights_v0.4, file = 'data/denticle_weights_v0.4.RData') #Example save

# Updated distances_clust function to coerce any morphological datasets into numeric values prior to processing
#     This was necessary because of a quirk of google docs formatting
# Also fixed an inefficiency with the distmat function that previously coerced two text columns into numeric values and threw errors
#     These text columns (names of the objects) were not required for creating the distance matrix, and therefore are removed
#     from the processing.

##### v0.2.2 - August 2020 #####
# Added range extension option for rangechart function

##### v0.2.1 - June 5, 2019 #####
# Added rangechart function for fossil work

##### v0.2 - May 9, 2019 #####
## Character state updates
# Added additional morphological characters to accommodate cichlid tooth diversity by splitting character C into C1 [same as old character C] and C2 [number of cusps, can be 1, 2, 3, or 4+]
# Added additional character state to trait L (protruding tip shape)
# Added additional character state to trait E1, allowing for 'recurved' teeth
# Added trait matrix CSV files for tooth shape directly into the data directory for this package. These will now be updated and curated on GitHub.
# Added documentation for trait CSVs, including the summary figure of tooth character shapes


## Laying Groundwork for different versions of tooth morphological character sets and also down the line, denticles vs. teeth
# saved old traits and weights objects as traits_v0.1.RData and weights_v0.1.RData
# saved NEW traits adn weights objects as tooth_traits_v0.2.RData tooth_weights_v0.2.RData


##### v0.1 - Feb 2019 #####
# Updated distances matrices to include additional morphological character states found in modern fish teeth
#    including adding additional states for K traits to allow for two flanges
#    Also changed the distance matrices considerably to be more in agreement with our observations of tooth shape
# cleaned up distances_clust function again

##### v0.0.3 - Dec 2018 #####
# Updated distances_clust function again

##### v0.0.2 - Apr 2018 #####
# added plot convex hull function for plotting in morphospace

##### v0.0.1 - Mar 2018 #####
# Updated distances_clust function to be more flexible

##### v0.0.0.9 - Nov 2017 #####
# ichthyoliths package, mostly the distances_clust function and trait CSV files put online at github.com/esibert/ichthyoliths

##### v0.0.0.1 - available Sept 2018 #####
# Original version of Ichthyoliths package was published as part of the toothmorph documentation (github.com/esibert/toothmorph)
# Characters for only Eocene to Cretaceous teeth, used in manuscript:
# Elizabeth Sibert, Matt Friedman, Pincelli Hull, Gene Hunt Richard Norris, "Two pulses of morphological diversification in Pacific pelagic fishes following the Cretaceous–Palaeogene mass extinction" (2018) Proceedings of the Royal Society B: Biological Sciences http://doi.org/10.1098/rspb.2018.1194
