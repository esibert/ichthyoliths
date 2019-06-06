##############################################
#                                            #
#               Version Log                  #
#            Started 5/9/2019                #
#          better late than never            #
#                                            #
##############################################

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
