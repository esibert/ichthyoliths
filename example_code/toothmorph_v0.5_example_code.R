##############################################
#                                            #
#     Example Code for Fish Tooth Morphology #
#     analysis, including workflow           #
#     and notes for recreating figures       #
#              v0.5, Dec 2025                #
#                                            #
##############################################

# Welcome to this R script for calculating morphological disparity between
# dermal denticles or fish teeth. Note that while the general workflow for
# denticles and teeth is the same, the underlying trait matrices are completely
# independent for each microfossil type, and therefore denticles and teeth
# should be analyzed independently. You will be able to define which
# character coding scheme you are using within the R function.

# This script walks through the examples using a fossil tooth morphotype
# dataset.
# For an example using denticles, please see the dentmorph_v0.5_example_code.R

# For teeth you will use the traits and weights objects for teeth
# The most recent versions are
# ichthyoliths::tooth_traits_v0.5 #trait matrices
# ichthyoliths::tooth_weights_v0.5 #weights for each trait

# The morphological disparity function (distances_clust) can take a long time
# and a lot of computing resources to run depending on the number of objects you
# are comparing. This is because it is calculating a pairwise distance between
# every single object in the dataset. The more objects, the more comparisons.
# Thus, I strongly recommend saving the calculated distances dataset as an .RData
# object and also as standalone .csv files (example code below). This will
# allow you to come back to these analyses and pick up where you left off rather
# than re-running the distances function. In a future version of this R package,
# I will have a "add objects" function, but at present, if you add objects to
# your datasets, you'll have to re-run the distances function in full.

##### General Workflow #####
# Step 0: Load the libraries
# Step 1: Call in the coded morphology matrix and define the columns which
#     contain numerical character state values. At this point you may also
#     clean up the dataset spreadsheet or define additional metadata columns
# Step 2: Run the disparity calculation (distances_clust function) and check for errors
# Step 3: Ordinate the distance matrix (NMDS) and save the ordination coordinates to the matrix
# Step 4: Plot the dataset!

########################################
#                                      #
#     Step 0: Set up the workspace     #
#                                      #
########################################

##### Libraries #####
# run this whole section every time, even if you've used a .RData file for disparity calculations
library(ichthyoliths) #For ichthyolith distance calculations and range chart
library(doParallel) #For calculating disparity efficiently using parallel computing
library(vegan) #for NMDS analyses
library(viridis) #for the range chart colors
library(rgl) #for 3D plotting - not necessary for functionality of the package, but very fun

# # To install the ichthyoliths package from Github, run the following code:
# library(devtools)
# install_github('esibert/ichthyoliths', force = TRUE) #force = TRUE overwrites any prior installation and allows for updates

##############################################
#                                            #
#     Step 1: Call in the datasets           #
#             and specify the version of     #
#             ichthyolithMorph used          #
#                                            #
##############################################

##### 1a. Define trait disparity matrices and define weights vector (or use defaults) #####
# The package has pre-loaded the traits and weights for tooth morphology v0.1, v0.2, v0.4 and v0.5.
# Note that for all future releases, the manuscript will specify the version used,
# and updated versions will be added as appropriate.

# If you want to define your own trait matrices or use this architecture to
# analyse an entirely different type of fossil, the way to import/organize the
# trait CSVs is included at the end of this script.

# ## Use the package-defined versions of the code (here using teeth_v0.5)
# you can either call this here, or you can call them directly in the distance function:
# traitset <- ichthyoliths::tooth_traits_v0.5
# weightset <- ichthyoliths::tooth_weights_v0.5

##### 1b. Import coded Morphotypes dataset #####

## Call in the dataset:
# If your dataset is directly from a file using our google drive template, skip the first two lines
toothdat <- read.csv('example_code/toothmorph_v05_Morphotypes.csv', skip = 2, header = TRUE, blank.lines.skip = TRUE)

# For the morphotypes dataset, the first 4 morphotypes are placeholders for generic types, and should be skipped as well.
toothdat <- toothdat[is.na(toothdat$Z1.1) == FALSE,] #Remove the morphotypes where there is no coded value for them, here flagged as Z1.1 = NA

## Validate the dataset to confirm all teeth are coded within bounds for each character:
invalid.df <- validate_code(toothdat, code.version = "tooth_v0.5")
# invalid.df should have length 0. If there are issues, they will appear here, correct them in the spreadsheet and re-download.

## Figure out which columns hold the numerical data for the coded teeth This will likely be the
# last 53 columns of the spreadsheet if you are using our google drive template.
# To confirm,
colnames(toothdat)
# This displays all the columns. We want columns 61 to 113 from this spreadsheet. See below for cleaner code to figure that out


## Establish the numerical values of the morphological character code:
#     Which columns hold numerical coding data (e.g. A1-O10)?
cols.names <- colnames(toothdat)
code.start.col <- which(cols.names == "A1.1") #First value of code.list
code.end.col <- which(cols.names == "M2.1") #Last value of code.list
full_morphcols <- c(code.start.col:code.end.col) #vector of code columns only
# full_morphcols should be 53 columns

## If you didn't code values for any particular tooth, you'll need to
#     replace the zero values with 'NA' for the distance function to run.
#     Note that the example dataset doesn't need this step, but your own
#     datasets, especially fossil datasets, may require it!

# Replace NA values with 0 values
toothdat[is.na(toothdat)] <- 0


####################################################
#                                                  #
#     Step 2: Running the distances function       #
#                                                  #
####################################################

#### Step 2a: run the distance function #####
tooth_distances.morphotypes <- distances_clust(morph = toothdat,
                                               traits = tooth_traits_v0.5,
                                               weights = tooth_weights_v0.5,
                                               morphCols = full_morphcols,
                                               IDCol = 1, # Morphotype  Names
                                               coresFree=2)

### Parameter explanations:
# morph is the coded matrix
# traits is the trait matrices, defined in the R package (as above) or you can pull in your own
# weights is the weights vector, defined in the R package (as above) or you can make your own
# morphCols are the columns that contain the morphotype code (this is defined above)
# IDCol is the column with the unique identifier for each object, usually a filename or
# coresFree passes to the doParallel loop and defines computing resources used.

# Note that the distances_clust function is also able to do sensitivity analyses
#  e.g. discount particular characters, upweight/downweight differently, etc.
#  If you are interested in using this feature to explore your morphospace,
#  please refer to the help file for the distances_clust function.
#  The function also can handle continuous trait values, though at present this is
#  not well-tested, so use at your own risk.

# Check for mis-coded values - if there are issues, you will get NA values; You'll need
# to find them and fix them in your coded sheet and then re-run the distances function.
# The below code helps you ID them.
df.na <- subset(tooth_distances.morphotypes, is.na(tooth_distances.morphotypes$dist.sum))
unique(df.na[,8])

##### Step 2b: Turn the output of the distance function into a distance matrix for ordination #####

# make distmat
tooth.distmat.morphotypes <- distmat(tooth_distances.morphotypes, type = "avg")
#  Should be 261 x 261 elements for the 261 toothmorph morphotypes in our example dataset

# The distance calculation calculates disparity using two different methods:
#  Average disparity across all traits considered
#  Total disparity across all traits considered
# Where there are 0 values coded, those traits are not considered for those pairs
#  as is standard in such disparity analyses, and allows for effective cross-comparison
#  of broken ichthyoliths.
#  Thus typically we use the "average" rather than the "sum" values for further calculations
#  However, you may choose differently. Feel free to explore the distance calculation
#  output to see the different metrics calculated and how you want to handle them.


##### Step 2c (optional): Save the output so you don't have to re-run long disparity calculations #####
###  Distances calculation output dataframe
write.csv(tooth_distances.morphotypes, file = 'example_code/tooth_distances.csv', row.names = FALSE)

### Distance matrix
write.csv(tooth.distmat.morphotypes, file = 'example_code/tooth_distmat.csv', row.names = FALSE)
# Note that you should also set col.names = FALSE, or remember to include skip=1
#  when calling the distmat back in. For some reason R throws an error on my
#  computer (but not others) when including col.names = FALSE in the write.csv command.


####################################################
#                                                  #
#        Step 3: Ordination!                       #
#                                                  #
####################################################

################### Teeth ###################
# This uses the vegan package's function metaMDS for a non-metric
#     multi-dimensional scaling calculation on a distance matrix
#     calculated above by the Ichthyoliths package

NMDS3.morphotypes <- metaMDS(tooth.distmat.morphotypes, k=3, distance = "euclidean", try = 100, trymax = 100)

#add ordination coordinates to morphology matrix for plotting
# This allows you to plot based on any metadata column you have included in the original data sheet
toothdat$MDS1 <- NMDS3.morphotypes$points[,1]
toothdat$MDS2 <- NMDS3.morphotypes$points[,2]
toothdat$MDS3 <- NMDS3.morphotypes$points[,3]

# Save the ordination to csv for later work
write.csv(toothdat, file = 'example_code/toothmorph_ordination.csv', row.names = FALSE)

####################################################
#                                                  #
#        Step 4: Make Plots!                       #
#                                                  #
####################################################

##### Example Plot: Base Cross Section (e.g. is it a triangle or cone?) #####

### Code for plot using a loop to objects by their character state(s) ###
## Example using Ridge System Type, Trait D2:
## I highly recommend including additional metadata, e.g. taxon, age, location, etc. in the toothdat data file for each tooth and using those as covariates to visualize as well - that is far more interesting!

# Graphical Parameters
cols <- c('firebrick', 'goldenrod1', 'forestgreen', 'purple', 'blue')
pchs <- c(21, 24, 22, 23, 8)

# Blank plot
plot(toothdat$MDS1, toothdat$MDS2, type = 'n',
     xlab = 'MDS1', ylab = 'MDS2', axes = F)
box()
mtext("D2: Base Cross Section", side = 3, cex = 1.2, line = 1.5, font = 2)
# Loop to add points from each ridge system type; Includes demo of using convexhull
for(i in 1:max(toothdat$D2.1)) {
   points(subset(toothdat, toothdat$D2.1 == i, select = c('MDS1', 'MDS2')),
          col = cols[i], bg = cols[i], pch = pchs[i])
   Plot_ConvexHull(subset(toothdat, toothdat$D2.1 == i, select = c('MDS1', 'MDS2')),
                   lcolor = cols[i], shade = TRUE, scolor = cols[i], alpha.f = 0.1)
}
# legend
legend('bottomleft', legend = c('Cone', 'Triangle','Rectangle', 'Multi-pronged', 'Asymmetrical'), pch = pchs, cex = 1, col = cols, pt.bg = cols)



##### 3D Plot #####
# ## 3d Plot with RGL library, its cool to look at - and for some reason doesn't work on macs, sorry!
# # see https://r-graph-gallery.com/3d_scatter_plot.html
# plot3d(x = toothdat$MDS1, y = toothdat$MDS2, z = toothdat$MDS3,
#        col = cols[as.factor(toothdat$A2.1)], pch = 16, radius = 0.02, type = 's')


# Have fun exploring!


##### Figure for Manuscript #####

## Graphical Parameters
fig.dims <- c(11, 10)
cols <- c('firebrick', 'goldenrod1', 'forestgreen', 'purple', 'blue', 'gray70')
pchs <- c(21, 24, 22, 23, 8, 25)

# Set up the plotting region
par(mfrow = c(2,2), oma = c(1, 0, 1, 0), mar = c(2,2,2,1))

## Plot 1: Tooth Depth (A2) ##
plot(toothdat$MDS1, toothdat$MDS2, type = 'n',
     xlab = '', ylab = '', axes = F)
box()
mtext('MDS1', side = 1, line = 0.5)
mtext('MDS2', side = 2, line = 0.5)
mtext('Tooth Depth (A2)', side = 3, line = 0.5, font = 2, cex = 1.2)
for(i in 1:length(unique(toothdat$A2.1))) {
   points(subset(toothdat, toothdat$A2.1 == i, select = c('MDS1', 'MDS2')),
          col = cols[i], bg = cols[i], pch = pchs[i])
   Plot_ConvexHull(subset(toothdat, toothdat$A2.1 == i, select = c('MDS1', 'MDS2')),
                   lcolor = cols[i], shade = TRUE, scolor = cols[i], alpha.f = 0.1)

}
legend('bottomleft',
       legend = c('Flat', 'Mid-depth', 'Deep', 'Asymmetrical'),
       col = cols[1:max(toothdat$A2.1)],
       pt.bg = cols[1:max(toothdat$A2.1)],
       pch = pchs[1:max(toothdat$A2.1)])


## Plot 2: Base Cross-Section (D2) ##
plot(toothdat$MDS1, toothdat$MDS2, type = 'n',
     xlab = '', ylab = '', axes = F)
box()
mtext('MDS1', side = 1, line = 0.5)
mtext('MDS2', side = 2, line = 0.5)
mtext('Base Cross-Section (D2)', side = 3, line = 0.5, font = 2, cex = 1.2)
for(i in 1:length(unique(toothdat$D2.1))) {
   points(subset(toothdat, toothdat$D2.1 == i, select = c('MDS1', 'MDS2')),
          col = cols[i], bg = cols[i], pch = pchs[i])
   Plot_ConvexHull(subset(toothdat, toothdat$D2.1 == i, select = c('MDS1', 'MDS2')),
                   lcolor = cols[i], shade = TRUE, scolor = cols[i], alpha.f = 0.1)

}
legend('bottomleft',
       legend = c('Cone', 'Triangle', 'Rectangle', 'Multi-Progned', 'Asymmetrical'),
       col = cols[1:max(toothdat$D2.1)],
       pt.bg = cols[1:max(toothdat$D2.1)],
       pch = pchs[1:max(toothdat$D2.1)])


## Plot 3: Blade Symmetry (F1) ##
plot(toothdat$MDS1, toothdat$MDS2, type = 'n',
     xlab = '', ylab = '', axes = F)
box()
mtext('MDS1', side = 1, line = 0.5)
mtext('MDS2', side = 2, line = 0.5)
mtext('Blade Symmetry (F1)', side = 3, line = 0.5, font = 2, cex = 1.2)
for(i in 1:length(unique(toothdat$F1.1))) {
   points(subset(toothdat, toothdat$F1.1 == i, select = c('MDS1', 'MDS2')),
          col = cols[i], bg = cols[i], pch = pchs[i])
   Plot_ConvexHull(subset(toothdat, toothdat$F1.1 == i, select = c('MDS1', 'MDS2')),
                   lcolor = cols[i], shade = TRUE, scolor = cols[i], alpha.f = 0.1)

}
legend('bottomleft',
       legend = c('No blades', 'Symmetrical', 'Asymmetrical', 'One blade'),
       col = cols[1:max(toothdat$F1.1)],
       pt.bg = cols[1:max(toothdat$F1.1)],
       pch = pchs[1:max(toothdat$F1.1)])


## Plot 4: Pulp Cavity Length (J1) ##
plot(toothdat$MDS1, toothdat$MDS2, type = 'n',
     xlab = '', ylab = '', axes = F)
box()
mtext('MDS1', side = 1, line = 0.5)
mtext('MDS2', side = 2, line = 0.5)
mtext('Pulp Cavity Length (J1)', side = 3, line = 0.5, font = 2, cex = 1.2)
for(i in 1:length(unique(toothdat$J1.1))) {
   points(subset(toothdat, toothdat$J1.1 == i, select = c('MDS1', 'MDS2')),
          col = cols[i], bg = cols[i], pch = pchs[i])
   Plot_ConvexHull(subset(toothdat, toothdat$J1.1 == i, select = c('MDS1', 'MDS2')),
                   lcolor = cols[i], shade = TRUE, scolor = cols[i], alpha.f = 0.1)

}
legend('bottomleft',
       legend = c('None', '1/4', '1/3', '1/2', '3/4', 'Full-length'),
       col = cols[1:max(toothdat$J1.1)],
       pt.bg = cols[1:max(toothdat$J1.1)],
       pch = pchs[1:max(toothdat$J1.1)])


###########################################
#           EXTRA CODE                    #
###########################################

##### Manually import trait CSV files and define weights #####
## This section allows you to change the underlying distance matrices
# and/or weights for any given trait, to explore the sensitivity of the calculation.

# ## Traits:
# # Calls the distance matrix csv files from the "data" folder of the R package.
# #   You can also manually make trait CSVs and call from a different folder if you want.
# traitset <- import_traits_csvs(csvname = "Trait*", csvpath = "data/v0.5/traitCSV_teeth_v0.5/", recurs = F)

# ## Weights:
# # Here we have given a specific weight to each tooth character; If no weights are defined, the function assumes equal weight to all traits
# weightset <- c(1, 1, 1, 0.5, 1, 1, 0.5, 1, 0.5, 0.5, 1, 1, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 1, 1)
# names(weightset) <- names(traitset) #assign names to the weights vector to confirm correct order


# ##### Find coding errors #####
# # Troubleshooting: If you have any character state values that are out of bounds
# # (e.g. coded an "8" where there are only 6 character states), the distances_clust
# # function returns an NA value.
# # Since implementing the dropdown-menu based coding this has not been an issue, but this is how we
# # used to identify coding errors
# df.na <- subset(tooth_distances.morphotypes, is.na(tooth_distances.morphotypes$dist.sum))
#
# #list of objects that broke the function (if any): (I often use column 3, but the object IDs (column 8) are also good for this);
# # Correct issues and re-run the distances_clust function until this returns an empty object.
# unique(df.na[,8])
