###########################################
#                                         #
#     Ichthyoliths Package Workflow       #
#           And Examples                  #
#              v0.5                       #
#                                         #
###########################################

# Welcome to this R script for calculating morphological disparity between
# dermal denticles or fish teeth. Note that while the general workflow for
# denticles and teeth is the same, the underlying trait matrices are completely
# independent for each microfossil type, and therefore denticles and teeth
# should be analyzed entirely independently. You will be able to define which
# character coding scheme you are using within the R function.

# R is a scripting language, which means each line is run independently, and you
# can run lines repeatedly, or in sequences. Each time you run a line, it
# tells the computer to execute the commands in that line. Lines (and text) with
# the # sign in front of them are "comments" and will not run. Lines without
# the # sign are not comments, and will run if you select them and hit 'run'.
# I recommend using RStudio, which is installed on all of the lab
# computers, and has a good user interface. Any questions about running R
# can be directed to google or me, whichever is easiest. Happy coding!

# The morphological disparity function (distances_clust) takes a long time
# and a lot of computing resources to run. This is because it is calculating
# a pairwise distance between every single object in the dataset. The more objects
# the more comparisons. Thus, once you are happy with the outcome, I strongly
# recommend saving the calculated distances dataset as an .RData object (example
# code below) and also as standalone .csv files (code is provided for this in this
# example script). This will allow you to come back to these analyses and pick
# up where you left off rather than re-running the distances function. In a future
# version of this R package, I will have a "add objects" function, but as of
# right now, this does not exist, so if you add objects to your datasets, you'll
# have to re-run the distances function in full.

########################################
#                                      #
#     Step 0: Set up the workspace     #
#                                      #
########################################


##### Libraries (run this whole section every time, even if you've used a .RData file) #####
library(ichthyoliths) #For ichthyolith distance calculations and range chart
library(doParallel) #For calculating disparity efficiently using parallel computing
library(vegan) #for NMDS analyses
library(viridis) #for the range chart colors
 library(rgl) #for 3D plotting - not necessary for functionality, but very fun

# # if for some reason the ichthyoliths package is NOT installed already, run the following code to install the package from GitHub:
# library(devtools)
# # if devtools isn't installed, run install.packages('devtools'), then load the library
#
# # to reinstall the most recent version of the ichthyoliths package, run:
# remove.packages('ichthyoliths')
# install_github('esibert/ichthyoliths')
## OR
# install_github('esibert/ichthyoliths', force = TRUE)

##### Functions #####
# If you have any functions that you want to use for future analyses or plotting
# that are not in the ichthyoliths R Package, define them here, or do what I do
# and write a separate functions.R file and run
#     source('functions.R')
# to call in the functions.

##############################################
#                                            #
#     Step 1: Call in the datasets           #
#             and specify the version of     #
#             ichthyolithMorph used          #
#                                            #
##############################################

################### DENTICLES ###################

##### 1a. Define trait disparity matrices and define weights vector (as appropriate) #####
# This can be done manually, by using import_traits_csvs() or by calling the saved trait matrices from within the R package.
# The package has objects for tooth morphology under v0.1, v0.2, and v0.4. It has denticle v0.4 (numbered to match the same version as the tooth morphology version).
# Note that for all future releases, the manuscript will specify the version used, and updated versions will be added as appropriate.

# ## Manual importing:
# # Traits:
# traitset <- import_traits_csvs(csvname = "Trait*", csvpath = "data/v0.4/traitCSV_denticles_v0.4/", recurs = F)
# # Here we have given a specific weight to each denticle character; If no weights are defined, the function assumes equal weight to all traits
# weightset <- c(2,1,1,1,1,1,1,1,0.5,0.5,0.5,2,1,1,1,1,1,0.5,0.5,1,1,1,1,1,1,1,1,1,1,1)

# ## Use the package-defined versions of the code (here using denticles_v0.4) - you can either call this here, or you can call them in the function:
# traitset <- ichthyoliths::denticle_traits_v0.4
# weightset <- ichthyoliths::denticle_weights_v0.4


##### 1b. Import coded dataset #####

## Call in the dataset:
# If downloaded directly from google drive, skip the first line
dentdat <- read.csv('example_code/Blank Copy of Denticles Sheet - V0.5_Holotypes.csv', skip = 1, header = TRUE, blank.lines.skip = TRUE)

## Figure out which columns hold the *numerical* data for the coded denticles. Try:
colnames(dentdat)
# This displays all the columns. We want columns 55 to 98, apparently.

## Which columns hold numerical coding data (e.g. A1-O10) - hint: if you can't count, try colnames(dentdat) and use that to pick the columns
full_morphcols.holotypes <- c(55:98) #should be 44 columns

## Clean the dataset: Remove any uncoded denticles: (Maybe don't run this on your own datasets, but we'll see)
# dentdat <- dentdat[complete.cases(dentdat[,full_morphcols.holotypes]),]



####################################################
#                                                  #
#     Step 2: Running the distances function       #
#                                                  #
####################################################

################### DENTICLES ###################

## running the distance function
dent_distances.holotypes <- distances_clust(morph = dentdat, traits = denticle_traits_v0.5,
                                            weights = denticle_weights_v0.5,
                                            morphCols = full_morphcols.holotypes,
                                            traitsStartCol = 55, IDCol = 1,
                                            subsetWeights = TRUE, contTraits = FALSE, coresFree=2)

### Parameter explanations:
# morph is the coded matrix
# traits is the trait matrices, called from the R package or you can pull in your own
# weights is the weights vector, called from the R package or you can make your own
# morphCols are the columns that contain the morphotype code (this is defined above and used for cleaning steps as well)
# traitStartCol is the first column of the code
# IDCol is the column with the unique identifier for each object
# subsetweights - if TRUE, pulls the selected traits from the weights vector: this only matters if doing distance calculation on a subset of traits
# contTraits tells the function whether there are any characters that are continuous rather than discrete, and is obsolete in this version of the code
# coresFree passes to the doParallel loop and defines computing resources used.

# Troubleshooting - find the NAs...
df.na <- subset(dent_distances.holotypes, is.na(dent_distances.holotypes$dist.sum))

#list of objects that broke the function (if any): (I often use column 3, but the object IDs (column 8) are also good for this);
# Correct issues and re-run the dat_distances function until this returns an empty object.
unique(df.na[,8])

## Once there are no more NA values, make the distance matrix object to calculate the morphospace
# make distmat
dent.distmat.holotypes <- distmat(dent_distances.holotypes)

## Save the output so you don't have to re-run disparity calculations next time!
write.csv(dent_distances.holotypes, file = 'example_code/dent_distances.csv', row.names = FALSE)
write.csv(dent.distmat.holotypes, file = 'example_code/dent_distmat.csv', row.names = FALSE) #For some reason it won't let me set col.names = FALSE, so remember to run skip = 1 when calling the distmat document in.

## You can call these back in at any time


####################################################
#                                                  #
#        Step 3: Ordination!                       #
#                                                  #
####################################################

################### DENTICLES ###################

NMDS3.holotypes <- metaMDS(dent.distmat.holotypes, k=3, distance = "euclidean", try = 100, trymax = 100)

#add ordination coordinates to morphology matrix for plotting
dentdat$MDS1 <- NMDS3.holotypes$points[,1]
dentdat$MDS2 <- NMDS3.holotypes$points[,2]
dentdat$MDS3 <- NMDS3.holotypes$points[,3]

# Plot this based on linear/geometric/etc.
cols <- rainbow(length(unique(dentdat$F1)))
plot(dentdat$MDS1, dentdat$MDS2, pch = 16, cex = 2, col = cols[as.factor(dentdat$F1)])
#color based on the broad denticle type (linear/geometric/etc - this is trait F1 in the code)
legend('topright', legend = c('smooth', 'linear','geometric', 'meandering', 'spine'), pch = 16, cex = 1, col = cols)


## 3d Plot with RGL library - this isn't required, its just cool
# see https://r-graph-gallery.com/3d_scatter_plot.html
plot3d(x = dentdat$MDS1, y = dentdat$MDS2, z = dentdat$MDS3,
       col = cols[as.factor(dentdat$F1)], pch = 16, radius = 0.05, type = 's')

# Example plot that looks at a specific character trait - in this case, number of independent ridges (G2)
cols <- rainbow(length(unique(dentdat$G2)))
#color based on the character state)
plot(dentdat$MDS1, dentdat$MDS2, pch = 16, cex = 2, col = cols[as.factor(dentdat$G2)])
legend('topright', legend = sort(unique(dentdat$G2)), pch = 16, cex = 1, col = cols)

# Now you try - see if you can make a plot with denticles coloring a different character you're interested in! hint: copy/paste the code and modify it accordingly...



####################################################
#                                                  #
#        Step 4:  Combine Your data with the       #
#                 Holotpes Dataset                 #
#                                                  #
####################################################

# To combine datasets (and, for example, plot your data on the same ordination as others),
#     you'll need to make several "objects" that have the same dimensions.
#     Unfortunately, everyone's metadata (first few columns) is different, so the first step is
#     definining your own dataset and making it the right format. Here's a template for how to do that:

# Step 1: Call in the dataset
# Step 2: Pick just the denticles (Z1.1 == 2)
# Step 3: Make a matrix that is *just* the denticle codes (the equivalent of the "full morphcols")
# Step 4: Add some identification columns - one for your sample ID's, and one to flag it as your own dataset
# Step 5: Replace all NA values with 0
# Step 6: COMBINE MATRICES :)


#Elizabeth P's dataset (Steps 1-5)
ElizP_fulldat <- read.csv('example_code/Blank Copy of Denticles Sheet - Elizabeth_P_v0.5.csv', skip = 1, header = TRUE, blank.lines.skip = TRUE)
# Call *only* the denticles:
ElizP_fulldat <- subset(ElizP_fulldat, ElizP_fulldat$Z1.1 == 2)
names(ElizP_fulldat)
morphcols_EP <- ElizP_fulldat[,58:101]
morphcols_EP$SampleID <- ElizP_fulldat$Full_File_Name
morphcols_EP$SampleSet <- "ElizabethP"
morphcols_EP[is.na(morphcols_EP)] <- 0



#Arleth's dataset (Steps 1-5)
Arleth_fulldat <- read.csv('example_code/Blank Copy of Denticles Sheet - Arleth_v0.5.csv', skip = 1, header = TRUE, blank.lines.skip = TRUE)
# Call *only* the denticles:
Arleth_fulldat <- subset(Arleth_fulldat, Arleth_fulldat$Z1.1 == 2)
names(Arleth_fulldat)
morphcols_AM <- Arleth_fulldat[,58:101]
morphcols_AM$SampleID <- Arleth_fulldat$Filename
morphcols_AM$SampleSet <- "Arleth"
morphcols_AM[is.na(morphcols_AM)] <- 0

#Holotypes dataset (Steps 3-5, we already did steps 1 and 2 up in the earlier part of hte code)
morphcols_Hol <- dentdat[,full_morphcols.holotypes]
morphcols_Hol$SampleID <- dentdat$Type
morphcols_Hol$SampleSet <- "Holotypes"


## Note that now each of these is 46 columns long and can be used to explore your datasets. ##

# Combined dataset
alldat <- rbind(morphcols_Hol, morphcols_AM, morphcols_EP) #this is all 3 of you.
###IMPORTANT: If you combine matrices, they *must* have the same number of columns. If you want to add any additional metadata (e.g. Elizabeth P, you want to include "age", for example", you'll need to add an empty "Age" column to any of the matrices that you're combining yours with before combining)
## Arleth, if you want to include species, or family, or other taxonomic data, same thing... you'll need to add those columns up above *before* combining the datasets.

# Run the distance function - note that I had to update the morphCols and IDCol to match the new matrix
alldat.distances <- distances_clust(morph = alldat, traits = denticle_traits_v0.5,
                                            weights = denticle_weights_v0.5,
                                            morphCols = c(1:44),
                                            traitsStartCol = 1, IDCol = 45,
                                            subsetWeights = TRUE, contTraits = FALSE, coresFree=2)

# Check the output:
df.na <- subset(alldat.distances, is.na(alldat.distances$dist.sum))

#list of objects that broke the function (if any): (I often use column 3, but the object IDs (column 8) are also good for this);
# Correct issues and re-run the dat_distances function until this returns an empty object.
unique(df.na[,8])

## Once there are no more NA values, make the distance matrix object to calculate the morphospace
# make distmat
dent.distmat.alldat <- distmat(alldat.distances)


# Ordinate the dataset!
NMDS3.alldat <- metaMDS(dent.distmat.alldat, k=3, distance = "euclidean", try = 100, trymax = 100)

#add ordination coordinates to morphology matrix for plotting
alldat$MDS1 <- NMDS3.alldat$points[,1]
alldat$MDS2 <- NMDS3.alldat$points[,2]
alldat$MDS3 <- NMDS3.alldat$points[,3]


# Plot this based on linear/geometric/etc.
cols <- rainbow(length(unique(alldat$F1)))
plot(alldat$MDS1, alldat$MDS2, pch = 16, cex = 2, col = cols[as.factor(alldat$F1)])
#color based on the broad denticle type (linear/geometric/etc - this is trait F1 in the code)
legend('topright', legend = sort(unique(alldat$F1.1)), pch = 16, cex = 1, col = cols)


# Plot this based on your personal denticles!
cols <- c('green', 'turquoise', 'gray')
plot(alldat$MDS1, alldat$MDS2, pch = 16, cex = 2, col = cols[as.factor(alldat$SampleSet)])
legend('topleft', legend = sort(unique(alldat$SampleSet)), col = cols, pch = 16, cex = 1)
