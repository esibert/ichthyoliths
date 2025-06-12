###################################################################
#                                                                 #
#        Script to update toothmorph codes from v0.4 to v0.5      #
#                                                                 #
###################################################################

library(stringr) #for string splitting

##### NOTES #####
# This script will update toothmorph codes from v0.4 to v0.5.
# It is important that the instructions be followed to set up the google
#  sheet for appropriate formatting prior to downloading it.
#  This will align the columns and column headers properly to be able to do the
#  relevant find-and-replace values, and return the columns to their original
#  state.
# In the end, this script will produce a csv file that is copy-paste-able into
#  first part of the columns of the google sheet. It does *not* create the
#  lookup values formula, but it does create the numerical values for each
#  coded column.

##### Step 0: Prepare the google spreadsheet #####
# This step is done manually in google drive.

# 0.0: duplicate the sheet you want to update and rename to v0.5.
#     This isn't strictly necessary but is important for troubleshooting, and also
#        good if you want to make sure you don't accidentally overwrite something.
#        We'll delete the v0.4 sheet manually later.
# 0.1: Add 3 rows to the top of the spreadsheet and copy-paste the top
#     3 rows of the “Blank_Coding_V0.5_Dropdowns” sheet into the new/empty
#     top 3 rows of the new v0.5 spreadsheet
# 0.2: Manually arrange the new v0.5 column headers to match up to the columns of v0.4.
#     This will involve:
#     -Insert a column for A4 between A3 and B1
#     -Insert 8 columns between D3 and E1 to make space for the new "cusps" characters
#     -Align E1 (new cusp  number) over D3 (old cusp number) to match up
#     -Align blade and barb by name rather than by letter, as these letters have shifted
#     -Insert 2 extra columns for each barb (F7-F8 and G7-G8)
#     -Confirm that there are no values in Barb 3 (old character I1 to I4) then
#        delete those columns.
#     -Ensure that the column headers for the "back half" of the spreadsheet
#        are also aligned properly.
#     -Move the metadata column headers (unique to each dataset) up to row 3.
#        This will ensure that all metadata columns are retained, and row 3 will become
#        the header row for this spreadsheet adventure.
#     -Once headers are all correct to v 0.5, delete the v0.4 header rows.
#     -Note that there is a column "Full Name" that is not part of the v0.5 system.
#        It also wasn't part of v0.4, its a holdover from v0.3. Its easiest if you
#        delete that before starting this script.
# 0.3: Save the google sheet as a csv file to bring into R.

##### Step 1: Call in the spreadsheet and prep for updates #####

# Call in spreadsheet, skipping first 2 rows with broad names and going straight for 3rd row as header
v0.4.orig <- read.csv('test_code/v0.4_v0.5_Update_Data/ToothMorph_V0.5_Coding_Spreadsheet - Hannah Bird MECO_v0.4.csv', skip = 2, header = TRUE)

# Set output filename
outfile.name <- 'test_code/v0.4_v0.5_Update_Data/Hannah_MECO_v0.5_updated.csv'

# Snag column names and determine code start and end columns
cols.names <- colnames(v0.4.orig)
code.start.col <- which(cols.names == "Z1")
code.end.col <- which(cols.names == "M2")

# Pull metadata columns (columns to the left of the code start column) to re-attach later
metadata.cols <- v0.4.orig[,1:(code.start.col-1)]

# Pull the code columns only (easier to work with this as columns will be indexed)
code.cols <- v0.4.orig[,code.start.col:code.end.col]

##### Step 2: Update the values #####

#### Update Character Trait Group A ####
# Group A: A1 and A2 unchanged;
#     Update A3 to A3 (overall tooth shape) and A4 (Triangular shape modifiers).
#     A3 was split into two characters in v0.5 from one character in v0.4.
#     Most teeth in the old A3 will be coded as “A3.1.Straight”, however we do
#        not want to lose the “concave”, “convex”, “right”, etc., values of the
#        old A3 – those are captured in A4. First step is therefore to make sure
#        that all the A3 values are also in column A4.

## Clone A3 into A4
code.cols$A4 <- code.cols$A3

## Update A3 to v0.5 values of A3.
code.cols$A3[code.cols$A3 == 'A3.1.Straight'] <- 'A3.1.Triangle'
code.cols$A3[code.cols$A3 == 'A3.2.Concave'] <- 'A3.1.Triangle'
code.cols$A3[code.cols$A3 == 'A3.3.Convex'] <- 'A3.1.Triangle'
code.cols$A3[code.cols$A3 == 'A3.4.Funnel'] <- 'A3.1.Triangle'
code.cols$A3[code.cols$A3 == 'A3.5.Bulbus'] <- 'A3.3.Bulbous'
code.cols$A3[code.cols$A3 == 'A3.6.Stalked'] <- 'A3.4.Stalked'
code.cols$A3[code.cols$A3 == 'A3.7.Right Triangle'] <- 'A3.1.Triangle'
code.cols$A3[code.cols$A3 == 'A3.8.Cusped'] <- 'A3.9.Multi-cusp'
code.cols$A3[code.cols$A3 == 'A3.9.Rectangular'] <- 'A3.2.Rectangle'

## Update A4 to v0.5 values of A4 (using prior values of A3 from above)
code.cols$A4[code.cols$A4 == 'A3.1.Straight'] <- 'A4.1.Straight'
code.cols$A4[code.cols$A4 == 'A3.2.Concave'] <- 'A4.2.Concave'
code.cols$A4[code.cols$A4 == 'A3.3.Convex'] <- 'A4.3.Convex'
code.cols$A4[code.cols$A4 == 'A3.4.Funnel'] <- 'A4.4.Funnel'
code.cols$A4[code.cols$A4 == 'A3.5.Bulbus'] <- 'A4.0.Not a triangle or too broken to code'
code.cols$A4[code.cols$A4 == 'A3.6.Stalked'] <- 'A4.0.Not a triangle or too broken to code'
code.cols$A4[code.cols$A4 == 'A3.7.Right Triangle'] <- 'A4.5.Right'
code.cols$A4[code.cols$A4 == 'A3.8.Cusped'] <- 'A4.0.Not a triangle or too broken to code'
code.cols$A4[code.cols$A4 == 'A3.9.Rectangular'] <- 'A4.0.Not a triangle or too broken to code'

#### Update Character Trait Group B ####
# Group B: B1 is unchanged; Update B2 and B3 to address surface texture and
#     additional modifiers. This may require looking at images of teeth,
#     especially for B3, however the majority of this can be done automatically.

# This character may require flagging for additional coding.
#     New column 'Spork.Flag' is added here
code.cols$Flag <- ''

## Systematically update B2:
# The major change here is that “spork” (previously B2.3.Spork in v0.4) is no
#     longer an “additional modifier” feature, but is captured under the Cusps,
#     character group E

# Flag sporks before overwriting them, as they may require additional coding!
code.cols$Flag[code.cols$B2 == 'B2.3.Spork'] <- paste0(code.cols$Flag[code.cols$B2 == 'B2.3.Spork'], 'spork, ') # tested this works to add flags

# Update Values
code.cols$B2[code.cols$B2 == 'B2.0.N/A'] <- 'B2.0.No modifiers or too broken to code'
code.cols$B2[code.cols$B2 == 'B2.3.Spork'] <- 'B2.0.No modifiers or too broken to code'
code.cols$B2[code.cols$B2 == 'B2.4.Lipped'] <- 'B2.3.Lipped'

# The following values are unchanged, no action is necessary:
#     B2.1.Cloudy
#     B2.2.Slinky

## Systematically update B3. If any teeth have a prior value of “B3.3 Partial Tooth”, you will need to look at the tooth fossil directly to confirm. Otherwise, this can be done systematically.

## Systematically update B3. If any teeth have a prior value of “B3.3 Partial Tooth”,
#     you will need to look at the tooth fossil directly to confirm values.
#     Otherwise, this can be done systematically.

# Flag B3.3.Partial modifiers state teeth for manual updating
code.cols$Flag[code.cols$B3 == 'B3.3.Partial Tooth'] <- paste0(code.cols$Flag[code.cols$B3 == 'B3.3.Partial Tooth'], 'B3.3.Partial, ') # tested this works to add flags

# Update values
code.cols$B3[code.cols$B3 == 'B3.1.No Modifiers'] <- 'B3.0.No Modifiers or too broken to code'
code.cols$B3[code.cols$B3 == 'B3.2.Whole tooth'] <- 'B3.1.Whole tooth'

#### Update Character Trait Group C ####
# Update numeric values for C1, C2, and C3. These are minor changes to the
#     numbering scheme and wording, no major changes to how things are coded.
#     C3 also has a new curve type (“scoop”) in v0.5, however this was added
#     only after observing this new datasets, and therefore no current datasets
#     require re-coding to “scoop”.

## Systematically update C1
# Update values:
code.cols$C1[code.cols$C1 == 'C1.2.Minor'] <- 'C1.2.Minor curve'
code.cols$C1[code.cols$C1 == 'C1.3.Major'] <- 'C1.3.Major curve'

# The following values are unchanged, no action is necessary:
#     C1.1.Uncurved
#     C1.4.Recurved

## Systematically update C2
# Update Values:
code.cols$C2[code.cols$C2 == 'C2.1.Uncurved'] <- 'C2.0.Uncurved or too broken to code'
code.cols$C2[code.cols$C2 == 'C2.2.Whole-tooth'] <- 'C2.1.Whole-tooth'
code.cols$C2[code.cols$C2 == 'C2.3.Tip-only'] <- 'C2.2.Tip-only'
code.cols$C2[code.cols$C2 == 'C2.4.Base-only'] <- 'C2.3.Base-only'

## Systematically update C3.
# Update Values:
code.cols$C3[code.cols$C3 == 'C3.1.Uncurved'] <- 'C3.0.Uncurved or too broken to code'
code.cols$C3[code.cols$C3 == 'C3.2.Side-curve'] <- 'C3.1.Side-curve'
code.cols$C3[code.cols$C3 == 'C3.3.Curl-curve'] <- 'C3.2.Curl-curve'
code.cols$C3[code.cols$C3 == 'C3.4.Multi-curve'] <- 'C3.3.Multi-curve'
code.cols$C3[code.cols$C3 == 'C3.5.Cone-curve'] <- 'C3.4.Cone-curve'
code.cols$C3[code.cols$C3 == 'C3.6.Spiral'] <- 'C3.5.Spiral'


#### Update Character Trait Group D ####
# D1 unchanged; Update D2 to have ‘multi-pronged’ and ‘asymmetrical’ characters
#     in more intuitive order; move D3 values to E2

## Systematically update D2:
# Update values:
code.cols$D2[code.cols$D2 == 'D2.4.Asymmetrical'] <- 'D2.5.Asymmetrical'
code.cols$D2[code.cols$D2 == 'D2.5.Multi-pronged'] <- 'D2.4.Multi-pronged'

# The following values are unchanged, no action is necessary:
#     D2.1.Cone
#     D2.2.Triangle
#     D2.3.Rectangle


#### Update Character Trait Group E ####
## This trait group is entirely new, will need to be coded manually for any
#     multi-cusp teeth. However, Trait D3 in the v0.4 code becomes Trait E1
#     in the new code. This can then be used to code 0 values in all subsequent
#     columns for single point teeth.

## Systematically update D3 to become E1.

# Flag any teeth with multiple cusps (state is not blank or 1) for further coding!
code.cols$Flag[code.cols$E1 != 'D3.1.Single point (1 cusp)' & code.cols$E1 != ''] <- paste0(code.cols$Flag[code.cols$E1 != 'D3.1.Single point (1 cusp)' & code.cols$E1 != ''], 'multicusp, ') # tested this works to add flags

# Update values:
code.cols$E1[code.cols$E1 == 'D3.1.Single point (1 cusp)'] <- 'E1.1.One cusp/Single point'
code.cols$E1[code.cols$E1 == 'D3.2.2 cusps'] <- 'E1.2.2 cusps'
code.cols$E1[code.cols$E1 == 'D3.3.3 cusps'] <- 'E1.3.3 cusps'
code.cols$E1[code.cols$E1 == 'D3.4.4 cusps'] <- 'E1.4.4 cusps'
code.cols$E1[code.cols$E1 == 'D3.5.5 cusps'] <- 'E1.5.5 cusps'
code.cols$E1[code.cols$E1 == 'D3.6.6 or more cusps'] <- 'E1.6.6 or more cusps'

## Populate columns E2 - E9 with 0 values for all coded teeth. Multi-cusp flag
#     for manual re-coding is already present.
code.cols$E2 <- code.cols$E1
code.cols$E3 <- code.cols$E1
code.cols$E4 <- code.cols$E1
code.cols$E5 <- code.cols$E1
code.cols$E6 <- code.cols$E1
code.cols$E7 <- code.cols$E1
code.cols$E8 <- code.cols$E1
code.cols$E9 <- code.cols$E1

## Update Columns E2-E9 with correct coded value for all single-point teeth.
#     Others will be flagged as invalid data and have 'multicusp' flag in the 'Flag' column
code.cols$E2[code.cols$E2 == 'E1.1.One cusp/Single point'] <- 'E2.0.One cusp or too broken to code'
code.cols$E3[code.cols$E3 == 'E1.1.One cusp/Single point'] <- 'E3.0.One cusp or too broken to code'
code.cols$E4[code.cols$E4 == 'E1.1.One cusp/Single point'] <- 'E4.0.One cusp or too broken to code'
code.cols$E5[code.cols$E5 == 'E1.1.One cusp/Single point'] <- 'E5.0.One cusp or too broken to code'
code.cols$E6[code.cols$E6 == 'E1.1.One cusp/Single point'] <- 'E6.0.One cusp or too broken to code'
code.cols$E7[code.cols$E7 == 'E1.1.One cusp/Single point'] <- 'E7.0.One cusp or too broken to code'
code.cols$E8[code.cols$E8 == 'E1.1.One cusp/Single point'] <- 'E8.0.One cusp or too broken to code'
code.cols$E9[code.cols$E9 == 'E1.1.One cusp/Single point'] <- 'E9.0.One cusp or too broken to code'


#### Update Character Trait Group F/G ####
# This trait group shifted from E/F in v0.4 to F/G in v0.5, and two additional
#     characters were added for “blade features” based on novel teeth observed
#     after v0.4 was implemented. If teeth are flagged for further coding with
#     blade features in the "notes" column, update those accordingly after running this script.

## Systematically update E1 to F1
# Update values
code.cols$F1[code.cols$F1 == 'E1.0.Too broken to code'] <- 'F1.0.Too broken to code'
code.cols$F1[code.cols$F1 == 'E1.1.No blades'] <- 'F1.1.No blades'
code.cols$F1[code.cols$F1 == 'E1.2.Symmetrical'] <- 'F1.2.Symmetrical'
code.cols$F1[code.cols$F1 == 'E1.3.Asymmetrical'] <- 'F1.3.Asymmetrical'
code.cols$F1[code.cols$F1 == 'E1.4.Single'] <- 'F1.4.Single'
code.cols$F1[code.cols$F1 == 'E1.5.Multi-blade'] <- 'F1.5.Multi-blade'

## Systematically update F2-F6 to G2-G6 and E2-E6 to F2-F6;
# I did this manually in reverse alphabetical order to avoid mistakes.
#  R shouldn't neeed that since I'm calling specific columns, but doing it anyway to be safe.

## Update values G2:
code.cols$G2[code.cols$G2 == 'F2.0.No blades'] <- 'G2.0.No blades or too broken to code'
code.cols$G2[code.cols$G2 == 'F2.1.Small'] <- 'G2.1.Small'
code.cols$G2[code.cols$G2 == 'F2.2.Medium'] <- 'G2.2.Medium'
code.cols$G2[code.cols$G2 == 'F2.3.Large'] <- 'G2.3.Large'

# Update values G3:
code.cols$G3[code.cols$G3 == 'F3.0.No blades'] <- 'G3.0.No blades or too broken to code'
code.cols$G3[code.cols$G3 == 'F3.1.Upper'] <- 'G3.1.Upper'
code.cols$G3[code.cols$G3 == 'F3.2.Middle'] <- 'G3.2.Middle'
code.cols$G3[code.cols$G3 == 'F3.3.Lower'] <- 'G3.3.Lower'
code.cols$G3[code.cols$G3 == 'F3.4.Full-length'] <- 'G3.4.Full-length'

# Update values G4:
code.cols$G4[code.cols$G4 == 'F4.0.No blades'] <- 'G4.0.No blades or too broken to code'
code.cols$G4[code.cols$G4 == 'F4.1.(1/4)'] <- 'G4.1.(1/4)'
code.cols$G4[code.cols$G4 == 'F4.2.(1/3)'] <- 'G4.2.(1/3)'
code.cols$G4[code.cols$G4 == 'F4.3.(1/2)'] <- 'G4.3.(1/2)'
code.cols$G4[code.cols$G4 == 'F4.4.(3/4)'] <- 'G4.4.(3/4)'
code.cols$G4[code.cols$G4 == 'F4.5.Full-length'] <- 'G4.5.Full-length'

# Update values G5:
code.cols$G5[code.cols$G5 == 'F5.0.No blades'] <- 'G5.0.No blades or too broken to code'
code.cols$G5[code.cols$G5 == 'F5.1.Straight'] <- 'G5.1.Straight'
code.cols$G5[code.cols$G5 == 'F5.2.Top-heavy'] <- 'G5.2.Top-heavy'
code.cols$G5[code.cols$G5 == 'F5.3.Middle-heavy'] <- 'G5.3.Middle-heavy'
code.cols$G5[code.cols$G5 == 'F5.4.Bottom-heavy (flared)'] <- 'G5.4.Bottom-heavy (flared)'

# Update values G6:
code.cols$G6[code.cols$G6 == 'F6.0.No blades'] <- 'G6.0.No blades or too broken to code'
code.cols$G6[code.cols$G6 == 'F6.1.Straight edge'] <- 'G6.1.Straight edge'
code.cols$G6[code.cols$G6 == 'F6.2.Outer-curve or long edge'] <- 'G6.2.Outer-curve or long edge'
code.cols$G6[code.cols$G6 == 'F6.3.Inner-curve or short edge'] <- 'G6.3.Inner-curve or short edge'

# Update values F2:
code.cols$F2[code.cols$F2 == 'E2.0.No blades'] <- 'F2.0.No blades or too broken to code'
code.cols$F2[code.cols$F2 == 'E2.1.Small'] <- 'F2.1.Small'
code.cols$F2[code.cols$F2 == 'E2.2.Medium'] <- 'F2.2.Medium'
code.cols$F2[code.cols$F2 == 'E2.3.Large'] <- 'F2.3.Large'

# Update values F3:
code.cols$F3[code.cols$F3 == 'E3.0.No blades'] <- 'F3.0.No blades or too broken to code'
code.cols$F3[code.cols$F3 == 'E3.1.Upper'] <- 'F3.1.Upper'
code.cols$F3[code.cols$F3 == 'E3.2.Middle'] <- 'F3.2.Middle'
code.cols$F3[code.cols$F3 == 'E3.3.Lower'] <- 'F3.3.Lower'
code.cols$F3[code.cols$F3 == 'E3.4.Full-length'] <- 'F3.4.Full-length'

# Update values F4:
code.cols$F4[code.cols$F4 == 'E4.0.No blades'] <- 'F4.0.No blades or too broken to code'
code.cols$F4[code.cols$F4 == 'E4.1.(1/4)'] <- 'F4.1.(1/4)'
code.cols$F4[code.cols$F4 == 'E4.2.(1/3)'] <- 'F4.2.(1/3)'
code.cols$F4[code.cols$F4 == 'E4.3.(1/2)'] <- 'F4.3.(1/2)'
code.cols$F4[code.cols$F4 == 'E4.4.(3/4)'] <- 'F4.4.(3/4)'
code.cols$F4[code.cols$F4 == 'E4.5.Full-length'] <- 'F4.5.Full-length'

# Update values F5:
code.cols$F5[code.cols$F5 == 'E5.0.No blades'] <- 'F5.0.No blades or too broken to code'
code.cols$F5[code.cols$F5 == 'E5.1.Straight'] <- 'F5.1.Straight'
code.cols$F5[code.cols$F5 == 'E5.2.Top-heavy'] <- 'F5.2.Top-heavy'
code.cols$F5[code.cols$F5 == 'E5.3.Middle-heavy'] <- 'F5.3.Middle-heavy'
code.cols$F5[code.cols$F5 == 'E5.4.Bottom-heavy (flared)'] <- 'F5.4.Bottom-heavy (flared)'

# Update values F6:
code.cols$F6[code.cols$F6 == 'E6.0.No blades'] <- 'F6.0.No blades or too broken to code'
code.cols$F6[code.cols$F6 == 'E6.1.Straight edge'] <- 'F6.1.Straight edge'
code.cols$F6[code.cols$F6 == 'E6.2.Outer-curve or long edge'] <- 'F6.2.Outer-curve or long edge'
code.cols$F6[code.cols$F6 == 'E6.3.Inner-curve or short edge'] <- 'F6.3.Inner-curve or short edge'

## Blade Features - most teeth in v0.4 don't have blade features, but if they have blades,
#     the value for F7 and G7 will be different than if they don't have blades at all.

# Start columns F7 and G7 based on value of column F1 (presence/absence of blades)
code.cols$F7 <- code.cols$F1
code.cols$G7 <- code.cols$F1

# Update values F7:
code.cols$F7[code.cols$F7 == 'F1.0.Too broken to code'] <- 'F7.0.No blades or too broken to code'
code.cols$F7[code.cols$F7 == 'F1.1.No blades'] <- 'F7.0.No blades or too broken to code'
code.cols$F7[code.cols$F7 == 'F1.2.Symmetrical'] <- 'F7.1.No blade features'
code.cols$F7[code.cols$F7 == 'F1.3.Asymmetrical'] <- 'F7.1.No blade features'
code.cols$F7[code.cols$F7 == 'F1.4.Single'] <- 'F7.1.No blade features'
code.cols$F7[code.cols$F7 == 'F1.5.Multi-blade'] <- 'F7.1.No blade features'
code.cols$F7[code.cols$F7 == 'F1.6.Different cusps have different blades'] <- 'F7.1.No blade features'

# Update values G7:
code.cols$G7[code.cols$G7 == 'F1.0.Too broken to code'] <- 'G7.0.No blades or too broken to code'
code.cols$G7[code.cols$G7 == 'F1.1.No blades'] <- 'G7.0.No blades or too broken to code'
code.cols$G7[code.cols$G7 == 'F1.2.Symmetrical'] <- 'G7.1.No blade features'
code.cols$G7[code.cols$G7 == 'F1.3.Asymmetrical'] <- 'G7.1.No blade features'
code.cols$G7[code.cols$G7 == 'F1.4.Single'] <- 'G7.1.No blade features'
code.cols$G7[code.cols$G7 == 'F1.5.Multi-blade'] <- 'G7.1.No blade features'
code.cols$G7[code.cols$G7 == 'F1.6.Different cusps have different blades'] <- 'G7.1.No blade features'

# Start columns F8 and G8 from Column F7
code.cols$F8 <- code.cols$F7
code.cols$G8 <- code.cols$F7

# Update values F8:
code.cols$F8[code.cols$F8 == 'F7.0.No blades or too broken to code'] <- 'F8.0.No blades/features or too broken to code'
code.cols$F8[code.cols$F8 == 'F7.1.No blade features'] <- 'F8.0.No blades/features or too broken to code'

# Update values G8:
code.cols$G8[code.cols$G8 == 'F7.0.No blades or too broken to code'] <- 'G8.0.No blades/features or too broken to code'
code.cols$G8[code.cols$G8 == 'F7.1.No blade features'] <- 'G8.0.No blades/features or too broken to code'

#### Update Character Trait Group H/I ####
# this was G, H, and I. If there ever was a barb it would’ve been in G.
#     There are no changes to the code, but the letters need to be updated.
#     This is best done in reverse alphabetical order (as we did with blades)
#     to avoid accidentally overwriting anything, although not strictly necessary

## Systematically update Barb #2, I1-I4 (usually always 0's)

# Update values I1:
code.cols$I1[code.cols$I1 == 'H1.0.No barb(s)'] <- 'I1.0.No barb(s)'
code.cols$I1[code.cols$I1 == 'H1.2.(1/4)'] <- 'I1.2.(1/4)'
code.cols$I1[code.cols$I1 == 'H1.3.(1/3)'] <- 'I1.3.(1/3)'
code.cols$I1[code.cols$I1 == 'H1.4.(1/2)'] <- 'I1.4.(1/2)'
code.cols$I1[code.cols$I1 == 'H1.5.(3/4)'] <- 'I1.5.(3/4)'

# Update values I2
code.cols$I2[code.cols$I2 == 'H2.0.No barb(s)'] <- 'I2.0.No barb(s)'
code.cols$I2[code.cols$I2 == 'H2.1.Top'] <- 'I2.1.Top'
code.cols$I2[code.cols$I2 == 'H2.2.Middle'] <- 'I2.2.Middle'
code.cols$I2[code.cols$I2 == 'H2.3.Bottom'] <- 'I2.3.Bottom'

# Update values I3
code.cols$I3[code.cols$I3 == 'H3.0.No barb(s)'] <- 'I3.0.No barb(s)'
code.cols$I3[code.cols$I3 == 'H3.1.Outer-curve'] <- 'I3.1.Outer-curve'
code.cols$I3[code.cols$I3 == 'H3.2.Inner-curve'] <- 'I3.2.Inner-curve'
code.cols$I3[code.cols$I3 == 'H3.3.Straight-edge'] <- 'I3.3.Straight-edge'

# Update values I4
code.cols$I4[code.cols$I4 == 'H4.0.No barb(s)'] <- 'I4.0.No barb(s)'
code.cols$I4[code.cols$I4 == 'H4.1.Right triangle'] <- 'I4.1.Right triangle'
code.cols$I4[code.cols$I4 == 'H4.2.Obtuse'] <- 'I4.2.Obtuse'
code.cols$I4[code.cols$I4 == 'H4.3.Hooked'] <- 'I4.3.Hooked'

# Update values H1
code.cols$H1[code.cols$H1 == 'G1.0.Too broken to code'] <- 'H1.0.Too broken to code'
code.cols$H1[code.cols$H1 == 'G1.1.No barb(s)'] <- 'H1.1.No barb(s)'
code.cols$H1[code.cols$H1 == 'G1.2.(1/4)'] <- 'H1.2.(1/4)'
code.cols$H1[code.cols$H1 == 'G1.3.(1/3)'] <- 'H1.3.(1/3)'
code.cols$H1[code.cols$H1 == 'G1.4.(1/2)'] <- 'H1.4.(1/2)'
code.cols$H1[code.cols$H1 == 'G1.5.(3/4)'] <- 'H1.5.(3/4)'

# Update values H2
code.cols$H2[code.cols$H2 == 'G2.0.No barb(s)'] <- 'H2.0.No barb(s)'
code.cols$H2[code.cols$H2 == 'G2.1.Top'] <- 'H2.1.Top'
code.cols$H2[code.cols$H2 == 'G2.2.Middle'] <- 'H2.2.Middle'
code.cols$H2[code.cols$H2 == 'G2.3.Bottom'] <- 'H2.3.Bottom'

# Update values H3
code.cols$H3[code.cols$H3 == 'G3.0.No barb(s)'] <- 'H3.0.No barb(s)'
code.cols$H3[code.cols$H3 == 'G3.1.Outer-curve'] <- 'H3.1.Outer-curve'
code.cols$H3[code.cols$H3 == 'G3.2.Inner-curve'] <- 'H3.2.Inner-curve'
code.cols$H3[code.cols$H3 == 'G3.3.Straight-edge'] <- 'H3.3.Straight-edge'

# Update values H4
code.cols$H4[code.cols$H4 == 'G4.0.No barb(s)'] <- 'H4.0.No barb(s)'
code.cols$H4[code.cols$H4 == 'G4.1.Right triangle'] <- 'H4.1.Right triangle'
code.cols$H4[code.cols$H4 == 'G4.2.Obtuse'] <- 'H4.2.Obtuse'
code.cols$H4[code.cols$H4 == 'G4.3.Hooked'] <- 'H4.3.Hooked'

#### Update Character Trait Group J ####
# No major changes, but some syntax updates are noted below;

# Update J2, J3, and J4 'rootless' values
code.cols$J2[code.cols$J2 == 'J2.0.Rootless'] <- 'J2.0.Rootless or too broken to code'
code.cols$J3[code.cols$J3 == 'J3.0.Rootless'] <- 'J3.0.Rootless or too broken to code'
code.cols$J4[code.cols$J4 == 'J4.0.Rootless/no modifiers'] <- 'J4.0.Rootless/no modifiers or too broken to code'

# All other values of J1, J2, J3, and J4 are unchanged, no action is necessary.

#### Update Character Trait Group K/L ####
# No changes to K1; L1 and L2 have minor syntax updates.

# Update L1 and L2
code.cols$L1[code.cols$L1 == 'L1.0.One-part tooth'] <- 'L1.0.One-part tooth or too broken to code'
code.cols$L2[code.cols$L2 == 'L2.0.One-part tooth'] <- 'L2.0.One-part tooth or too broken to code'
code.cols$L2[code.cols$L2 == 'L2.1.Hollow'] <- 'L2.1.Straight'
code.cols$L2[code.cols$L2 == 'L2.2.Straight'] <- 'L2.1.Straight'
code.cols$L2[code.cols$L2 == 'L2.3.Flared'] <- 'L2.2.Flared'

# All other values of K1, and L1 are unchanged, no action is necessary.

#### Update Character Trait Group M ####
# Changes include updated numbers for trait M1and syntax shifts for Trait M2.

# Update M1 values:
code.cols$M1[code.cols$M1 == 'M1.0.Tip not preserved'] <- 'M1.0.Tip not preserved/too broken to code'
code.cols$M1[code.cols$M1 == 'M1.2.Rounded'] <- 'M1.2.Rounded/Dome'
code.cols$M1[code.cols$M1 == 'M1.3.Bulbous'] <- 'M1.4.Bulbous'
code.cols$M1[code.cols$M1 == 'M1.4.Flowery'] <- 'M1.5.Flowery'
code.cols$M1[code.cols$M1 == 'M1.5.Extended'] <- 'M1.7.Extended tip'
code.cols$M1[code.cols$M1 == 'M1.6.Protruded'] <- 'M1.8.Protruding'

# Update M2 values:
code.cols$M2[code.cols$M2 == 'M2.0.Tip not preserved'] <- 'M2.0.Tip not preserved/too broken to code'
code.cols$M2[code.cols$M2 == 'M2.1.No distinct tip material'] <- 'M2.1.Tip material same as rest of tooth'

# All other values of M1 and M2 are unchanged, no action is necessary.

##### Step 3: Extract numerical code values #####

## Pull the middle (second) value of the verbal character state
num.cols <- as.data.frame((apply(code.cols, 2, str_split_i, "\\.", i=2))) #to get the period, have to escape using \\

## Convert all columns except flag to numeric
num.cols <- data.frame(lapply(num.cols[,c(1:dim(num.cols)[2]-1)], as.numeric))

# Convert all NA values back to blanks
num.cols[is.na(num.cols) == TRUE] <- ''


## Testing below:
# test.col <- code.cols$A1
# str.split <- str_split_fixed(test.col, "\\.", n=3) #to get the period, have to escape using \\
# head(str.split)
# str.split <- as.numeric(str_split_i(test.col, "\\.", i=2))

##### Step 4: Make output CSV file to return to the master coding spreadsheet #####
out.df <- cbind.data.frame(metadata.cols, code.cols, num.cols)

invalid.df <- validate_code(out.df, code.version = "tooth_v0.5")

write.csv(out.df, file = outfile.name, row.names = FALSE)

##### Step 5: Return to Google Sheets for final review #####
# 1. open the CSV file and copy-paste it into row 3 of the google sheet
# 2. Update the data validation by copy-paste-special, "data validation only" the
#     data validation dropdowns from the template sheet
# 3. Re-do the "lookup" function by copy-pasting the formula from the Z1 column from the template sheet
#     Be sure to confirm that it is correct and then copy-paste it in all lookup cells.
# 4. Copy out the "flag" column. This is the column between the code columns and the numeric columns.
#     In the google sheet it is formatted as fully black. I usually copy and paste it into
#     a new column I insert to its left, and then reset the color so that it is no longer black.
# 5. If there are any flagged teeth, manually update their codes in the spreadsheet based on the images.
# 6. Congratulations, you are done!
