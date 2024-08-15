##############################################
#                                            #
#     Standalone figure code for Denticle    #
#        R package demonstraiton             #
#        figure for manuscript               #
#                                            #
#     **Relies on calculations from          #
#     dentmorph_v0.5_Example_code.R**        #
#                                            #
#    **CAN (& SHOULD) BE RUN AS "SOURCE"**   #
#                                            #
##############################################


# writeFile <- 'pdf'
# writeFile <- 'jpg'
writeFile <- 'off'

## Graphical Parameters
fig.dims <- c(10, 4)
cols <- c('firebrick', 'goldenrod1', 'green2', 'purple', 'blue', 'gray70')
pchs <- c(21, 24, 22, 23, 8, 25)


if(writeFile == 'pdf') {
   pdf(file = "example_code/denticle_F1_Figure.pdf", width = fig.dims[1], height = fig.dims[2])
}

if(writeFile == 'jpg') {
   jpeg('example_code/denticle_F1_Figure.jpg', width = fig.dims[1], height = fig.dims[2], units = 'in', res = 300)
}

##### Figure for Manuscript #####
par(mfrow = c(1,3), xpd = NA, oma = c(1, 0, 1, 0))
# mtext("Trait F1: Ridge System Type", side = 3)

## Plot 1: MDS1/MDS2 ##
plot(dentdat$MDS1, dentdat$MDS2, type = 'n',
     xlab = '', ylab = '', axes = F)
box()
mtext('MDS1', side = 1, line = 1)
mtext('MDS2', side = 2, line = 1)
mtext('MDS1 / MDS2', side = 3, line = 0.5, font = 2)
for(i in 1:length(unique(dentdat$F1.1))) {
   points(subset(dentdat, dentdat$F1.1 == i, select = c('MDS1', 'MDS2')),
          col = cols[i], bg = cols[i], pch = pchs[i])
}


## Plot 2: MDS1/MDS3 ##
plot(dentdat$MDS1, dentdat$MDS3, type = 'n',
     xlab = '', ylab = '', axes = F)
box()
mtext('MDS1', side = 1, line = 1)
mtext('MDS3', side = 2, line = 1)
mtext('MDS1 / MDS3', side = 3, line = 0.5, font = 2)
for(i in 1:length(unique(dentdat$F1.1))) {
   points(subset(dentdat, dentdat$F1.1 == i, select = c('MDS1', 'MDS3')),
          col = cols[i], bg = cols[i], pch = pchs[i])
}

## Annotations (add to middle plot) ##
# legend (add to plot #2)
legend('bottom', inset = c(0, -0.28), cex = 1.4,
       legend = c('Smooth', 'Linear','Geometric', 'Meandering', 'Spine', 'Branching'),
       pch = pchs, col = cols, pt.bg = cols,
       #bty = 'n',
       horiz = T, xjust = 0.5, yjust = 0.5)
mtext("Character F1: Ridge System Type", side = 3, font = 2, line = 3, cex = 1.1)

## Plot 3: MDS2/MDS3
plot(dentdat$MDS2, dentdat$MDS3, type = 'n',
     xlab = '', ylab = '', axes = F)
box()
mtext('MDS2', side = 1, line = 1)
mtext('MDS3', side = 2, line = 1)
mtext('MDS2 / MDS3', side = 3, line = 0.5, font = 2)
for(i in 1:length(unique(dentdat$F1.1))) {
   points(subset(dentdat, dentdat$F1.1 == i, select = c('MDS2', 'MDS3')),
          col = cols[i], bg = cols[i], pch = pchs[i])
}

if(writeFile != 'off') {
   dev.off()
}
