##############################################
#                                            #
#     Standalone figure code for Tooth       #
#        R package demonstraiton             #
#        figure for manuscript               #
#                                            #
#     **Relies on calculations from          #
#     toothmorph_v0.5_Example_code.R**       #
#                                            #
#    **CAN (& SHOULD) BE RUN AS "SOURCE"**   #
#                                            #
##############################################


# writeFile <- 'pdf'
# writeFile <- 'jpg'
 writeFile <- 'off'

## Graphical Parameters
fig.dims <- c(11, 10)
cols <- c('firebrick', 'goldenrod1', 'forestgreen', 'purple', 'blue', 'gray70')
pchs <- c(21, 24, 22, 23, 8, 25)


if(writeFile == 'pdf') {
   pdf(file = "example_code/toothmorph_Figure.pdf", width = fig.dims[1], height = fig.dims[2])
}

if(writeFile == 'jpg') {
   jpeg('example_code/toothmorph_Figure.jpg', width = fig.dims[1], height = fig.dims[2], units = 'in', res = 300)
}

##### Figure for Manuscript #####
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


if(writeFile != 'off') {
   dev.off()
}
