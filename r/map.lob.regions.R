# ====================================================================
# Purpose:       plot the regions that are being analyzed
# ====================================================================

library(maps)      
library(PBSmapping)
data(worldLL)   
par(cex = 0.9, oma = c(0,0,0,0), mar = c(0,0,0,0))
plotMap(worldLL,xlim = c(285,320),ylim = c(39,55),tckMinor = 0, xlab = expression(Longitude~(degree~E)), lwd=0.75 , las = 1, tck = -0.015, col = "lightgrey", border = "darkgrey", ylab = expression(Latitude~(degree~N))) 

