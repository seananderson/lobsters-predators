# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Dec 26, 2012
# Last modified: Oct 06, 2014
# Purpose:       create lagged data with both NAO and temperature ...
# ignore effort
# ====================================================================

# source necessary functions:
source("import.hom.data.R")
source("read.hom.data.R")
source("combine.lob.data.2.R")
source("scale.dat.R")
#source("explore.hom.ggplot.R")    #creates a plot of all the data for each region

use.effort <- FALSE
EQUALLY.WEIGHT.INDICES <- FALSE

# read in the data:
hom.dat <- read.hom.dat(save.file.name = "hom.dat", include.row.name = "Lobster-predators")
hom.indices <- read.hom.dat(save.file.name = "hom.indices", include.row.name = "Lobster indices")

#combine the data by predators, prey, and environment:
# also writes out a .csv file of the combined data
hom.dat.combined <- combined.lob.data(dat = hom.dat, type = "lob.pred.climate", save.file.name = "hom.dat.combined", equally.weight.indices = EQUALLY.WEIGHT.INDICES)
hom.indices.combined <- combined.lob.data(dat = hom.indices, type = "lob.indices", save.file.name = "hom.indices.combined")

hom.indices.combined <- subset(hom.indices.combined, year > 1)
write.csv(hom.dat.combined, file = "../data/hom.dat.combined.csv",
  row.names = FALSE)
write.csv(hom.indices.combined, file = "../data/hom.indices.combined.csv",
  row.names = FALSE)

use.NAO.vec <- c(TRUE, FALSE)
for(i in 1:2) {
  use.NAO <- use.NAO.vec[i]  # if false will use water temperature
  source("lag.the.data.R")
  if(use.NAO == TRUE){
    write.csv(d.scaled, file = "d.scaled.NAO.csv", row.names = FALSE)
    d.melt <- junk.melt
    write.csv(d.melt, file = "d.melt.NAO.csv", row.names = FALSE)
  }
  if(use.NAO == FALSE) {
    write.csv(d.scaled, file = "d.scaled.TEMP.csv", row.names = FALSE)
    d.melt <- junk.melt
    write.csv(d.melt, file = "d.melt.TEMP.csv", row.names = FALSE)
  }
}

