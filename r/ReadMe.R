#Steps to run the analyses of lobster, predators and temperature in the NW Atlantic
##  Created January 2011 by Stephanie Boudreau
#Goal will be to source this file to run the analysis. Right now (Feb 2011) it is just bits and pieces in the making. 

#setwd("C:\\Users\\Stephanie\\Documents\\My Dropbox\\Lobsters_Predators\\r") #Steph
#getwd()
#setwd("../r") #FYI, .. means go down a folder, / change folder to
#setwd("C:\\Users\\Stephanie\\Documents\\My Dropbox\\Lobsters_Predators\\r")

rm(list = ls())

###
# options:
use.NAO <- FALSE # if false will use water temperature
read.in.new.data <- FALSE # if false will load previous data to save time
####

# libraries:
library(ggplot2)
library(reshape)
library(plyr)
library(meta)
library(arm)
#library(nlme)
library(AICcmodavg)

# source necessary functions:
source("import.hom.data.R") 
source("read.hom.data.R")
source("combine.lob.data.2.R")
source("scale.dat.R")
#source("explore.hom.ggplot.R")    #creates a plot of all the data for each region

# the regions to import (these names must match the names on the tabs
# in the spreadsheet
#regions <- c("GOM", "SNE", "GB", "Massachusetts", "Rhode Island", "Connecticut", "sGSL", "SSBOF", "NL")
if(read.in.new.data) {
regions <- c("Gulf of Maine", "s New England", "Georges Bank", "Massachusetts", "Rhode Island", "Connecticut", "s Gulf St Lawrence", "Nova Scotia", "Newfoundland")

# read in the data:
hom.dat <- read.hom.dat(save.file.name = "hom.dat", include.row.name = "Lobster-predators")
hom.indices <- read.hom.dat(save.file.name = "hom.indices", include.row.name = "Lobster indices")

# make plots of the raw data by region:
# no idea if this still runs
#explore.hom.ggplot(hom.dat)

#combine the data by predators, prey, and environment:
# also writes out a .csv file of the combined data
hom.dat.combined <- combined.lob.data(dat = hom.dat, type = "lob.pred.climate", save.file.name = "hom.dat.combined")
hom.indices.combined <- combined.lob.data(dat = hom.indices, type = "lob.indices", save.file.name = "hom.indices.combined")

# plot the raw time series for publication:
  save.image(file = "hom.dat.for.lagging.rda")
  source("plot.time.series.R")   

} else {  # we're going to use the data as it was last time it was read in:
  load("hom.dat.for.lagging.rda")
}

# lag the data:
source("lag.the.data.R")

# run multimodel averaging:
#source("mod.averaging.lmer.R")
source("lmer.mod.avg.formulas2.R")

# scatter plots at lags, loess curves fit through time series, and
# regressions and mixed model fits to scatter plots:
source("assorted.plots.and.scatterplots.R")
# or run this for the big grid of lmer fits (but don't run both since
# they overwrite the same object names)
 source("lmer.grid.R")

# plot correlations and meta-analysis like the snow-crab cod paper:
source("snow.crab.style.correlation.plot.R")

# correlate individual predators by region and lag
source("ind.pred.reg.R")
