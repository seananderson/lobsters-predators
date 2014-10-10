# Created by:    Sean C. Anderson
# Created:       Feb 20, 2012
# Last modified: Oct 10, 2014
# Purpose:       Lobster-predators-temperature-NAOI paper

library(ggplot2)
library(reshape)
library(plyr)
library(metafor)

# regions to import; these names must match the file names of the raw
# data .csv files:
regions <- c("Gulf of Maine", "s New England", "Georges Bank", "Massachusetts",
  "Rhode Island", "Connecticut", "s Gulf St Lawrence", "Nova Scotia",
  "Newfoundland")
# alternative version:
# regions <- c("Gulf of Maine", "s New England", "Georges Bank",
#   "Massachusetts", "Rhode Island", "Connecticut")

# The part of the analysis that reads and combines the raw data has been
# commented out because we don't have permission to share the raw data.
# Instead, begin with the analysis below that starts from the processed
# data.
# source("make.lagged.data.R")

# make effort lagged data:
# source("effort.correlations.r")

# plot the raw time series for publication:
# source("plot.time.series.R")

# reshape the data for use in the meta-analysis
# this combines the temperature, nao, predator, and prey time series
source("shape.data.for.meta.analysis.R")

# conduct the meta-analysis calculations:
rma.type <- "FE" # one of RE (random effect), FE (fixed effect), or
# MV (rma.mv with a specified variance structure)
source("meta.analytic.models.R")

# make exploratory plots (and create prediction data):
source("exploratory.plots.R")

# the main quadratic correlation lag plot:
source("meta.analytic.curves.plot.R")

# make effort correlation plots:
source("effort.cor.plots.r")

# check the residuals for spatial patterns:
source("check.spatial.autocorr.R")

# supplementary plot of correlations by predator:
source("calc.individual.predator.corr.R")

# make result tables:
source("result.tables.r")

# make the map:
source("map.r")
