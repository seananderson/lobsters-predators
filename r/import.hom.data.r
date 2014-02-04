# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Nov 8, 2010
# Modified:      Mar 29, 2011
# Purpose:       experiment with reading in a data file
# ====================================================================

# contains read.xls()
# needs to have Perl installed
#require(gdata) 

# read in a sample data sheet:
#setwd("~/Dropbox/Lobsters_Predators/r/")
#setwd("~/Dropbox/Lobsters_Predators/r/")
 #Steph
#getwd()
#setwd("../r") FYI, .. means go down a folder, / change folder to

#hom <- list()


#for(i in 8){


import.hom.data <- function(sheet, include.row.name = "Lobster-predators"){
d <- read.csv(paste("../data/", sheet, ".csv", sep = ""), stringsAsFactors = FALSE, strip.white = TRUE) 

starting.row <- (1:nrow(d))[d[,1] == "Long name"]
d <- d[-c(1:starting.row), ]

  if(!"Type" %in% d[,1]){    
    stop(paste(sheet, "does not have the row Type after import."))
  }
    
  
  # figure out what rows we need:
include.row <- (1:nrow(d))[d[,1] == include.row.name]
#print(include.row)
col.names.row <- (1:nrow(d))[d[,1] == "Short name"]  
type.row <- (1:nrow(d))[d[,1] == "Type"] 
category.row <- (1:nrow(d))[d[,1] == "Category"] 
units.row <- (1:nrow(d))[d[,1] == "Units"] 
average.row <- (1:nrow(d))[d[,1] == "Average"] 
data.type.row <- (1:nrow(d))[d[,1] == "Data"] 

# only use those with a "1" in the include row:
names(d) <- NULL
d <- d[, as.character(d[include.row,]) == "1" & !is.na(d[include.row,])]

# get the short column names:
col.names <- as.character(as.matrix(d[col.names.row,]))
types <- as.character(as.matrix(d[type.row,]))   
categories <- as.character(as.matrix(d[category.row,]))
data.type <- as.character(as.matrix(d[data.type.row,]))
units <- as.character(as.matrix(d[units.row,]))
average.var.name <- as.character(as.matrix(d[average.row,]))

# keep only the included columns:
included.columns <- (1:ncol(d))[as.character(d[include.row,]) == "1"]

col.names <- col.names[included.columns]
types <- types[included.columns]
categories <- categories[included.columns]
average.var.name <- average.var.name[included.columns]
data.type <- data.type[included.columns]

# get rid of the meta-data rows:
d <- d[-c(1:(include.row)), ]

# use the pretty short names for the columns:
names(d) <- col.names
                                                           
# make the value into numeric values (were character or factor):
for(i in 1:ncol(d)) d[,i] <- as.numeric(as.character(d[,i]))
                                                                                                    
# reset the row numbers to keep things pretty: (renumber from 1 to
# whatever)
rownames(d) <- NULL

#region <-  names(read.xls("../data/trawl-surveys-lobster.xls", sheet = sheet, stringsAsFactors = FALSE, check.names = FALSE, nrows = 1))[1] 
region <- sheet

types.names.df <- data.frame(variable = col.names, type = types, category = categories, units = units, region = region, average.var.name = average.var.name, data.type = data.type)

types.names.df <- types.names.df[-1,] # removing year, not needed


require(reshape)

  #browser()
hom<-melt(d,id="year",na.rm=TRUE) #new data frame, by year. Going from wide to long format

hom <- merge(hom, types.names.df)

hom
}

