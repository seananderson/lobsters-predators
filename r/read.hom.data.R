# ====================================================================
# Purpose:       Read in the lobster data from the spreadsheet. This
# can be a bit slow. The data can be quickly loaded later by
# load("hom.dat.RData").
# ====================================================================

# read in all the data:
# (each sheet gets read in to a list and then it gets turned into a
# dataframe at the end)
source("import.hom.data.R") # contains a function for importing the data

read.hom.dat <- function(save.file.name = "hom.dat", include.row.name = "Lobster-predators"){

hom.dat <- list()
for(i in 1:length(regions)) {
  hom.dat[[i]] <- import.hom.data(sheet = regions[i], include.row.name = include.row.name)
}
hom.dat <- do.call("rbind", hom.dat) # a fancy way of turning a list into a dataframe

# just making sure none are factors:
# mostly unnecessary 
hom.dat$type <- as.character(hom.dat$type)
hom.dat$average.var.name <- as.character(hom.dat$average.var.name)
hom.dat$type <- as.character(hom.dat$type)
hom.dat$variable <- as.character(hom.dat$variable)
hom.dat$region <- as.character(hom.dat$region)
hom.dat$category <- as.character(hom.dat$category)
hom.dat$data.type <- as.character(hom.dat$data.type)
hom.dat$year <- as.numeric(hom.dat$year)
hom.dat$value <- as.numeric(hom.dat$value)
# remove extra spaces at the end of the "type" column:
hom.dat$type <- sub(" ", "", hom.dat$type)
hom.dat$average.var.name <- sub(" ", "", hom.dat$average.var.name)
hom.dat$category <- sub(" ", "", hom.dat$category)
hom.dat$data.type <- sub(" ", "", hom.dat$data.type)
hom.dat$variable <- sub(" ", "", hom.dat$variable)

# save it so we can read it in quickly later:
save(hom.dat, file = paste(save.file.name, ".RData", sep = ""))
write.csv(hom.dat, file = paste(save.file.name, ".csv", sep = ""), row.names = FALSE)
invisible(hom.dat)
}

