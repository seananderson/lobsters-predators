# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 26, 2011
# Purpose:       combine the lobster predator prey environment data by
# region, year, and data type, this time use the new average row
# ====================================================================

combined.lob.data <- function(dat = hom.dat, type = "lob.pred.climate", save.file.name = "hom.dat.combined"){

require(plyr)
require(reshape)


#browser()
## split up Rhode Island:
#hom.ri <- subset(dat, region == "Rhode Island")
#hom.fox <- hom.ri[grep("fox", hom.ri$variable), ]
#hom.whale <- hom.ri[grep("whale", hom.ri$variable), ]
#hom.ridem <- hom.ri[grep("ridem", hom.ri$variable), ]

#hom.ri.lob <- hom.ri[grep("lob", hom.ri$variable), ]

#hom.fox <- rbind(hom.fox, hom.ri.lob)
#hom.whale <- rbind(hom.whale, hom.ri.lob)
#hom.ridem <- rbind(hom.ridem, hom.ri.lob)

#hom.fox$region <- "Fox"
#hom.whale$region <- "Whale"
#hom.ridem$region <- "RIDEM"

#dat <- dat[-which(dat$region == "Rhode Island"), ]
#dat <- rbind(dat, hom.fox, hom.whale, hom.ridem)
## end splitting up Rhode Island

## average some of the data:
dat.to.average <- subset(dat, average.var.name != "")
dat.no.average <- subset(dat, average.var.name == "")

dat.averaged <- ddply(dat.to.average, c("region", "type", "year", "average.var.name"), function(x) data.frame(value = mean(x$value), category = unique(x$category)[1], data.type = unique(x$data.type)[1], units = unique(x$units)[1] ,variable = unique(x$average.var.name)[1]))
dat <- merge(dat.no.average, dat.averaged, all = TRUE)
dat$average.var.name <- NULL

if(type == "lob.pred.climate") {
## combine by variable:
dat.combined <- ddply(dat, c("region", "type", "year"), function(x) {
    combined.value <- sum(x$value)
    units <- paste(unique(x$units), collapse = ", ")
    data.type <- paste(unique(x$data.type), collapse = ", ")
    variables <- paste(unique(x$variable), collapse = " , ")
    data.frame(combined.value = combined.value, units = units, data.type = data.type, variables = variables)
})
} else {
# some are blank:
  dat$data.type[dat$data.type == ""] <- dat$type[dat$data.type == ""]
dat.combined <- ddply(dat, c("region", "data.type", "year"), function(x) {
    combined.value <- sum(x$value)
    units <- paste(unique(x$units), collapse = ", ")
    data.type <- paste(unique(x$data.type), collapse = ", ")
    variables <- paste(unique(x$variable), collapse = " , ")
    data.frame(combined.value = combined.value, units = units, variables = variables)
})
}

write.csv(dat.combined, paste(save.file.name, ".csv", sep = ""), row.names = FALSE)
invisible(dat.combined)

#library(ggplot2)
#pdf("combined.data.check.pdf", width = 35, height = 10)
#print(ggplot(dat.combined, aes(year, combined.value)) + geom_line() + facet_grid(type ~ region, scales = "free_y") + scale_x_continuous(breaks = c(1960, 1980, 2000)) )
#dev.off()


}
