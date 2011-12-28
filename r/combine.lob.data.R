# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 26, 2011
# Purpose:       combine the lobster predator prey environment data by
# region, year, and data type
# ====================================================================

require(plyr)
library(reshape)

hom.dat.combined.1 <- ddply(hom.dat, c("region", "type", "year"), function(x) { 
    combined.value <- sum(x$value)
    units <- paste(x$units, collapse = " | ")
    variables <- paste(x$variable, collapse = " | ")
    data.frame(combined.value = combined.value, units = units, variables = variables)
})


  hom.dat.temp <- subset(hom.dat, region != "Massachusetts") # need to figure out how to combine these first
  hom.dat.temp <- hom.dat.temp[-which(hom.dat.temp$variable == "lob.bio" & hom.dat.temp$region == "SSBOF"), ] # we're using landings here

  hom.dat.combined <- ddply(hom.dat.temp, c("region", "type"), function(x) {
        #print(as.character(unique(x$region)))
        #print(unique(x$type))
        fall.f.check <- grep("fall.f", as.character(unique(x$variable)))
        if(unique(x$type) == "prey" & length(fall.f.check == 1)) { 
    #if the above is true then we assume we have fall and spring male and female abundances for lobster and need to combine them:
    lob.abund.rows <- x[c(grep("fall.f", x$variable), grep("fall.m", x$variable), grep("spring.f", x$variable), grep("spring.m", x$variable)), ]
# this next bit is getting rid of years without overlapping data:
    lob.abund.rows.wide <- cast(lob.abund.rows, year ~ variable, value = "value")
    lob.abund.rows.wide <- na.omit(lob.abund.rows.wide)
    lob.fall.f <- lob.abund.rows.wide[,grep("fall.f", names(lob.abund.rows.wide))]
    lob.fall.m <- lob.abund.rows.wide[,grep("fall.m", names(lob.abund.rows.wide))]
    lob.spring.f <- lob.abund.rows.wide[,grep("spring.f", names(lob.abund.rows.wide))]
    lob.spring.m <- lob.abund.rows.wide[,grep("spring.m", names(lob.abund.rows.wide))]
    lob.abund.index <- (lob.fall.f + lob.fall.m + lob.spring.f + lob.spring.m) / 2
    n <- length(lob.abund.index)
    to.return <- data.frame(year = lob.abund.rows.wide$year, combined.value =  lob.abund.index, units = rep("Abundance index", n), variables = "lob.abund.indices")
          } else {
## these need to be averaged over fall and spring:
          if(unique(x$region=="Rhode Island") & unique(x$type=="predator")) {
            #browser()
            x.wide <- cast(x, year ~ variable, value = "value")
            x.wide <- na.omit(x.wide)
    x.wide <- transform(x.wide, cod.abund.ridem = (cod.abund.ridem.spr + cod.abund.ridem.fall) / 2, smoothdog.abund.ridem = (smoothdog.abund.ridem.fall + smoothdog.abund.ridem.spr) / 2,  cunner.abund.ridem = (cunner.abund.ridem.fall + cunner.abund.ridem.spr) / 2, spinydog.abund.ridem = (spinydog.abund.ridem.fall + spinydog.abund.ridem.spr) / 2, longhorn.abund.ridem = (longhorn.abund.ridem.fall + longhorn.abund.ridem.spr) / 2, tautog.abund.ridem = (tautog.abund.ridem.fall + tautog.abund.ridem.spr) / 2, monk.abund.ridem = (monk.abund.ridem.fall + monk.abund.ridem.spr) / 2)
   x.wide <- x.wide[,-c(grep("fall", names(x.wide)), grep("spr", names(x.wide)))]
     year <- x.wide$year
            if(ncol(x.wide) > 2) # i.e. there are multiple series to combine
            combined.value <- rowSums(x.wide[,-1]) # this now has removed the years without overlapping data
          else combined.value <- x.wide[,2] # there's only one series
            units <- paste(unique(x$units), collapse = " | ")
            variables <- paste(unique(x$variable), collapse = " | ")
    to.return <- data.frame(year = year, combined.value = combined.value, units = units, variables = variables)           
          } else {
            # if the above wasn't true, then just sum the available indices
            x.wide <- cast(x, year ~ variable, value = "value")
            x.wide <- na.omit(x.wide)
            year <- x.wide$year
            if(ncol(x.wide) > 2) # i.e. there are multiple series to combine
            combined.value <- rowSums(x.wide[,-1]) # this now has removed the years without overlapping data
          else combined.value <- x.wide[,2] # there's only one series
            units <- paste(unique(x$units), collapse = " | ")
            variables <- paste(unique(x$variable), collapse = " | ")
    to.return <- data.frame(year = year, combined.value = combined.value, units = units, variables = variables)
    }}
        #print(to.return)
        return(to.return)
})

# the Connecticut temperature series needs to be divided by two since
# it should be an average of spring and fall:
hom.dat.combined[hom.dat.combined$region == "Connecticut" & hom.dat.combined$type == "environment", "combined.value"] <-   hom.dat.combined[hom.dat.combined$region == "Connecticut" & hom.dat.combined$type == "environment", "combined.value"] / 2

# the Rhode Island temperature series needs to be divided by 4 since
# it should be an average of 4 series
hom.dat.combined[hom.dat.combined$region == "Rhode Island" & hom.dat.combined$type == "environment", "combined.value"] <-   hom.dat.combined[hom.dat.combined$region == "Rhode Island" & hom.dat.combined$type == "environment", "combined.value"] / 4


# troubleshooting:
  #d_ply(hom.dat, c("region", "type"), function(x) {
        #print(as.character(unique(x$region)))
        #print(unique(x$type))

#})
  

# check for duplicate variable names:
#d_ply(hom.dat, c("region", "type", "year"), function(x) { 
                       #if(nrow(x) / length(unique(x$variable)) > 1){ 
                         #print(unique(as.character(x$region)))
                         #print(as.character(x$variable[duplicated(x$variable)]))
                       #}
#})

#write.csv(hom.dat.combined.1, "hom.dat.combined.original.csv", row.names = FALSE)

#write.csv(hom.dat.combined.1[,-c(3, 4)][!duplicated(hom.dat.combined[,-c(3, 4)]), ], file = "unique.types.units.csv", row.names = FALSE)

write.csv(hom.dat.combined, "hom.dat.combined.csv", row.names = FALSE)

#library(ggplot2)
#pdf("combined.data.check.pdf", width = 35, height = 10)
#print(ggplot(hom.dat.combined, aes(year, combined.value)) + geom_line() + facet_grid(type ~ region, scales = "free_y") + scale_x_continuous(breaks = c(1960, 1980, 2000)) )
#dev.off()


