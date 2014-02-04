# ====================================================================
# Created:       Nov 25, 2011
# Last modified: Apr 01, 2013
# Purpose:       create a data frame with lagged data
# ====================================================================

d <- cast(hom.dat.combined, region + year ~ type, value = "combined.value")

#### correlations at different lags:
junk <- ddply(d, "region", transform,                                                                                                     
  environment.lag00 = c(rep(NA, 0), environment[1:(length(environment)-0)]), predator.lag00 = c(rep(NA, 0), predator[1:(length(predator) - 0)]), climate.lag00 = c(rep(NA, 0), Climate[1:(length(Climate)-0)]),  
  environment.lag01 = c(rep(NA, 1), environment[1:(length(environment)-1)]), predator.lag01 = c(rep(NA, 1), predator[1:(length(predator) - 1)]), climate.lag01 = c(rep(NA, 1), Climate[1:(length(Climate)-1)]), 
  environment.lag02 = c(rep(NA, 2), environment[1:(length(environment)-2)]), predator.lag02 = c(rep(NA, 2), predator[1:(length(predator) - 2)]), climate.lag02 = c(rep(NA, 2), Climate[1:(length(Climate)-2)]), 
    environment.lag03 = c(rep(NA, 3), environment[1:(length(environment)-3)]), predator.lag03 = c(rep(NA, 3), predator[1:(length(predator) - 3)]), climate.lag03 = c(rep(NA, 3), Climate[1:(length(Climate)-3)]),
  environment.lag04 = c(rep(NA, 4), environment[1:(length(environment)-4)]), predator.lag04 = c(rep(NA, 4), predator[1:(length(predator) - 4)]), climate.lag04 = c(rep(NA, 4), Climate[1:(length(Climate)-4)]),
  environment.lag05 = c(rep(NA, 5), environment[1:(length(environment)-5)]), predator.lag05 = c(rep(NA, 5), predator[1:(length(predator) - 5)]), climate.lag05 = c(rep(NA, 5), Climate[1:(length(Climate)-5)]), 
  environment.lag06 = c(rep(NA, 6), environment[1:(length(environment)-6)]), predator.lag06 = c(rep(NA, 6), predator[1:(length(predator) - 6)]), climate.lag06 = c(rep(NA, 6), Climate[1:(length(Climate)-6)]), 
  environment.lag07 = c(rep(NA, 7), environment[1:(length(environment)-7)]), predator.lag07 = c(rep(NA, 7), predator[1:(length(predator) - 7)]), climate.lag07 = c(rep(NA, 7), Climate[1:(length(Climate)-7)]), 
  environment.lag08 = c(rep(NA, 8), environment[1:(length(environment)-8)]), predator.lag08 = c(rep(NA, 8), predator[1:(length(predator) - 8)]), climate.lag08 = c(rep(NA, 8), Climate[1:(length(Climate)-8)])
  #environment.lag09 = c(rep(NA, 9), environment[1:(length(environment)-9)]), predator.lag09 = c(rep(NA, 9), predator[1:(length(predator) - 9)]), climate.lag09 = c(rep(NA, 9), Climate[1:(length(Climate)-9)]), 
  #environment.lag10 = c(rep(NA, 10), environment[1:(length(environment)-10)]), predator.lag10 = c(rep(NA, 10), predator[1:(length(predator) - 10)]), climate.lag10 = c(rep(NA, 10), Climate[1:(length(Climate)-10)]), 
  #environment.lag11 = c(rep(NA, 11), environment[1:(length(environment)-11)]), predator.lag11 = c(rep(NA, 11), predator[1:(length(predator) - 11)]), climate.lag11 = c(rep(NA, 11), Climate[1:(length(Climate)-11)]) 
)

junk$Climate <-NULL
junk$Exploitation <-NULL
junk$environment <-NULL
junk$predator <-NULL

## if we're not using effort, then remove the columns that have
## "effort" in them. Otherwise we'll end up removing lots of NA rows
## unnecessarily:
if(use.effort == FALSE)
junk <- junk[,-grep("effort", names(junk))]

# use NAO or temperature?
if(use.NAO) {
  junk <- junk[, -c(grep("environment", names(junk)))]
  names(junk) <- gsub("climate", "environment", names(junk))
} else {
  junk <- junk[, -c(grep("climate", names(junk)))]
}

junk <- na.omit(junk)

junk.melt <- melt(junk, id.vars = c("region", "year"))
if(use.NAO){
junk.melt.env <- subset(junk.melt, grepl("env", variable))
junk.melt.no.env <- subset(junk.melt, grepl("env", variable) == FALSE)
junk.melt.no.env <- transform(junk.melt.no.env, value = log(value))
junk.melt2 <- rbind(junk.melt.no.env, junk.melt.env)
} else {
  junk.melt2 <- junk.melt
}

#if(use.NAO) browser()
junk.scaled <- ddply(junk.melt2, c("region", "variable"), transform, value.scaled = scale.dat(value))
junk.scaled$value <- NULL

#junk.scaled <- transform(junk.scaled, region.ordered.by.mean.temp = reorder(region, -mean.temp))

d.scaled <- cast(junk.scaled, region + year ~ variable, value = "value.scaled")
d.scaled <- na.omit(d.scaled)

