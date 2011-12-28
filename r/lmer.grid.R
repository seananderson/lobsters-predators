# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Nov 29, 2011
# Last modified: Dec 19, 2011
# Purpose:       do the lmer grid plot figure for lots of lags
# ====================================================================

junk.scaled$mean.temp <- NULL # not sure where this came from it is different TODO look into this, was causing problems on merge because already existed
region.temp <- ddply(subset(junk.melt, variable !="prey" & variable == "environment.lag00"), c("region"), summarize, mean.temp = mean(value))
junk.scaled <- merge(junk.scaled, region.temp)
junk.scaled$region <- as.factor(junk.scaled$region)
junk.scaled <- transform(junk.scaled, region.ordered.by.mean.temp = reorder(region, mean.temp))

get.lmer.slopes <- function(data = d.scaled, lag = 8, type = "predator"){ 
  if(type == "predator")
    column.name <- paste("predator.lag", lag, sep = "")
if(type == "environment")
  column.name <- paste("environment.lag", lag, sep = "")
if(type != "predator" & type != "environment")
  stop("Type must be either predator or environment")

m <- lmer(prey ~ d.scaled[ ,column.name] + (1 + d.scaled[,column.name]|region), data=d.scaled)
m.coef <- coef(m)$region
m.coef$region <- row.names(m.coef)
names(m.coef) <- c("intercept", "slope", "region")
row.names(m.coef) <- NULL
m.coef$variable <- column.name
return(m.coef)
}

lags <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11")
#j6 <- merge(j3, fm5.combined, all = TRUE)
pred.lmer.slopes <- ldply(lags, function(x) get.lmer.slopes(lag = x, type = "predator"))
pred.lmer.slopes$type <- "predator"
env.lmer.slopes <- ldply(lags, function(x) get.lmer.slopes(lag = x, type = "environment"))
env.lmer.slopes$type <- "environment"
#lmer.slopes <- rbind(pred.lmer.slopes, env.lmer.slopes)

d.scaled.without.prey <- subset(junk.scaled, variable != "prey")
d.scaled.prey.only <- subset(junk.scaled, variable == "prey")
names(d.scaled.prey.only)[names(d.scaled.prey.only) == "value.scaled"] <- "prey.value.scaled"
d.scaled.prey.only$variable <- NULL

d.scaled2 <- merge(d.scaled.without.prey, d.scaled.prey.only)

d.scaled2.pred <- merge(d.scaled2, pred.lmer.slopes)
d.scaled2.env <- merge(d.scaled2, env.lmer.slopes)

#d.scaled2.pred <- subset(d.scaled2, type = "predator")
#d.scaled2.env <- subset(d.scaled2, type = "environment")
#d.scaled2.pred$variable <- factor(d.scaled2.pred$variable)
#d.scaled2.env$variable <- factor(d.scaled2.env$variable)

library(ggplot2)
#p <- ggplot(subset(d.scaled2, type = "predator"), aes(year, value.scaled,colour=variable)) + geom_point() + facet_grid(variable~region.ordered.by.mean.temp) +stat_smooth(se=FALSE, lwd = 1)
p <- ggplot(d.scaled2.pred, aes(value.scaled, prey.value.scaled, colour = slope)) +  facet_grid(region.ordered.by.mean.temp~variable) + stat_smooth(method = "lm", col = "#000000", lty = 3) + geom_point() + geom_abline(aes(intercept = intercept, slope = slope, width = 1), lty = 1)
ggsave("../fig/lmer-grid-predator-ordered-by-temp.pdf", plot = p, width = 16, height = 12)


p <- ggplot(d.scaled2.env, aes(value.scaled, prey.value.scaled, colour = slope)) +  facet_grid(region.ordered.by.mean.temp~variable) + stat_smooth(method = "lm", col = "#000000", lty = 3) + geom_point() + geom_abline(aes(intercept = intercept, slope = slope, width = 1), lty = 1)
ggsave("../fig/lmer-grid-environment-ordered-by-temp.pdf", plot = p, width = 16, height = 12)
