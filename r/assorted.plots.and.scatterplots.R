# ====================================================================
# Created:       Nov 25, 2011
# Last modified: Dec 19, 2011
# Purpose:       scatter plots, time series plots, mostly exploratory
# with ggplot
# ====================================================================

### a bunch of plots:
#pdf("../fig/scatterplot-lags-predator.pdf", width = 8, height = 8)
#print(ggplot(d.scaled, aes(predator.lag00, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag01, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag02, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag03, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag04, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag05, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag06, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag07, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag08, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag09, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag10, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(predator.lag11, prey)) + geom_point() + facet_wrap(~region))
#dev.off()
#pdf("../fig/scatterplot-lags-environment.pdf", width = 8, height = 8)
#print(ggplot(d.scaled, aes(environment.lag00, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag01, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag02, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag03, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag04, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag05, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag06, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag07, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag08, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag09, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag10, prey)) + geom_point() + facet_wrap(~region))
#print(ggplot(d.scaled, aes(environment.lag11, prey)) + geom_point() + facet_wrap(~region))
#dev.off()


region.temp <- ddply(subset(junk.melt, variable == "environment.lag08"), c("region"), summarize, mean.temp = mean(value))
junk.scaled <- merge(junk.scaled, region.temp)
junk.scaled$region <- as.factor(junk.scaled$region)
junk.scaled <- transform(junk.scaled, region.ordered.by.mean.temp = reorder(region, -mean.temp))

temp <- subset(junk.scaled, variable == "prey")
temp.without.prey <- subset(junk.scaled, variable != "prey") 
names(temp)[4] <- "prey.value.scaled"
#names(temp)[5] <- "prey.value"
temp$variable <- NULL
temp$mean.temp <- NULL
temp$region.ordered.by.mean.temp <- NULL

j7 <- merge(temp.without.prey, temp, all = TRUE)
j7 <- transform(j7, region.ordered.by.mean.temp.rev = reorder(region, mean.temp))


#### other plotting stuff:
j2 <- subset(junk.scaled, variable %in% c("prey", "environment.lag08", "predator.lag08"))
j2$variable <- factor(j2$variable)
#ggplot(j2, aes(year, value.scaled)) + geom_line() + facet_grid(variable~region)

region.temp <- ddply(subset(junk.melt, variable == "environment.lag08"), c("region"), summarize, mean.temp = mean(value))

junk.scaled <- merge(junk.scaled, region.temp)
junk.scaled$region <- as.factor(junk.scaled$region)
junk.scaled <- transform(junk.scaled, region.ordered.by.mean.temp = reorder(region, -mean.temp))

j2 <- subset(junk.scaled, variable %in% c("prey", "environment.lag08", "predator.lag08"))
j2$variable <- factor(j2$variable)
p <- ggplot(j2, aes(year, value.scaled,colour = variable)) + geom_line() + facet_grid(variable~region.ordered.by.mean.temp) + geom_smooth(col = "#00000050")
ggsave("../fig/lag8-grid-ordered-by-temp.pdf", plot= p, width = 16, height = 6)

p <- ggplot(j2, aes(year, value.scaled,colour=variable)) + geom_point() + facet_wrap(~region.ordered.by.mean.temp) +stat_smooth(se=FALSE, lwd = 1)
ggsave("../fig/lag8-wrap-ordered-by-temp.pdf", plot= p, width = 9, height = 8)

#j2.wide <- cast(j2, region + year +region.ordered.by.mean.temp +mean.temp ~ variable, value = "value.scaled")
#j2.wide <- cast(j2, region + year +region.ordered.by.mean.temp +mean.temp ~ variable, value = "value.scaled")
#ggplot(j2.wide, aes(predator.lag8, prey)) + geom_point() + facet_wrap(~region.ordered.by.mean.temp)

j4 <- subset(j2, variable == "prey")
j4$variable <- NULL
j4$mean.temp <- NULL
names(j4)[3] <- "prey.value.scaled"
j3 <- merge(subset(j2, variable != "prey"), j4)
j3$variable <- factor(j3$variable)

#ggplot(j3, aes(value.scaled, prey.value.scaled)) +  facet_grid(variable~region.ordered.by.mean.temp) + stat_smooth(col = "#00000050") + geom_point()

#ggplot(j3, aes(value.scaled, prey.value.scaled, col = variable)) +  facet_grid(variable~region.ordered.by.mean.temp) + stat_smooth(method = "lm", col = "#00000050") + geom_point()


fm5 <- lmer(prey ~ predator.lag08 + (1 + predator.lag08|region), data=d.scaled)
fm5.env <- lmer(prey ~ environment.lag08 + (1 + environment.lag08|region), data=d.scaled)

fm5.coef <- coef(fm5)$region
fm5.env.coef <- coef(fm5.env)$region

fm5.coef$region <- row.names(fm5.coef)
fm5.env.coef$region <- row.names(fm5.env.coef)

names(fm5.coef) <- c("lmer.inter", "lmer.lag08.slope", "region")
names(fm5.env.coef) <- c("lmer.inter", "lmer.lag08.slope", "region")

row.names(fm5.coef) <- NULL
row.names(fm5.env.coef) <- NULL

fm5.coef$variable <- "predator.lag08"
fm5.env.coef$variable <- "environment.lag08"

#fm5.combined <- merge(fm5.coef, fm5.env.coef)
fm5.combined <- rbind(fm5.env.coef, fm5.coef)

j6 <- merge(j3, fm5.combined, all = TRUE)

temp <- subset(junk.melt, variable == "environment.lag08")
names(temp)[4] <- "abs.temp"
temp$variable <- NULL
j6 <- merge(j6, temp, all = TRUE)


p <- ggplot(j6, aes(value.scaled, prey.value.scaled, col = abs.temp)) +  facet_grid(variable~region.ordered.by.mean.temp) + stat_smooth(method = "lm", col = "#000000", lty = 1) + geom_point() + geom_abline(aes(intercept = lmer.inter, slope = lmer.lag08.slope), lty = 2)
ggsave("../fig/lag8-scattered-grid-ordered-by-temp.pdf", plot = p, width = 12, height = 5.5)



