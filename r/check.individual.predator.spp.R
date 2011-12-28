# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Month 00, 2011
# Last modified: Dec 07, 2011
# Purpose:       check the correlations with individual predator
# species
# ====================================================================
hom.dat.pred <- subset(hom.dat, type == "predator")[,c("variable", "year", "value", "region")]
names(hom.dat.pred)[c(1, 3)] <- c("predator.variable", "predator.value")

hom.dat.lob <- subset(hom.dat, type == "prey")[,c("year", "value", "region")]
names(hom.dat.lob)[c(2)] <- c("prey.value")
#hom.dat.lob$year <- hom.dat.lob$year - 8 # I hope this is the correct direction!!
hom.dat.lob <- ddply(hom.dat.lob, c("year", "region"), summarize, prey.value = mean(prey.value)) # for those cases where they need to be averaged

hom.dat.env <- subset(hom.dat, type == "environment")[,c("year", "value", "region")]
names(hom.dat.env)[c(2)] <- c("env.value")

hom.dat.ind.spp <- merge(hom.dat.pred, hom.dat.lob)
hom.dat.ind.spp <- merge(hom.dat.ind.spp, hom.dat.env)

hom.dat.ind.spp <- transform(hom.dat.ind.spp, predator.value = log(predator.value+0.5), prey.value = log(prey.value+0.5), env.value = scale(env.value))

#pdf("../fig/individual-predators-vs-lobster.pdf", width = 10, height = 10)
#for(region.i in unique(hom.dat.ind.spp$region)) {
#p <- ggplot(subset(hom.dat.ind.spp, region ==region.i), aes(predator.value, prey.value)) + geom_point() + facet_wrap(~predator.variable, scales = "free") + opts(title = region.i)
#print(p)
#}
#dev.off()


#individual predators with lobster, ccf
out <- ddply(hom.dat.ind.spp, c("region", "predator.variable"), function(x) {
  y<-ccf(x$predator.value, x$prey.value, plot = FALSE, lag.max = 9)
  clim <- qnorm((1 + .95)/2)/sqrt(y$n.used)
  data.frame(lag = y$lag, r = as.numeric(y$acf), clim.u = clim, clim.l = -clim)
})

# only take the lags we are interested in:
out <- subset(out, lag <= 2)
# and flip the lags to be positive:
out <- transform(out, lag = lag * -1)


pdf("../fig/individual-predators-vs-lobster-ccf2.pdf", width = 9, height = 9)
for(region.i in unique(out$region)) {
  print(region.i)
print(ggplot(subset(out, region == region.i), aes(lag, r)) + facet_wrap(~predator.variable) + geom_linerange(aes(lag, ymin = 0, ymax = r)) + opts(title = region.i) + geom_hline(aes(yintercept = clim.u), lty = 2) + geom_hline(aes(yintercept = clim.l), lty = 2))
}
dev.off()

