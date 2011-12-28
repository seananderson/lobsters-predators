# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Aug 02, 2011
# Last modified: Aug 02, 2011
# Purpose:       show example of correlating some lobster predator
# data
# ====================================================================

library(plyr)
load("hom.dat.RData")

plot.lob.pred <- function(dat, selected.region, lob.variables, pred.variables) {
  if(length(lob.variables) != 4) stop("Are you sure you have spring and fall and male and female lobster values?")
  hom.region <- dat[dat$region == selected.region, ]

  hom.region.lob <- subset(hom.region, variable %in% c('year', lob.variables))
  hom.region.pred <- subset(hom.region, variable %in% c('year', pred.variables))

  mean.lob.abund <- ddply(hom.region.lob, "year", function(x) data.frame(lob.abund = ifelse(length(x$value) == length(lob.variables), sum(x$value)/2, NA)))
  total.pred.abund <- ddply(hom.region.pred, "year", function(x) data.frame(pred.abund = ifelse(length(x$value) == length(pred.variables), sum(x$value), NA)))

  hom.region.lob.pred <- merge(mean.lob.abund, total.pred.abund)
  hom.region.lob.pred[,2:3] <- log(hom.region.lob.pred[,2:3])

  plot(hom.region.lob.pred[,3:2], xlab = "log Predator abundance", ylab = "log Lobster abundance", pch = 19)
  m <- lm(lob.abund ~ pred.abund, data = hom.region.lob.pred)
  
  with(hom.region.lob.pred, curve(coef(m)[1] + coef(m)[2] * x, from = min(pred.abund), to = max(pred.abund), add = TRUE, lwd = 2, col = "red"))

}

#### now you can call it like this:
plot.lob.pred.whale(hom.dat, "Rhode Island", lob.variables = c('lob.abund.spr.m','lob.abund.spr.f','lob.abund.fall.m','lob.abund.fall.f'), pred.variables = c('cunner.abund.whale','longhorn.abund.whale','tautog.abund.whale','littleskate.abund.whale'))

ri.whale<-merge(x,y,by="year")