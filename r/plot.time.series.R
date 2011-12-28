# ====================================================================
# Purpose:       plot the raw time series for publication
# ====================================================================

hom.dat.to.plot <- hom.dat.combined[-which(hom.dat.combined$type == "Exploitation" | hom.dat.combined$type == "Climate"), ]

# so we can order by mean temperature:
hom.dat.to.plot <- ddply(hom.dat.to.plot, "region", transform, mean.temp = mean(combined.value[type == "environment"]))

plot.order <- data.frame(type = c("prey", "predator", "environment"), type.plot.order = c(1, 2, 3))
hom.dat.to.plot <- merge(hom.dat.to.plot, plot.order)

# reorder for plotting:
hom.dat.to.plot <- transform(hom.dat.to.plot, ordered.type = reorder(type, type.plot.order))
hom.dat.to.plot <- transform(hom.dat.to.plot, ordered.region = reorder(region, mean.temp))
hom.dat.to.plot <- hom.dat.to.plot[order(hom.dat.to.plot$ordered.type, hom.dat.to.plot$ordered.region, hom.dat.to.plot$year), ]

n <- length(unique(hom.dat.to.plot$region))

pdf("../fig/hom-time-series.pdf", width = 7, height =10)
par(mfcol = c(n, 3))
par(mar = c(0, 3, 0, 0))
par(oma = c(4.5,1,2,3))
par(cex = 0.7)
par(mgp = c(1.6, .4, 0))
par(tck = -0.035)
i <<- 1

hom.dat.to.plot.1960 <- subset(hom.dat.to.plot, year >=1960)

d_ply(hom.dat.to.plot.1960, c("ordered.type", "ordered.region"), function(x) {
      par(xpd = NA)
current.units <- unique(x$units)[1]
if(current.units %in% c("deg C", "Deg C", "C | ", "mean C", "C")) current.units <- expression(degree*C)
  with(x, plot(year, combined.value, xlim = c(1960, 2010), axes = FALSE, ylab= current.units, xlab = "", lwd = 0.9, type = "l"))
  box(col = "darkgrey")
  axis(2, col = "darkgrey", col.axis = "grey30")
  text(par("usr")[1], par("usr")[4] - 0.1 * (par("usr")[4] - par("usr")[3]), unique(x$data.type), col = "grey30", pos = 4)
  if(i %in% c(n, n*2, n*3)) axis(1, col = "darkgrey", col.axis = "grey30", cex = 0.9)
  if(i == n*2+1) mtext("Temperature", side = 3, line = .5, col = "grey30", cex = 0.9)
  if(i == n+1) mtext("Predators", side = 3, line = .5, col = "grey30", cex = 0.9)
  if(i == 1) mtext("Lobsters", side = 3, line = .5, col = "grey30", cex = 0.9)
  if(i %in% seq(n*2+1, n*3, 1)) mtext(unique(x$region), side = 4, line = 1.2, col = "grey30", cex = 0.9)
  i <<- i + 1
})
mtext("Year", side = 1, outer = TRUE, line = 2.75, col = "grey30")
dev.off()



