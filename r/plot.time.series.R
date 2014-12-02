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
hom.dat.combined.effort <- subset(hom.dat.combined, type == "effort")[,c("region", "year", "combined.value")]
names(hom.dat.combined.effort)[3] <- "effort.value"
hom.dat.to.plot <- merge(hom.dat.to.plot, hom.dat.combined.effort, all.x = TRUE)
#write.csv(hom.dat.to.plot, file = "../data/lobster-dat.csv")

pdf("../fig/hom-time-series-effort.pdf", width = 7, height =10.5)
par(mfcol = c(n, 3))
par(mar = c(0, 3, 0, 0))
par(oma = c(4.5,1,2,3))
par(cex = 0.7)
par(mgp = c(1.6, .4, 0))
par(tck = 0.035)
i <<- 1

hom.dat.to.plot.1960 <- subset(hom.dat.to.plot, year >=1960)

d_ply(hom.dat.to.plot.1960, c("ordered.type", "ordered.region"), function(x) {
  par(xpd = NA)
  current.units <- unique(x$units)[1]

cv <- x$combined.value
cu <- current.units
data_type <- x$data.type[1]
if(data_type == "landings") {
  data_type <- "Landings"
}
if(data_type == "trawlsurvey") {
  data_type <- "Trawl survey"
}
if(data_type == "stockassessment") {
  data_type <- "Stock assessment"
}
if(cu == "Million" | cu == "Millions") {
  cv <- cv / 1e6
  cu <- "Million"
}
if(cu == "t") {
  cu <- "1000 t"
  cv <- cv / 1000
}
  if(cu %in% c("deg C", "Deg C", "C | ", "mean C", "C")) cu <- expression(degree*C)

  if(unique(x$type) == "environment")
    ylim <- c(min(cv)*0.9, max(cv)*1.1)
  else
    ylim <- c(0, max(cv) * 1.20)

  with(x, plot(year, cv, xlim = c(1960, 2010), axes = FALSE, ylab= cu, ylim = ylim, xlab = "", lwd = 0.9, type = "l", yaxs = "i"))
  at <- axTicks(side = 2)
  at <- at[seq(1, length(at), 2)]
  if(max(at) > 0.95*ylim[2]) at <- at[-length(at)]
  axis(2, at = at, col = "darkgrey", col.axis = "grey30", las = 2)
  if(unique(x$type == "prey")) {
    par(new = TRUE)
    with(x, plot(year, effort.value, xlim = c(1960, 2010), axes = FALSE, ylab= "", xlab = "", lwd = 0.9, type = "l", lty = 2, ylim = c(0, max(effort.value, na.rm = TRUE) * 1.1)))
    max.traps <- subset(x, effort.value == max(effort.value, na.rm = TRUE))
    max.traps <- max.traps[nrow(max.traps), ]
    text.y <- max.traps$effort.value * 0.98
    text.x <- max.traps$year
    if(i == 2) {
      text.y <- text.y * 1.09
      text.x <- text.x -10
    }
    if(i == 3) {
      text.y <- text.y * 1.05
    }
    if(i == 4) {
      text.y <- text.y * 1.08
      text.x <- text.x -6
    }
    if(i == 5) {
      text.y <- text.y * 1.08
    }
    text(text.x, text.y,round(max.traps$effort.value/1000, 0), col = "grey40", pos = 4)
    par(new = FALSE)
  }
  box(col = "darkgrey")
  text(par("usr")[1], par("usr")[4] - 0.1 * (par("usr")[4] - par("usr")[3]), data_type, col = "grey30", pos = 4)
  if(i %in% c(n, n*2, n*3)) axis(1, col = "darkgrey", col.axis = "grey30", cex = 0.8)
  if(i == n*2+1) mtext("Temperature", side = 3, line = .5, col = "grey30", cex = 0.8)
  if(i == n+1) mtext("Predators", side = 3, line = .5, col = "grey30", cex = 0.8)
  if(i == 1) mtext("Lobsters", side = 3, line = .5, col = "grey30", cex = 0.8)
  if(i %in% seq(n*2+1, n*3, 1)) mtext(unique(x$region), side = 4, line = 1.2, col = "grey30", cex = 0.8)
  i <<- i + 1
})
mtext("Year", side = 1, outer = TRUE, line = 2.75, col = "grey30")
dev.off()



