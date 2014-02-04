#' look at thermal tolerance curves of predators by region with data
#' from Malin

# first run down to calc.individual.predator.corr.R to get the data
# created

ptemp <- read.csv("predators-temp-range.csv", stringsAsFactor = FALSE)

#pred.d <- out
pred.d <- total.pred
names(pred.d)[2] <- "common"
pred.d <- join(pred.d, ptemp)
pred.d <- join(pred.d, region.temp)

pdf("../fig/predators-temp-ranges.pdf", width = 7, height = 6.5)
  par(mfrow = c(9, 1), mar = c(0,0,2,0), cex = 0.6, oma = c(5, .5, 1, .5))
  d_ply(pred.d, "median.temp", function(x) {
 with(x, plot(1,1,xlim= c(0, 16), ylim = c(0, 10), type = "n", xlab = "", ylab = "", axes = FALSE))
 with(x, abline(v = bottempmin, col = "#3C6BFF", lwd = prop.scaled*4))
 with(x, abline(v = bottempmax, col = "#FF3226", lwd = prop.scaled*4))
 with(x, abline(v = median.temp, col = "darkgrey", lwd = 3, lty = 1))
 with(x, rect(unique(q0.1.temp), -2, unique(q0.9.temp), 15, col = "#00000050", border = NA))
 box(col = "grey50")
 par(xpd = NA)
 with(x, text(bottempmin, rep(11, length(bottempmin)), labels = common, srt = 45))
 with(x, text(bottempmax, rep(11, length(bottempmin)), labels = common, srt = 45))
 par(xpd = FALSE)
 with(x, mtext(unique(region), cex = 0.9))


})
axis(1, col = "grey50")
mtext(expression(Bottom~temperature~(degree*C)), outer = TRUE, side = 1, line = 3.5, cex = 0.9)
dev.off()

write.csv(pred.d, file = "predators-temp-ranges-from-plot.csv", row.names = FALSE)



