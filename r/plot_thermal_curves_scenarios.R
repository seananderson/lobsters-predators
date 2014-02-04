#' plot thermal curves for different conservation scenarios

source("thermal_curve_a.R")
source("gg_color_hue.R")
col_pal <- rev(gg_color_hue(10))
col_pal[c(4, 5, 6)] <- "#000000"
lwd <- c(2, 2, 2, 4, 4, 4, 4, 2, 2, 2)

pdf("../fig/optimal-curves.pdf", width = 5, height = 3)
par(cex = 0.6, mgp = c(1.0, 0.65, 0), tck = -0.02, mfrow = c(1,1), mar = c(0,2,0,.5), oma = c(2, 0, 2.8, 0))

optim_temps <- seq(9.5, 22.5, length.out = 10)
optim_temps[5] <- optim_temps[5] + diff(optim_temps)[1]/2

widths <- c(seq(0.07, 0.07, length.out = 5), rev(seq(0.07, 0.07, length.out = 5)))

heights <- c(seq(2.8, 2.8, length.out = 5), rev(seq(2.8, 2.8, length.out = 5)))

x <- seq(3, 29, length.out = 200)

plot_the_curves <- function(ids) {
  plot(1, 1, xlim = c(6, 26), ylim = c(0.02, 2.9), ylab = "", xlab = "", type = "n", yaxs = "i", las = 1, xaxt = "n", yaxt = "n", axes = FALSE)
  box(col = "grey40")
  for(i in ids) {
    lines(x, thermal_curve_a(x, optim_temp = optim_temps[i], max_a = heights[i], width_param = widths[i]), col = col_pal[i], lwd = lwd[i])
  }
}
#plot_the_curves(c(2, 4, 7, 9)) 
#plot_the_curves(c(4, 5, 6, 7)) 
#plot_the_curves(c(1, 2, 3, 4)) 
#plot_the_curves(c(7, 8, 9, 10)) 
plot_the_curves(c(5, 1, 2, 3, 8, 9, 10)) 
#plot_the_curves(c(5, 2, 9)) 
#axis(1)
par(xpd = NA)
mtext("Thermal tolerance", side = 2, outer = TRUE, line = -1.5, las = 0, col = "grey40")
mtext(expression(Temperature), side = 1, outer = FALSE, line = 0.6, las = 0, col = "grey40")

#abline(v = 16, lty = 2)
#abline(v = c(16-3.5, c = 16+3.5), lty = 3)


par(xpd = NA)
segments(16, 0.02, 16, 3.1, lty = "32", lwd = 2, col = "grey50")
segments(11, 0.02, 11, 3.1, lty = "32", lwd = 2, col = "grey50")
segments(21, 0.02, 21, 3.1, lty = "32", lwd = 2, col = "grey50")
#abline(v = 11, lty = "41", lwd = 2, col = "grey50")
#abline(v = 21, lty = "23", lwd = 2, col = "grey50")

par(xpd = NA)
text(11, 3.38, "Cool\nregions", col = "grey40")
text(21, 3.38, "Warm\nregions", col = "grey40")
text(16, 3.38, "Intermediate\nregions", col = "grey40")
  #axis(1)
  #axis(2)
dev.off()
