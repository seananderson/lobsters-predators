# Created by:    Sean C. Anderson
# Created:       Feb 20, 2013
# Last modified: May 06, 2014
# Purpose:       plot of prediction curves


x <- subset(curve_predictions, variable == "predator" & lag == "3")
x.pts <- subset(pt.dat, variable == "predator" & lag == "3")

panel_func <- function(x, the.pt.dat, xlim = c(3.3, 16.8), add.axis2 = 1:8, add.axis2.bottom = 9, add.axis1 = c(9, 18, 27), print.lag = c(19:27), add.sig.star = c(3, 4, 5, 6, 12, 14, 15, 16, 17,18)) {
  current_lag <- x$lag[1]
  current_variable <- x$variable[1]
  axis.cols <- "grey40"
  with(x, plot(1, 1, xlim = xlim, ylim = c(-1, 1), xaxs = "i", yaxs = "i", type = "n", xlab = "", ylab = "", axes = FALSE))
  abline(h = 0, lty = 2, lwd = 1.5, col = "grey60")
  with(x, lines(mean.temp, pred, lwd = 1.5))
  x <- x[order(x$mean.temp), ]
  with(x, polygon(c(mean.temp, rev(mean.temp)), c(ci.lb, rev(ci.ub)), border = NA, col = "#00000020"))
  x.pt.dat <- subset(the.pt.dat, variable == current_variable & lag == current_lag)
  with(x.pt.dat, points(mean.temp, ri, col = region.cols[region], pch = 20, cex = 1.1))
  with(x.pt.dat, segments(mean.temp, pi.lb, mean.temp, pi.ub, lwd = 1, col = region.cols[region]))
  box(col = "grey40")
  if(i %in% add.axis2) axis(2, las = 1, at = c(-0.5, 0, 0.5, 1), col = axis.cols, col.axis = axis.cols)
  if(i %in% add.axis2.bottom) axis(2, las = 1, at = c(-1, -0.5, 0, 0.5, 1), col = axis.cols, col.axis = axis.cols)
  if(i %in% add.axis1) axis(1, col = axis.cols, col.axis = axis.cols, at = seq(4, 16, 4))
  if(i %in% print.lag) mtext(paste("Lag", x$lag[1]), side = 4, las = 0, col = "grey30", cex = 0.8, line = 0.5)
# highlight significant panels?
  # if(i %in% c(3, 4, 5, 14, 15, 16, 17)) rect(0, -2, 20, 2, border = NA, col = "#FFFF0015")
  if(i %in% add.sig.star) mtext("*", side = 3, adj = 0.035, line = -1.35, cex = 1.0, col = "grey20")
  # if(!i %in% c(3, 4, 5, 14, 15, 16, 17)) rect(0, -2, 20, 2, border = NA, col = "#00000015")
  # par(xpd = NA)
  # if(i %in% c(3, 4, 5, 14, 15, 16, 17)) box(col = "grey15", lwd = 2.2)
  i <<- i + 1
  # par(xpd = FALSE)
  # mtext(x$variable[1])
}

# library(RColorBrewer)
# region.cols <- brewer.pal(9, "Dark2")
gg_color_hue <- function(n) {
  # hues = seq(15, 375, length=n+1)
  hues = seq(8, 318, length=n+1)
  hcl(h=hues, l=57.5, c=100)[1:n]
}
region.cols <- gg_color_hue(length(unique(d$region)))

dp <- subset(curve_predictions, variable %in% c("predator", "nao", "temperature"))
dp <- dp[order(dp$lag, dp$variable), ]
var_order <- data.frame(variable = c("predator", "nao", "temperature"), var_order = c(1, 2, 3))
dp <- join(dp, var_order)
# dp <- transform(dp, variable_ordered = reorder(variable, var_order))
# pt.dat <- subset(d.ri.cis, variable %in% c("predator", "nao"))
pt.dat <- transform(d.ri.cis, region = reorder(region, -mean.temp))

pdf("../fig/meta_analytic_curves.pdf", width = 5.5, height = 8.5)
i <<- 1
par(mfcol = c(9, 3), mar = c(0,0,0,0), oma = c(10, 4, 2, 2), cex = 0.7, tck = 0.05, mgp = c(3, 0.3, 0))
d_ply(dp, c("var_order", "lag"), function(x) panel_func(x, the.pt.dat = pt.dat))
mtext("Correlation", side = 2, outer = TRUE, line = 2.7, cex = 0.8, col = "grey30")
mtext(expression(Mean~temperature~(degree*C)), side = 1, outer = TRUE, line = 2.7, cex = 0.8, col = "grey30")
mtext("Predators", side = 3, outer = TRUE, adj = 0.1, cex = 0.8, col = "grey30", line = 0.3)
mtext("NAOI", side = 3, outer = TRUE, adj = 0.5, cex = 0.8, col = "grey30", line = 0.3)
mtext("Temperature", side = 3, outer = TRUE, adj = 0.9, cex = 0.8, col = "grey30", line = 0.3)
par(xpd = NA)
legend(x = -16, y = -2.3, legend = rev(levels(pt.dat$region))[1:5], col = rev(region.cols)[1:5], lty = 1, bty = "n", pch = 20, text.col = "grey30")
legend(x = -3, y = -2.3, legend = rev(levels(pt.dat$region))[6:9], col = rev(region.cols)[6:9], lty = 1, pch = 20, bty = "n", text.col = "grey30")
dev.off()

