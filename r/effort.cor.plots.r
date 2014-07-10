#' correlation plot with effort quadratic rma models

# ggplot(q, aes(mean.temp, ri)) + geom_pointrange(aes(ymin = pi.lb, ymax = pi.ub)) +geom_hline(yintercept = 0, lty = 2) + facet_wrap(~lag)

# to re-run with only US effort series, you will probably also want to subset
# for those above 7.5 for predictions:
#dp.effort <- subset(curve_predictions, variable == "effort" & mean.temp > 7.5)

dp.effort <- subset(curve_predictions, variable == "effort")
dp.effort <- dp.effort[order(dp.effort$lag, dp.effort$variable), ]
pt.dat.effort <- transform(d.ri.cis, region = reorder(region, -mean.temp))

pdf("../fig/meta_analytic_curves_effort.pdf", width = 2.8, height = 6.5)
i <<- 1
par(mfcol = c(7, 1), mar = c(0,0,0,0), oma = c(8.5, 4, 2, 2), cex = 0.7, tck = 0.05, mgp = c(3, 0.3, 0))
d_ply(dp.effort, c("lag"), function(x) panel_func(x, the.pt.dat = pt.dat.effort, add.sig.star = c(999), print.lag = 1:7, add.axis2 = 1:6, add.axis2.bottom = 7, add.axis1 = 7))
mtext("Correlation", side = 2, outer = TRUE, line = 2.7, cex = 0.8, col = "grey30")
mtext(expression(Mean~temperature~(degree*C)), side = 1, outer = TRUE, line = 2.0, cex = 0.8, col = "grey30")
mtext("Effort", side = 3, outer = TRUE, adj = 0.5, cex = 0.8, col = "grey30", line = 0.3)
par(xpd = NA)
legend(x = 1.5, y = -2.0, legend = rev(levels(pt.dat.effort$region))[1:5], col = rev(region.cols)[1:5], lty = 1, bty = "n", pch = 20, text.col = "grey30")
legend(x = 10.5, y = -2.0, legend = rev(levels(pt.dat.effort$region))[6:length(unique(d$region))], col = rev(region.cols)[6:length(unique(d$region))], lty = 1, pch = 20, bty = "n", text.col = "grey30")
dev.off()
