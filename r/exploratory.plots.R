# Created by:    Sean C. Anderson
# Created:       Feb 20, 2013
# Last modified: Aug 26, 2013
# Purpose:       meta-analysis plots - all of them

p <-  ggplot(d.ri, aes(ni.raw, ni, colour = region)) + geom_point() + facet_grid(~variable)
 ggsave("../fig/pyper_autocor_adjust.pdf")

# BLUPS:
p <- ggplot(out, aes(lag, pred, colour = variable)) + geom_pointrange(aes(ymin = pi.lb, ymax = pi.ub), position = position_dodge(width = 0.3)) + facet_wrap(~region) + geom_hline(yintercept = 0, lty = 2)
ggsave("../fig/BLUPS.pdf")

p <- ggplot(d.ri.cis, aes(lag, ri, colour = variable)) + geom_pointrange(aes(ymin = pi.lb, ymax = pi.ub), position = position_dodge(width = 0.3)) + facet_wrap(~region) + geom_hline(yintercept = 0, lty = 2)
ggsave("../fig/non_BLUPS.pdf")

p <- ggplot(out.ma, aes(lag, ma.pred, colour = variable)) + geom_pointrange(aes(ymin = ma.ci.lb, ymax = ma.ci.ub), position = position_dodge(width = 0.3)) + geom_hline(yintercept = 0, lty = 2)
ggsave("../fig/rma.pdf")

p <- ggplot(out, aes(lag, pred, colour = region)) + geom_line() + facet_wrap(~variable, ncol = 1) + geom_hline(yintercept = 0, lty= 2) + geom_pointrange(data = out.ma, aes(lag, ma.pred, ymin = ma.ci.lb, ymax = ma.ci.ub ), colour = "black")
ggsave("../fig/rma_with_BLUPS.pdf")

p <- ggplot(d.ri.cis, aes(lag, ri, colour = region)) + geom_line() + facet_wrap(~variable, ncol = 1) + geom_hline(yintercept = 0, lty= 2) + geom_pointrange(data = out.ma, aes(lag, ma.pred, ymin = ma.ci.lb, ymax = ma.ci.ub ), colour = "black") + theme_bw() + ylab("Correlation") + xlab("Lag (years)")
ggsave("../fig/rma_with_non_BLUPS.pdf", width = 6.5, height = 6.5)

#out <- join(out, region.temp)
p <- ggplot(d.ri.cis, aes(mean.temp, ri, colour = region)) + geom_point() + facet_grid(~variable) + geom_hline(yintercept = 0, lty = 2)
ggsave("../fig/cor_by_mean_temp_colour_region.pdf")


p1 <- ggplot(out.mods.ma, aes(lag, b.int)) + geom_pointrange(aes(ymin = b.ci.l.int, ymax = b.ci.u.int)) + facet_wrap(~variable) + geom_hline(yintercept = 0, lty = 2)

p2 <- ggplot(out.mods.ma, aes(lag, b.mean.temp)) + geom_pointrange(aes(ymin = b.ci.l.mean.temp, ymax = b.ci.u.mean.temp)) + facet_wrap(~variable) + geom_hline(yintercept = 0, lty = 2) 

p3 <- ggplot(out.mods.ma, aes(lag, b.mean.temp.sq)) + geom_pointrange(aes(ymin = b.ci.l.mean.temp.sq, ymax = b.ci.u.mean.temp.sq)) + facet_wrap(~variable) + geom_hline(yintercept = 0, lty = 2) 

library(gridExtra)
pdf("meta-analytic-mods-temp-temp-squared.pdf", width = 8, height = 8)
grid.arrange(p1, p2, p3, ncol=1)
dev.off()

library(gplots)
cols <- rich.colors(length(unique(out.mods$region)))
names(cols) <- 0:8
#p <- ggplot(curves, aes(temps, ri_pred, colour= as.factor(lag))) + geom_line(aes(group = lag)) + facet_wrap(~variable) + geom_hline(yintercept = 0, lty = 2) + ylab("Correlation") + xlab("Mean temperature") + scale_colour_manual(values = cols)
#ggsave("../fig/curves_predicted_temp_temp_squared_rich.pdf", width = 10, height = 6)

#p <- ggplot(curves, aes(temps, ri_pred, colour=lag)) + geom_line(aes(group = lag)) + facet_wrap(~variable) + geom_hline(yintercept = 0, lty = 2) + ylab("Correlation") + xlab("Mean temperature")
#ggsave("../fig/curves_predicted_temp_temp_squared.pdf", width = 10, height = 6)

if(class(d.rma.mods[[1]]$model) != "rrma") {
  curve_predictions <- ldply(d.rma.mods, function(x){
  avg <- predict(x$model, newmods = cbind(temps, temps^2), transf = transf.ztor)
  data.frame(mean.temp = temps, pred = avg$pred, ci.lb = avg$ci.lb, ci.ub = avg$ci.ub)
})
} else {
  curve_predictions <- ldply(d.rma.mods, function(x){
        avg <- predict(x$model, newdata = data.frame(mean.temp = temps, mean.temp.sq = temps^2), interval = "confidence")
    data.frame(mean.temp = temps, pred = transf.ztor(avg$fit), ci.lb = transf.ztor(avg$lwr), ci.ub = transf.ztor(avg$upr))
})}

x <- subset(curve_predictions, variable == "predator" & lag == 3)
pt.dat <- subset(d.ri.cis, lag == 3 & variable == "predator")

p <- ggplot(x) + geom_ribbon(aes(x = mean.temp, ymax = ci.ub, ymin = ci.lb), fill = "#00000030") + geom_line(aes(x = mean.temp, y = pred)) + geom_hline(yintercept = 0, lty = 2) + theme_bw() + xlab("Mean temperature") + ylab("Correlation") + geom_pointrange(data = pt.dat, aes(x = mean.temp, y = ri, ymin = pi.lb, ymax = pi.ub)) + geom_text(data = pt.dat, aes(label= region, x = mean.temp, y = pi.ub+0.1), cex = 4.0) + ylim(-1, 1)+xlim(3, 17)
ggsave("../fig/prediction_predation_quadratics_raw_ris_lag3.pdf", width = 5.5, height = 4)

# with empirical Bayes comparisons:
pts.eb <- subset(out.mods, lag == 3 & variable == "predator")
ggplot(x) + geom_ribbon(aes(x = mean.temp, ymax = ci.ub, ymin = ci.lb), fill = "#00000030") + geom_line(aes(x = mean.temp, y = pred)) + geom_hline(yintercept = 0, lty = 2) + theme_bw() + xlab("Mean temperature") + ylab("Correlation") + geom_pointrange(data = pt.dat, aes(x = mean.temp, y = ri, ymin = pi.lb, ymax = pi.ub)) + geom_text(data = pt.dat, aes(label= region, x = mean.temp, y = pi.ub+0.1), cex = 4.0) + ylim(-1, 1)+xlim(3, 17)  + geom_point(data = pts.eb, aes(mean.temp, pred), col = "red")

x <- subset(curve_predictions, variable == "predator")
pt.dat <- subset(d.ri.cis, variable == "predator")
pt.dat <- transform(pt.dat, region = reorder(region, -mean.temp))
p <- ggplot(x) + geom_ribbon(aes(x = mean.temp, ymax = ci.ub, ymin = ci.lb), fill = "#00000030") + geom_line(aes(x = mean.temp, y = pred)) + geom_hline(yintercept = 0, lty = 2) + theme_bw() + xlab("Mean temperature") + ylab("Correlation") + geom_pointrange(data = pt.dat, aes(x = mean.temp, y = ri, ymin = pi.lb, ymax = pi.ub), show_guide = FALSE) + geom_text(data = subset(pt.dat, lag == 4), aes(label= region, x = mean.temp, y = pi.ub+0.1), cex = 3.0) + ylim(-1, 1)+xlim(3, 17) + facet_wrap(~lag) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
ggsave("../fig/prediction_predation_quadratics_raw_ris_facet.pdf", width = 10, height = 7)

# non_blups with mean temp colour coding
p <- ggplot(d.ri.cis, aes(lag, ri, pch = variable, colour = variable )) + geom_pointrange(aes(ymin = pi.lb, ymax = pi.ub), position = position_dodge(width = 0.6)) + facet_wrap(~region_temperature_ordered) + geom_hline(yintercept = 0, lty = 1, col = "#00000030") + theme_bw() + xlab("Lag") + ylab("Correlation")
ggsave("../fig/non_BLUPS.pdf", width = 10, height = 6)     

p <- ggplot(out.mods, aes(lag, pred, pch = variable, colour = variable )) + geom_pointrange(aes(ymin = pi.lb, ymax = pi.ub), position = position_dodge(width = 0.5)) + facet_wrap(~region_temperature_ordered) + geom_hline(yintercept = 0, lty = 1, col = "#00000030") + theme_bw() + xlab("Lag") + ylab("Correlation")
ggsave("../fig/BLUPS_mods_temp_temp_sqaured.pdf", width = 10, height = 6)     

library(gplots)
cols <- rich.colors(length(unique(out.mods$region)))
names(cols) <- 0:8
p <- ggplot(out.mods, aes(mean.temp, pred)) + geom_line(aes(group = lag, colour = lag)) + facet_grid(~variable) + geom_hline(yintercept = 0, lty = 2) + geom_text(data = subset(out.mods, lag == 3), aes(label= region, x = mean.temp, y = pred), cex = 3.0) + xlim(3.7, 16) + xlab("Mean temperature") + ylab("Correlation") 
ggsave("../fig/cor_BLUPS_by_mean_temp_colour_lag_lines.pdf", width = 10, height = 6)



##########
x <- subset(curve_predictions, variable %in% c("predator", "nao"))
 pt.dat <- subset(d.ri.cis, variable %in% c("predator", "nao"))
 pt.dat <- transform(pt.dat, region = reorder(region, -mean.temp))
ggplot(x) + geom_ribbon(aes(x = mean.temp, ymax = ci.ub, ymin = ci.lb, group = variable, fill = variable), alpha = 0.3) + 
geom_line(aes(x = mean.temp, y = pred, colour = variable)) + 
geom_hline(yintercept = 0, lty = 2) + theme_bw() + xlab("Mean temperature") + ylab("Correlation") + 
# geom_pointrange(data = pt.dat, aes(x = mean.temp, y = ri, ymin = pi.lb, ymax = pi.ub, colour = variable), show_guide = FALSE) + 
geom_point(data = pt.dat, aes(x = mean.temp, y = ri, colour = variable, size = 0.001*1/abs((pi.ub - pi.lb)))) + 
ylim(-1, 1)+xlim(3, 17) + 
facet_wrap(~lag) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
# geom_text(data = subset(pt.dat, lag == 4), aes(label= region, x = mean.temp, y = pi.ub+0.1), cex = 3.0) + 

