# Check whether there is remaining spatial correlation in residuals

temps <- sort(unique(pt.dat$mean.temp))
pred <- ldply(d.rma.mods, function(x){
  avg <- predict(x$model, newmods = cbind(temps, temps^2), transf = transf.ztor)
  data.frame(mean.temp = temps, pred = avg$pred)
})

pred <- plyr::join(pred, pt.dat[,c("mean.temp", "variable", "lag", "ri")])
pred$residual <- pred$ri - pred$pred

reg.ids <- pt.dat[,c("mean.temp", "region")]
reg.ids <- reg.ids[-which(duplicated(reg.ids)), ]
pred <- plyr::join(pred, reg.ids)

p <- ggplot(subset(pred, variable != "effort"), aes(mean.temp, residual, colour = region)) + geom_point() + facet_grid(lag~variable) + geom_abline(intercept = 0, slope = 0, lty = 2) + theme_bw() + xlab(expression(Mean~region~temperature~(degree*C))) + ylab("Residual") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("../fig/spatial_residual_check.pdf", width = 8, height = 10, plot = p)
