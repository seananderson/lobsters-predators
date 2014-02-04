#' make result tables for supplement

pt.dat <- transform(d.ri.cis, region = reorder(region, -mean.temp))
x1 <- pt.dat[,c("region", "variable", "lag", "ri", "ni", "ni.raw",
  "pi.lb", "pi.ub")]
x1$ri <- sprintf("%.2f", round(x1$ri, 2))
x1$pi.lb <- sprintf("%.2f", round(x1$pi.lb, 2))
x1$pi.ub <- sprintf("%.2f", round(x1$pi.ub, 2))
x1$ni <- sprintf("%.2f", round(x1$ni, 1))
x1$variable <- as.character(x1$variable)
x1$variable[x1$variable == "nao"] <- "NAOI"
x1$variable[x1$variable == "predator"] <- "Predators"
x1$variable[x1$variable == "temperature"] <- "Temperature"
x1$variable[x1$variable == "effort"] <- "Effort"
names(x1) <- c("Region", "Variable", "Lag", "r", "n*", "n", "2.5%
  CI*", "97.5% CI*")
write.csv(x1, row.names = FALSE, file = "../fig/correlations-table.csv")

###############################################################
x.pval <- ldply(d.rma.mods, function(x) {
  pval <- x$model$pval[3]
  data.frame(pval.b.sq = pval)
})

x2 <- ldply(d.rma.mods, function(x) {
  b <- x$model$b
  se <- x$model$se
  data.frame(Intercept = paste0(sprintf("%.2f", round(b[1], 2)), " (", sprintf("%.2f",round(se[1], 2)), ")"),
    B.temperature = paste0(sprintf("%.2f", round(b[2], 2)), " (", sprintf("%.2f", round(se[2], 2)), ")"),
    B.temperature.squared = paste0(sprintf("%.3f", round(b[3], 3)), " (", sprintf("%.3f", round(se[3], 3)), ")"), quad_pval = x$model$pval[3])
})
x2$variable <- as.character(x2$variable)
x2$variable[x2$variable == "nao"] <- "NAOI"
x2$variable[x2$variable == "predator"] <- "Predators"
x2$variable[x2$variable == "temperature"] <- "Temperature"
x2$variable[x2$variable == "effort"] <- "Effort"
names(x2) <- c("Variable", "Lag", "Intercept", "B_temperature",
  "B_temperature-squared")

write.csv(x2, row.names = FALSE, file = "../fig/quadratic-meta-regressions.csv")

###############################################################

temps.table <- c(4, 10, 16)
pr_table <- ldply(d.rma.mods, function(x){
  avg <- predict(x$model, newmods = cbind(temps.table, temps.table^2),
    transf = transf.ztor)
  data.frame(mean.temp = temps.table, pred = avg$pred, ci.lb =
    avg$ci.lb, ci.ub = avg$ci.ub)
})
pr_table$pred <- sprintf("%.2f", round(pr_table$pred, 2))
pr_table$ci.lb <- sprintf("%.2f", round(pr_table$ci.lb, 2))
pr_table$ci.ub <- sprintf("%.2f", round(pr_table$ci.ub, 2))
pr_table$pr <- with(pr_table, paste0(pred, " (", ci.lb, ", ", ci.ub, ")"))
pr_table <- pr_table[, c("variable", "lag", "mean.temp", "pr")]
x3 <- cast(pr_table, variable + lag ~ mean.temp, value = "pr")
x3$variable <- as.character(x3$variable)
x3$variable[x3$variable == "nao"] <- "NAOI"
x3$variable[x3$variable == "predator"] <- "Predators"
x3$variable[x3$variable == "temperature"] <- "Temperature"
x3$variable[x3$variable == "effort"] <- "Effort"
names(x3) <- c("Variable", "Lag", "4 degrees C", "10 degrees C", "16 degrees C")
#write.csv(x3, row.names = FALSE, file = "../fig/warm-mid-cool-predictions.csv")

###############################################################

x4 <- d.ri[,c("region", "mean.temp", "sd.temp", "median.temp", "q0.1.temp", "q0.9.temp")]
x4 <- x4[!duplicated(x4), ]
x4 <- x4[order(x4$mean.temp), ]
x4$mean.temp <- sprintf("%.2f", round(x4$mean.temp, 2))
x4$sd.temp <- sprintf("%.2f", round(x4$sd.temp, 2))
x4$median.temp <- sprintf("%.2f", round(x4$median.temp, 2))
x4$q0.1.temp <- sprintf("%.2f", round(x4$q0.1.temp, 2))
x4$q0.9.temp <- sprintf("%.2f", round(x4$q0.9.temp, 2))
names(x4) <- c("Region", "Mean", "SD", "Median", "10% quantile", "90% quantile")
#write.csv(x4, row.names = FALSE, file = "../fig/region-temperatures.csv")
