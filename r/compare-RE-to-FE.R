# compare RE and FE estimates for bias

rma.type <- "FE"
source("meta.analytic.models.R")
source("exploratory.plots.R")
fe_curve_predictions <- curve_predictions
rma.type <- "RE"
source("meta.analytic.models.R")
source("exploratory.plots.R")
re_curve_predictions <- curve_predictions

names(fe_curve_predictions) <- paste0("fe_", names(fe_curve_predictions))
names(re_curve_predictions) <- paste0("re_", names(re_curve_predictions))
names(re_curve_predictions)[1] <- "variable"
cp <- data.frame(re_curve_predictions, fe_curve_predictions[,-1])
library(ggplot2)
p <- ggplot(subset(cp, variable != "effort"), aes(fe_pred, re_pred)) + geom_point() + facet_grid(fe_lag~variable) + geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) + coord_fixed()
ggsave("fe-vs-re-bias-check.pdf", width = 6, height = 10)
