# ====================================================================
# Last modified: Nov 25, 2011
# Purpose:       derive AICc multimodel averaged coefficient estimates
# ====================================================================

#http://kcl.academia.edu/JamesMillington/Papers/739046/Multi-Model_Inference_in_Biogeography

#hom.dat.combined <- combined.lob.data(dat = hom.dat, type = "lob.pred.climate", save.file.name = "hom.dat.combined")
#d <- cast(hom.dat.combined, region + year ~ type, value = "combined.value")

#junk <- ddply(d, "region", transform, 
  #environment.lag6 = c(rep(NA, 6), environment[1:(length(environment)-6)]), predator.lag6 = c(rep(NA, 6), predator[1:(length(predator) - 6)]), 
  #environment.lag7 = c(rep(NA, 7), environment[1:(length(environment)-7)]), predator.lag7 = c(rep(NA, 7), predator[1:(length(predator) - 7)]),
  #environment.lag8 = c(rep(NA, 8), environment[1:(length(environment)-8)]), predator.lag8 = c(rep(NA, 8), predator[1:(length(predator) - 8)]),
  #environment.lag9 = c(rep(NA, 9), environment[1:(length(environment)-9)]), predator.lag9 = c(rep(NA, 9), predator[1:(length(predator) - 9)]),
  #environment.lag10 = c(rep(NA, 10), environment[1:(length(environment)-10)]), predator.lag10 = c(rep(NA, 10), predator[1:(length(predator) - 10)]) 
#)



#junk$Climate <-NULL
#junk$Exploitation <-NULL
#junk$environment <-NULL
#junk$predator <-NULL

#junk <- na.omit(junk)


#junk.melt <- melt(junk, id.vars = c("region", "year"))
#junk.scaled <- ddply(junk.melt, c("region", "variable"), transform, value.scaled = scale.dat(value))
#junk.scaled$value <- NULL

#d.scaled <- cast(junk.scaled, region + year ~ variable, value = "value.scaled")

#d.scaled <- d.scaled[-which(d.scaled$region == "Connecticut"), ] # only three non-NA years after lagging the data

cand.set <- list()

cand.set[[1]] <- lmer(prey ~ predator.lag6 * environment.lag6 + (predator.lag6 | region) + (0 + environment.lag6 | region), data = d.scaled, REML = FALSE) # 0=take the fitted values obtained by the population model, allow slopes to vary by region. brackets denotes random

cand.set[[2]] <- lmer(prey ~ predator.lag7 * environment.lag7 + (predator.lag7 | region) + (0 + environment.lag7 | region), data = d.scaled, REML = FALSE)

cand.set[[3]] <- lmer(prey ~ predator.lag8 * environment.lag8 + (predator.lag8 | region) + (0 + environment.lag8 | region), data = d.scaled, REML = FALSE)

cand.set[[4]] <- lmer(prey ~ predator.lag9 * environment.lag9 + (predator.lag9 | region) + (0 + environment.lag9 | region), data = d.scaled, REML = FALSE)

cand.set[[5]] <- lmer(prey ~ predator.lag10 * environment.lag10 + (predator.lag10 | region) + (0 + environment.lag10 | region), data = d.scaled, REML = FALSE)


cand.set[[6]] <- lmer(prey ~ predator.lag6 + environment.lag6 + (predator.lag6 | region) + (0 + environment.lag6 | region), data = d.scaled, REML = FALSE)

cand.set[[7]] <- lmer(prey ~ predator.lag7 + environment.lag7 + (predator.lag7 | region) + (0 + environment.lag7 | region), data = d.scaled, REML = FALSE)

cand.set[[8]] <- lmer(prey ~ predator.lag8 + environment.lag8 + (predator.lag8 | region) + (0 + environment.lag8 | region), data = d.scaled, REML = FALSE)

cand.set[[9]] <- lmer(prey ~ predator.lag9 + environment.lag9 + (predator.lag9 | region) + (0 + environment.lag9 | region), data = d.scaled, REML = FALSE)

cand.set[[10]] <- lmer(prey ~ predator.lag10 + environment.lag10 + (predator.lag10 | region) + (0 + environment.lag10 | region), data = d.scaled, REML = FALSE)


cand.set[[11]] <- lmer(prey ~ predator.lag6  + (predator.lag6 | region) , data = d.scaled, REML = FALSE)

cand.set[[12]] <- lmer(prey ~ predator.lag7  + (predator.lag7 | region) , data = d.scaled, REML = FALSE)

cand.set[[13]] <- lmer(prey ~ predator.lag8  + (predator.lag8 | region) , data = d.scaled, REML = FALSE)

cand.set[[14]] <- lmer(prey ~ predator.lag9  + (predator.lag9 | region) , data = d.scaled, REML = FALSE)

cand.set[[15]] <- lmer(prey ~ predator.lag10  + (predator.lag10 | region) , data = d.scaled, REML = FALSE)


cand.set[[16]] <- lmer(prey ~ environment.lag6  + (environment.lag6 | region) , data = d.scaled, REML = FALSE)

cand.set[[17]] <- lmer(prey ~ environment.lag7  + (environment.lag7 | region) , data = d.scaled, REML = FALSE)

cand.set[[18]] <- lmer(prey ~ environment.lag8  + (environment.lag8 | region) , data = d.scaled, REML = FALSE)

cand.set[[19]] <- lmer(prey ~ environment.lag9  + (environment.lag9 | region) , data = d.scaled, REML = FALSE)

cand.set[[20]] <- lmer(prey ~ environment.lag10  + (environment.lag10 | region) , data = d.scaled, REML = FALSE)


modnames <- c("all.6", "all.7", "all.8", "all.9", "all.10",
              "no.inter.6", "no.inter.7", "no.inter.8", "no.inter.9", "no.inter.10",
              "pred.6", "pred.7", "pred.8", "pred.9", "pred.10",
              "env.6", "env.7", "env.8", "env.9", "env.10"
              )

sink("../fig/aic-mavg-lmer-table.txt")
print(aictab(cand.set, modnames))
sink()


beta.avg <- list()
beta.avg[[1]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag6", exclude = list("environment.lag6:predator.lag6"))
beta.avg[[2]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag7", exclude = list("environment.lag7:predator.lag7"))
beta.avg[[3]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag8", exclude = list("environment.lag8:predator.lag8"))
beta.avg[[4]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag9", exclude = list("environment.lag9:predator.lag9"))
beta.avg[[5]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag10", exclude = list("environment.lag10:predator.lag10"))

beta.avg[[6]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag6", exclude = list("environment.lag6:predator.lag6"))
beta.avg[[7]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag7", exclude = list("environment.lag7:predator.lag7"))
beta.avg[[8]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag8", exclude = list("environment.lag8:predator.lag8"))
beta.avg[[9]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag9", exclude = list("environment.lag9:predator.lag9"))
beta.avg[[10]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag10", exclude = list("environment.lag10:predator.lag10"))

beta.avg.df <- ldply(beta.avg, summarize, mod.avg.beta = Mod.avg.beta, u = Upper.CL, l = Lower.CL)
beta.avg.df$parameter <- c("predator.lag6", "predator.lag7", "predator.lag8","predator.lag9", "predator.lag10", "environment.lag6","environment.lag7", "environment.lag8", "environment.lag9","environment.lag10")


pdf("../fig/averaged-scaled-parameter-estimates-lmer.pdf", width = 5, height = 6)
pch <- c(rep(19, 5), rep(21, 5), cex = 0.8)
par(mar = c(4, 9, 1, 1))
plot(1, 1, type = "n", xlim = c(-0.5, 0.5), ylim = c(1,10), axes = FALSE, xlab = "Averaged and scaled parameter estimate", ylab = "")
with(beta.avg.df, points(mod.avg.beta, 1:10, pch = pch))
with(beta.avg.df, segments(l, 1:10, u, 1:10))
abline(v = 0, lty = 2)
abline(h = 5.5, lty = 2)
axis(1)
axis(2, at = 1:10, labels = beta.avg.df$parameter, las = 1)
dev.off()

### ignoring this for now because I'm not sure I'm predicting
### correctly
#pred.dat <- modavgpred(cand.set, modnames, newdata = d.scaled) # this will take a looong time
#cor.test(pred.dat$mod.avg.pred, d.scaled$prey)
#m.check.dat <- data.frame(mod.avg.pred = pred.dat$mod.avg.pred, region = d.scaled$region, prey = d.scaled$prey)

#library(ggplot2)
#pdf("../fig/actual-vs-predicted-lmer-mavg.pdf", width = 6, height = 5)
#ggplot(m.check.dat, aes(prey, mod.avg.pred, colour = region)) + geom_point() + geom_abline(intercept = 0, slope = 1) + scale_x_continuous(limits = c(-1.5, 1.5)) + scale_y_continuous(limits = c(-1.5, 1.5))
#dev.off()


