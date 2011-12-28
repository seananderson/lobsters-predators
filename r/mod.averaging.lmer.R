# ====================================================================
# Last modified: Nov 29, 2011
# Purpose:       derive AICc multimodel averaged coefficient estimates
# ====================================================================

#http://kcl.academia.edu/JamesMillington/Papers/739046/Multi-Model_Inference_in_Biogeography

#hom.dat.combined <- combined.lob.data(dat = hom.dat, type = "lob.pred.climate", save.file.name = "hom.dat.combined")
#d <- cast(hom.dat.combined, region + year ~ type, value = "combined.value")

#junk <- ddply(d, "region", transform, 
  #environment.lag06 = c(rep(NA, 6), environment[1:(length(environment)-6)]), predator.lag06 = c(rep(NA, 6), predator[1:(length(predator) - 6)]), 
  #environment.lag07 = c(rep(NA, 7), environment[1:(length(environment)-7)]), predator.lag07 = c(rep(NA, 7), predator[1:(length(predator) - 7)]),
  #environment.lag08 = c(rep(NA, 8), environment[1:(length(environment)-8)]), predator.lag08 = c(rep(NA, 8), predator[1:(length(predator) - 8)]),
  #environment.lag09 = c(rep(NA, 9), environment[1:(length(environment)-9)]), predator.lag09 = c(rep(NA, 9), predator[1:(length(predator) - 9)]),
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
#(pred +temp|region) or (pred*temp|region) if we want the interaction to vary as well.

cand.set <- list()

cand.set[[1]] <- lmer(prey ~ predator.lag00 * environment.lag00 + (predator.lag00 | region) + (0 + environment.lag00 | region), data = d.scaled, REML = FALSE)

cand.set[[2]] <- lmer(prey ~ predator.lag04 * environment.lag01 + (predator.lag01 | region) + (0 + environment.lag01 | region), data = d.scaled, REML = FALSE)

cand.set[[3]] <- lmer(prey ~ predator.lag02 * environment.lag02 + (predator.lag02 | region) + (0 + environment.lag02 | region), data = d.scaled, REML = FALSE)

cand.set[[4]] <- lmer(prey ~ predator.lag03 * environment.lag03 + (predator.lag03 | region) + (0 + environment.lag03 | region), data = d.scaled, REML = FALSE)

cand.set[[5]] <- lmer(prey ~ predator.lag04 * environment.lag04 + (predator.lag04 | region) + (0 + environment.lag04 | region), data = d.scaled, REML = FALSE)

cand.set[[6]] <- lmer(prey ~ predator.lag05 * environment.lag05 + (predator.lag05 | region) + (0 + environment.lag05 | region), data = d.scaled, REML = FALSE)

cand.set[[7]] <- lmer(prey ~ predator.lag06 * environment.lag06 + (predator.lag06 | region) + (0 + environment.lag06 | region), data = d.scaled, REML = FALSE) # 0=take the fitted values obtained by the population model, allow slopes to vary by region. brackets denotes random

cand.set[[8]] <- lmer(prey ~ predator.lag07 * environment.lag07 + (predator.lag07 | region) + (0 + environment.lag07 | region), data = d.scaled, REML = FALSE)

cand.set[[9]] <- lmer(prey ~ predator.lag08 * environment.lag08 + (predator.lag08 | region) + (0 + environment.lag08 | region), data = d.scaled, REML = FALSE)

cand.set[[10]] <- lmer(prey ~ predator.lag09 * environment.lag09 + (predator.lag09 | region) + (0 + environment.lag09 | region), data = d.scaled, REML = FALSE)

cand.set[[11]] <- lmer(prey ~ predator.lag10 * environment.lag10 + (predator.lag10 | region) + (0 + environment.lag10 | region), data = d.scaled, REML = FALSE)


cand.set[[12]] <- lmer(prey ~ predator.lag00 + environment.lag00 + (predator.lag00 | region) + (0 + environment.lag00 | region), data = d.scaled, REML = FALSE)

cand.set[[13]] <- lmer(prey ~ predator.lag01 + environment.lag01 + (predator.lag01 | region) + (0 + environment.lag01 | region), data = d.scaled, REML = FALSE)

cand.set[[14]] <- lmer(prey ~ predator.lag02 + environment.lag02 + (predator.lag02 | region) + (0 + environment.lag02 | region), data = d.scaled, REML = FALSE)

cand.set[[15]] <- lmer(prey ~ predator.lag03 + environment.lag03 + (predator.lag03 | region) + (0 + environment.lag03 | region), data = d.scaled, REML = FALSE)

cand.set[[16]] <- lmer(prey ~ predator.lag04 + environment.lag04 + (predator.lag04 | region) + (0 + environment.lag04 | region), data = d.scaled, REML = FALSE)

cand.set[[17]] <- lmer(prey ~ predator.lag05 + environment.lag05 + (predator.lag05 | region) + (0 + environment.lag05 | region), data = d.scaled, REML = FALSE)

cand.set[[18]] <- lmer(prey ~ predator.lag06 + environment.lag06 + (predator.lag06 | region) + (0 + environment.lag06 | region), data = d.scaled, REML = FALSE)

cand.set[[19]] <- lmer(prey ~ predator.lag07 + environment.lag07 + (predator.lag07 | region) + (0 + environment.lag07 | region), data = d.scaled, REML = FALSE)

cand.set[[20]] <- lmer(prey ~ predator.lag08 + environment.lag08 + (predator.lag08 | region) + (0 + environment.lag08 | region), data = d.scaled, REML = FALSE)

cand.set[[21]] <- lmer(prey ~ predator.lag09 + environment.lag09 + (predator.lag09 | region) + (0 + environment.lag09 | region), data = d.scaled, REML = FALSE)

cand.set[[22]] <- lmer(prey ~ predator.lag10 + environment.lag10 + (predator.lag10 | region) + (0 + environment.lag10 | region), data = d.scaled, REML = FALSE)


cand.set[[23]] <- lmer(prey ~ predator.lag00  + (predator.lag00 | region) , data = d.scaled, REML = FALSE)

cand.set[[24]] <- lmer(prey ~ predator.lag01  + (predator.lag01 | region) , data = d.scaled, REML = FALSE)

cand.set[[25]] <- lmer(prey ~ predator.lag02  + (predator.lag02 | region) , data = d.scaled, REML = FALSE)

cand.set[[26]] <- lmer(prey ~ predator.lag03  + (predator.lag03 | region) , data = d.scaled, REML = FALSE)

cand.set[[27]] <- lmer(prey ~ predator.lag04  + (predator.lag04 | region) , data = d.scaled, REML = FALSE)

cand.set[[28]] <- lmer(prey ~ predator.lag05  + (predator.lag05 | region) , data = d.scaled, REML = FALSE)

cand.set[[29]] <- lmer(prey ~ predator.lag06  + (predator.lag06 | region) , data = d.scaled, REML = FALSE)

cand.set[[30]] <- lmer(prey ~ predator.lag07  + (predator.lag07 | region) , data = d.scaled, REML = FALSE)

cand.set[[31]] <- lmer(prey ~ predator.lag08  + (predator.lag08 | region) , data = d.scaled, REML = FALSE)

cand.set[[32]] <- lmer(prey ~ predator.lag09  + (predator.lag09 | region) , data = d.scaled, REML = FALSE)

cand.set[[33]] <- lmer(prey ~ predator.lag10  + (predator.lag10 | region) , data = d.scaled, REML = FALSE)


cand.set[[34]] <- lmer(prey ~ environment.lag00  + (environment.lag00 | region) , data = d.scaled, REML = FALSE)

cand.set[[35]] <- lmer(prey ~ environment.lag01  + (environment.lag01 | region) , data = d.scaled, REML = FALSE)

cand.set[[36]] <- lmer(prey ~ environment.lag02  + (environment.lag02 | region) , data = d.scaled, REML = FALSE)

cand.set[[37]] <- lmer(prey ~ environment.lag03  + (environment.lag03 | region) , data = d.scaled, REML = FALSE)

cand.set[[38]] <- lmer(prey ~ environment.lag04  + (environment.lag04 | region) , data = d.scaled, REML = FALSE)

cand.set[[39]] <- lmer(prey ~ environment.lag05  + (environment.lag05 | region) , data = d.scaled, REML = FALSE)

cand.set[[40]] <- lmer(prey ~ environment.lag06  + (environment.lag06 | region) , data = d.scaled, REML = FALSE)

cand.set[[41]] <- lmer(prey ~ environment.lag07  + (environment.lag07 | region) , data = d.scaled, REML = FALSE)

cand.set[[42]] <- lmer(prey ~ environment.lag08  + (environment.lag08 | region) , data = d.scaled, REML = FALSE)

cand.set[[43]] <- lmer(prey ~ environment.lag09  + (environment.lag09 | region) , data = d.scaled, REML = FALSE)

cand.set[[44]] <- lmer(prey ~ environment.lag10  + (environment.lag10 | region) , data = d.scaled, REML = FALSE)


modnames <- c("all.0","all.1","all.2","all.3","all.4","all.5","all.6", "all.7", "all.8", "all.9", "all.10",
              "no.inter.0","no.inter.1","no.inter.2","no.inter.3","no.inter.4","no.inter.5","no.inter.6", 
              "no.inter.7", "no.inter.8", "no.inter.9", "no.inter.10", "pred.0","pred.1","pred.2","pred.3",
              "pred.4","pred.5","pred.6", "pred.7", "pred.8", "pred.9", "pred.10", "env.0","env.1","env.2",
              "env.3","env.4","env.5", "env.6", "env.7", "env.8", "env.9", "env.10"
              )

sink("../fig/aic-mavg-lmer-table.txt")
print(aictab(cand.set, modnames))
sink()


beta.avg <- list()
beta.avg[[1]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag00", exclude = list("environment.lag00:predator.lag00"))
beta.avg[[2]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag01", exclude = list("environment.lag01:predator.lag01"))
beta.avg[[3]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag02", exclude = list("environment.lag02:predator.lag02"))
beta.avg[[4]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag03", exclude = list("environment.lag03:predator.lag03"))
beta.avg[[5]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag04", exclude = list("environment.lag04:predator.lag04"))
beta.avg[[6]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag05", exclude = list("environment.lag05:predator.lag05"))
beta.avg[[7]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag06", exclude = list("environment.lag06:predator.lag06"))
beta.avg[[8]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag07", exclude = list("environment.lag07:predator.lag07"))
beta.avg[[9]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag08", exclude = list("environment.lag08:predator.lag08"))
beta.avg[[10]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag09", exclude = list("environment.lag09:predator.lag09"))
beta.avg[[11]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag10", exclude = list("environment.lag10:predator.lag10"))

beta.avg[[12]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag00", exclude = list("environment.lag00:predator.lag00"))
beta.avg[[13]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag01", exclude = list("environment.lag01:predator.lag01"))
beta.avg[[14]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag02", exclude = list("environment.lag02:predator.lag02"))
beta.avg[[15]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag03", exclude = list("environment.lag03:predator.lag03"))
beta.avg[[16]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag04", exclude = list("environment.lag04:predator.lag04"))
beta.avg[[17]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag05", exclude = list("environment.lag05:predator.lag05"))
beta.avg[[18]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag06", exclude = list("environment.lag06:predator.lag06"))
beta.avg[[19]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag07", exclude = list("environment.lag07:predator.lag07"))
beta.avg[[20]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag08", exclude = list("environment.lag08:predator.lag08"))
beta.avg[[21]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag09", exclude = list("environment.lag09:predator.lag09"))
beta.avg[[22]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag10", exclude = list("environment.lag10:predator.lag10"))

beta.avg.df <- ldply(beta.avg, summarize, mod.avg.beta = Mod.avg.beta, u = Upper.CL, l = Lower.CL)
beta.avg.df$parameter <- c("predator.lag00","predator.lag01","predator.lag02","predator.lag03","predator.lag04","predator.lag05","predator.lag06", 
        "predator.lag07", "predator.lag08","predator.lag09", "predator.lag10", "environment.lag00","environment.lag01","environment.lag02",
        "environment.lag03","environment.lag04","environment.lag05","environment.lag06","environment.lag07", "environment.lag08", "environment.lag09",
        "environment.lag10")


pdf("../fig/averaged-scaled-parameter-estimates-lmer.pdf", width = 5, height = 6)
pch <- c(rep(19, 11), rep(21, 11), cex = 0.8)
par(mar = c(4, 9, 1, 1))
plot(1, 1, type = "n", xlim = c(-0.5, 0.5), ylim = c(1,22), axes = FALSE, xlab = "Averaged and scaled parameter estimate", ylab = "")
with(beta.avg.df, points(mod.avg.beta, 1:22, pch = pch))
with(beta.avg.df, segments(l, 1:22, u, 1:22))
abline(v = 0, lty = 2)
abline(h = 11.5, lty = 2)   #value for horizontal line y-axis
axis(1)
axis(2, at = 1:22, labels = beta.avg.df$parameter, las = 1)
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


