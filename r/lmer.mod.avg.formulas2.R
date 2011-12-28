library(lme4)
pred.vars <- paste("predator.lag", c("04","05","06","07","08","09","10"), sep = "")
env.vars <- paste("environment.lag", c("00", "01", "02", "03", "04","05","06","07","08","09","10"), sep = "")


formulas_with_interactions <- as.character(sapply(pred.vars, function(p.v) {
  sapply(env.vars, function(e.v) {
         #browser()
    paste("prey ~ ", p.v, " * ", e.v, " + (", p.v, " + ", e.v, " | region)", sep = "")
  })
}))
#formulas_with_interactions <- as.character(sapply(pred.vars, function(p.v) {
  #sapply(env.vars, function(e.v) {
    #paste("prey ~ ", p.v, " * ", e.v, " + (", p.v, " + ", e.v, " | region)", " + (", p.v, " + ", e.v, " | year)", sep = "")
  #})
#}))
formulas_with_interactions_names <- as.character(sapply(pred.vars, function(p.v) {
  sapply(env.vars, function(e.v) {
    paste(abbreviate(p.v), abbreviate(e.v), "int", sep = ".")
  })
}))

formulas_without_interactions <- as.character(sapply(pred.vars, function(p.v) {
  sapply(env.vars, function(e.v) {
    paste("prey ~ ", p.v, " + ", e.v, " + (", p.v, " + ", e.v, " | region)", sep = "")
  })
}))
formulas_without_interactions_names <- as.character(sapply(pred.vars, function(p.v) {
  sapply(env.vars, function(e.v) {
    paste(abbreviate(p.v), abbreviate(e.v), sep = ".")
  })
}))

formulas_pred_only <- as.character(sapply(pred.vars, function(p.v) {
    paste("prey ~ ", p.v, " + (", p.v, " | region)", sep = "")
}))
formulas_pred_only_names <- as.character(sapply(pred.vars, function(p.v) {
    paste(abbreviate(p.v), sep = ".")
}))

formulas_env_only <- as.character(sapply(env.vars, function(e.v) {
    paste("prey ~ ", e.v, " + (", e.v, " | region)", sep = "")
}))
formulas_env_only_names <- as.character(sapply(env.vars, function(e.v) {
    paste(abbreviate(e.v), sep = ".")
}))

formulas <- c(formulas_with_interactions, formulas_without_interactions, formulas_pred_only, formulas_env_only)
formulas_names <- c(formulas_with_interactions_names, formulas_without_interactions_names, formulas_pred_only_names, formulas_env_only_names)

####
## create an R file that contains the code to be sourced
## can't just use as.formula(formulas[1]) with lmer and modavg()
## because it will interpret the formula as "as.formula(formulas[i])"
## and not know what to exclude
## argh
####
# This is insane, R is scripting itself here:
lmer.formulas <- sapply(1:length(formulas), function(i) paste("cand.set[[", i, "]] <- lmer(", formulas[i], ", data = d.scaled, REML = FALSE)", sep = ""))
write.table(lmer.formulas, file = "lmer.formulas.R", row.names = FALSE, col.names = FALSE, quote = FALSE)
cand.set <- list()
source("lmer.formulas.R")
## was:
#cand.set <- list()
#for(i in 1:length(formulas)) {
  #print(i)
  #cand.set[[i]] <- lmer(as.formula(formulas[i]), data = d.scaled, REML = FALSE)
#}

modnames <- formulas_names

sink("../fig/aic-mavg-lmer-table-2.txt")
print(aictab(cand.set, modnames))
sink()

interactions_to_exclude <- sapply(pred.vars, function(p.v) sapply(env.vars, function(e.v) paste(p.v, ":", e.v, sep = "")))
#interactions_to_exclude <- as.character(interactions_to_exclude)

#######
#######
#######
#### Here to below needs to be fixed!! having problems - Sean
#######
#######
#######
#######

beta.avg <- list()
beta.avg[[1]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag04", exclude = as.list(interactions_to_exclude[,1]))
beta.avg[[2]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag05", exclude = as.list(interactions_to_exclude[,2]))
beta.avg[[3]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag06", exclude = as.list(interactions_to_exclude[,3]))
beta.avg[[4]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag07", exclude = as.list(interactions_to_exclude[,4]))
beta.avg[[5]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag08", exclude = as.list(interactions_to_exclude[,5]))
beta.avg[[6]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag09", exclude = as.list(interactions_to_exclude[,6]))
beta.avg[[7]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "predator.lag10", exclude = as.list(interactions_to_exclude[,7]))

# env
beta.avg[[8]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag00", exclude = as.list(interactions_to_exclude[1,]))
beta.avg[[9]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag01", exclude = as.list(interactions_to_exclude[2,]))
beta.avg[[10]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag02", exclude = as.list(interactions_to_exclude[3,]))
beta.avg[[11]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag03", exclude = as.list(interactions_to_exclude[4,]))
beta.avg[[12]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag04", exclude = as.list(interactions_to_exclude[5,]))
beta.avg[[13]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag05", exclude = as.list(interactions_to_exclude[6,]))
beta.avg[[14]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag06", exclude = as.list(interactions_to_exclude[7,]))
beta.avg[[15]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag07", exclude = as.list(interactions_to_exclude[8,]))
beta.avg[[16]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag08", exclude = as.list(interactions_to_exclude[9,]))
beta.avg[[17]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag09", exclude = as.list(interactions_to_exclude[10,]))
beta.avg[[18]] <- modavg(cand.set = cand.set, modnames = modnames, parm = "environment.lag10", exclude = as.list(interactions_to_exclude[11,]))

beta.avg.df <- ldply(beta.avg, summarize, mod.avg.beta = Mod.avg.beta, u = Upper.CL, l = Lower.CL)
beta.avg.df$parameter <- c(pred.vars, env.vars)



pdf("../fig/averaged-scaled-parameter-estimates-lmer2.pdf", width = 5, height = 6)
pch <- c(rep(19, 7), rep(21, 11), cex = 0.8)
par(mar = c(4, 9, 1, 1))
plot(1, 1, type = "n", xlim = c(-0.5, 0.5), ylim = c(1,nrow(beta.avg.df)), axes = FALSE, xlab = "Averaged and scaled parameter estimate", ylab = "")
with(beta.avg.df, points(mod.avg.beta, 1:nrow(beta.avg.df), pch = pch))
with(beta.avg.df, segments(l, 1:nrow(beta.avg.df), u, 1:nrow(beta.avg.df)))
abline(v = 0, lty = 2)
abline(h = 7.5, lty = 2)   #value for horizontal line y-axis
axis(1)
axis(2, at = 1:nrow(beta.avg.df), labels = beta.avg.df$parameter, las = 1)
dev.off()

