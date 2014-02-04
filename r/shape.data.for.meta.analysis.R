# Created by:    Sean C. Anderson
# Created:       Feb 20, 2013
# Last modified: Jul 21, 2013
# Purpose:       take d.scaled and d.temp and reshape as necessary for
# use in the meta-analysis

d.scaled.temp <- read.csv("d.scaled.TEMP.csv", stringsAsFactors = FALSE)
d.scaled.nao <- read.csv("d.scaled.NAO.csv", stringsAsFactors = FALSE)
names(d.scaled.temp) <- sub("environment", "temperature", names(d.scaled.temp))
names(d.scaled.nao) <- sub("environment", "nao", names(d.scaled.nao))
d.scaled.nao$prey <- NULL
d.scaled.nao <- d.scaled.nao[, -grep("predator", names(d.scaled.nao))]
d.scaled <- join(d.scaled.temp, d.scaled.nao)

d.melt.temp <- read.csv("d.melt.TEMP.csv", stringsAsFactors = FALSE)
d.melt.nao <- read.csv("d.melt.NAO.csv", stringsAsFactors = FALSE)

region.temp <- ddply(subset(d.melt.temp, variable !="prey" & variable == "environment.lag00"), c("region"), summarize, mean.temp = mean(value), sd.temp = sd(value), median.temp = median(value), q0.1.temp = quantile(value, 0.1), q0.9.temp = quantile(value, 0.9))

# d.melt$value <- log(d.melt$value + 0.5)
d.melt <- melt(d.scaled, id.vars = c("region", "year"))

d.melt.prey <- subset(d.melt, variable == "prey")
d.melt.prey$variable <- NULL
d.melt.prey <- rename(d.melt.prey, c("value" = "prey_val"))
d.melt.notprey <- subset(d.melt, variable != "prey")
d.melt.notprey <- rename(d.melt.notprey, c("variable" = "comp_var", "value" = "com_val"))


d <- merge(d.melt.prey, d.melt.notprey)
d <- transform(d, variable = gsub("([a-z]+)(\\.)[a-z]+[0-9][0-9]", "\\1", comp_var), lag = as.numeric(gsub("([a-z]+)(\\.)[a-z]+([0-9][0-9])", "\\3", comp_var)))
d$com_var <- NULL
d$comp_var <- NULL
ef <- read.csv("effort-lags.csv")

d <- rbind(d, ef)
d <- d[order(d$region, d$lag, d$variable, d$year), ]

d.pred <- subset(d, variable == "predator" & lag == 0)
d.pred <- rename(d.pred, c("com_val" = "pred_val"))
d.pred$comp_var <- NULL
d.pred$variable <- NULL
d.env <- subset(d, variable == "temperature" & lag == 0)
d.env <-  rename(d.env, c("com_val" = "env_val")) 
d.env$comp_var <- NULL
d.env$variable <- NULL
d.nao <- subset(d, variable == "nao" & lag == 0)
d.nao <-  rename(d.nao, c("com_val" = "nao_val")) 
d.nao$comp_var <- NULL
d.nao$variable <- NULL
d.env.pred <- merge(d.env, d.pred, all = TRUE)
d.env.pred <- merge(d.env.pred, d.nao[,c("region", "year", "lag", "nao_val")], all = TRUE)
# d.env.pred <-na.omit(d.env.pred)

