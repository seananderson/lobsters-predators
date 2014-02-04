#' Examine effort-lobster lagged correlations

ef <- read.csv("hom.dat.combined.csv")

ef <- subset(ef, type %in% c("prey", "effort"))[,c("year", "combined.value", "type", "region")]
library(reshape)

ef <- cast(ef, region + year ~ type, value = "combined.value")

ef$prey <- log(ef$prey)
ef$effort <- log(ef$effort)

ef <- ddply(ef, "region", transform,
  lagneg3 = c(effort[4:(length(effort))], rep(NA, 3)),
  lagneg2 = c(effort[3:(length(effort))], rep(NA, 2)),
  lagneg1 = c(effort[2:(length(effort))], rep(NA, 1)),
  lag0 = c(rep(NA, 0), effort[1:(length(effort))]),  
  lag1 = c(rep(NA, 1), effort[1:(length(effort)-1)]),
  lag2 = c(rep(NA, 2), effort[1:(length(effort)-2)]),
  lag3 = c(rep(NA, 3), effort[1:(length(effort)-3)])
)

ef <- na.omit(ef)

ef.m <- melt(ef, id.vars = c("region", "year"))
ef.m <- ddply(ef.m, c("region", "variable"), transform, value.scaled = scale.dat(value))

ef.scaled <- cast(ef.m, region + year ~ variable, value = "value.scaled")
ef.scaled$effort <- NULL

ef.lags <- ef.scaled[,-which(names(ef.scaled) %in% "prey")]
ef.prey <- ef.scaled[,c("region", "year", "prey")]

ids <- grep("lag", names(ef.lags))
ef.m <- melt.data.frame(ef.lags, id.vars = c("region", "year"))
ef.m <- plyr::rename(ef.m, replace = c("variable" = "lag", "value" = "effort"))
ef.m$lag <- sub("lag", "", ef.m$lag)
ef.m$lag <- sub("neg", "-", ef.m$lag)
ef.m$lag <- as.numeric(ef.m$lag)

ef.m <- ddply(ef.m, "region", function(x) merge(x, ef.prey))
ef.m <- ef.m[order(ef.m$region, ef.m$lag, ef.m$year), ]

# now get in the same shape as others:
ef.m$variable <- "effort"
ef.m <- plyr::rename(ef.m, replace = c("prey" = "prey_val", "effort" = "com_val"))

write.csv(ef.m, file = "effort-lags.csv", row.names = FALSE)

