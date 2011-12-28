# ====================================================================
# Last modified: Nov 25, 2011
# Purpose:       run correlation and meta-analyses by lags like the
# snow-crab cod paper
# ====================================================================



#hom.dat.combined <- combined.lob.data(dat = hom.dat, type = "lob.pred.climate", save.file.name = "hom.dat.combined")

out <- ddply(j7, c("region.ordered.by.mean.temp.rev", "variable"), function(x) {
  #m <- lm(prey.value.scaled ~ value.scaled, data = x)
  #ci <- confint(m)
  #l <- ci[2,1]
  #u <- ci[2,2]
  m <- cor.test(x$prey.value.scaled, x$value.scaled)
  r <- m$estimate
  l <- m$conf.int[1]
  u <- m$conf.int[2]
  n <- nrow(x)
  lag <- as.numeric(gsub("[a-z\\.]*", "", unique(x$variable)))
  variable.type <- sub("([a-z]*)\\.[a-z]*[0-9]*", "\\1", unique(x$variable))
  data.frame(variable.type = variable.type, lag = lag, r = r, l = l, u = u, mean.temp = unique(x$mean.temp), n = n)
})

p <- ggplot(out, aes(lag, r, col = mean.temp)) + geom_pointrange(aes(x = lag, y = r, ymin = l, ymax = u)) + facet_grid(region.ordered.by.mean.temp.rev ~ variable.type) + geom_abline(aes(intercept = 0, slope = 0),col = "#00000060")
ggsave("../fig/correlation-lag-ggplot.pdf", plot = p, width = 7, height = 10)

out.meta <- ddply(out, c("lag", "variable.type"), function(x) {
  m <- with(x, metacor(r, n))
  FE <- summary(m)$fixed$TE
  FE.l <- summary(m)$fixed$lower
  FE.u <- summary(m)$fixed$upper

  RE <- summary(m)$random$TE 
  RE.l <- summary(m)$random$lower
  RE.u <- summary(m)$random$upper
  data.frame(FE = FE, FE.l = FE.l, FE.u = FE.u, RE = RE, RE.l = RE.l, RE.u = RE.u)
})

out.meta.FE <- out.meta[,c("lag", "variable.type", "FE", "FE.l", "FE.u")]
out.meta.RE <- out.meta[,c("lag", "variable.type", "RE", "RE.l", "RE.u")]
out.meta.FE$meta.type <- "FE"
out.meta.RE$meta.type <- "RE"
out.meta.FE$lag <- out.meta.FE$lag - 0.2
out.meta.RE$lag <- out.meta.FE$lag + 0.2
names(out.meta.FE) <- c("lag", "variable.type", "TE", "TE.l", "TE.u", "meta.type")
names(out.meta.RE) <- c("lag", "variable.type", "TE", "TE.l", "TE.u", "meta.type")
out.meta <- rbind(out.meta.FE, out.meta.RE)

p <- ggplot(out.meta, aes(lag, TE, col = meta.type)) + geom_pointrange(aes(x = lag, y = TE, ymin = TE.l, ymax = TE.u)) + facet_wrap(~variable.type) + geom_abline(aes(intercept = 0, slope = 0), col= "#00000070")
ggsave("../fig/snow-crab-style-meta-ggplot.pdf", plot = p, width = 7, height = 3)
