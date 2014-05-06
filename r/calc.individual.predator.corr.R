# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Month 00, 2011
# Purpose:       check the correlations with individual predator
# species
# ====================================================================
load("hom.dat.Rdata")
max.lag <- 8
# min.lag <- 0

## average some of the data:
dat.to.average <- subset(hom.dat, average.var.name != "")
dat.no.average <- subset(hom.dat, average.var.name == "")

dat.averaged <- ddply(dat.to.average, c("region", "type", "year", "average.var.name"), function(x) data.frame(value = mean(x$value), category = unique(x$category)[1], data.type = unique(x$data.type)[1], units = unique(x$units)[1] ,variable = unique(x$average.var.name)[1]))
hom.dat <- merge(dat.no.average, dat.averaged, all = TRUE)
# dat$average.var.name <- NULL
# hom.dat2 <- dat
# load("hom.dat2.rda")
# hom.dat <- hom.dat2
hom.dat.pred <- subset(hom.dat, type == "predator")[,c("variable", "year", "value", "region")]
names(hom.dat.pred)[c(1, 3)] <- c("predator.variable", "predator.value")

hom.dat.lob <- subset(hom.dat, type == "prey")[,c("year", "value", "region")]
names(hom.dat.lob)[c(2)] <- c("prey.value")
#hom.dat.lob$year <- hom.dat.lob$year - 8 # I hope this is the correct direction!!
hom.dat.lob <- ddply(hom.dat.lob, c("year", "region"), summarize, prey.value = mean(prey.value)) # for those cases where they need to be averaged

hom.dat.env <- subset(hom.dat, type == "environment")[,c("year", "value", "region")]
names(hom.dat.env)[c(2)] <- c("env.value")

hom.dat.ind.spp <- merge(hom.dat.pred, hom.dat.lob)
hom.dat.ind.spp <- merge(hom.dat.ind.spp, hom.dat.env)

hom.dat.ind.spp <- transform(hom.dat.ind.spp, predator.value = log(predator.value+0.5), prey.value = log(prey.value+0.5), env.value = scale(env.value))

#pdf("../fig/individual-predators-vs-lobster.pdf", width = 10, height = 10)
#for(region.i in unique(hom.dat.ind.spp$region)) {
#p <- ggplot(subset(hom.dat.ind.spp, region ==region.i), aes(predator.value, prey.value)) + geom_point() + facet_wrap(~predator.variable, scales = "free") + opts(title = region.i)
#print(p)
#}
#dev.off()

source("Autocorr.Pyper.r")
#individual predators with lobster, ccf
out <- ddply(hom.dat.ind.spp, c("region", "predator.variable"), function(x) {
  y<-ccf(x$predator.value, x$prey.value, plot = FALSE, lag.max = 8)
  clim <- qnorm((1 + .95)/2)/sqrt(y$n.used)
  # ni = Autocorr.Pyper(nrow(x), x$prey_val, x$com_val)
  # browser()
  data.frame(lag = y$lag, r = as.numeric(y$acf), clim.u = clim, clim.l = -clim)
})

corr_pvalue <- function(r, N) {
  df <- N - 2
  t.val = sqrt(df) * r / sqrt(1 - r^2)
  p <- pt(t.val, df)
  return(2 * min(p, 1 - p))
}

## with Pyper method for DF:
out <- ddply(hom.dat.ind.spp, c("region", "predator.variable"), function(x) {
  rs <- rep(NA, 9)
  pvalues <- rep(NA, 9)

  x$prey.value[1:max.lag] <- NA
  # browser()
  N <- nrow(x)
   for(LAG in 0:max.lag) {
    pred.val <- x$predator.value[1:(N - LAG)]
    prey.val <- x$prey.value[(1 + LAG):N]
    temp.dat <- data.frame(pred.val, prey.val)
    temp.dat <- na.omit(temp.dat)
    corr <- with(temp.dat, cor(pred.val, prey.val))
    n.star <- Autocorr.Pyper(N = nrow(temp.dat), tsx = temp.dat$prey.val, tsy = temp.dat$pred.val)
    p.value <- corr_pvalue(corr, n.star)
    rs[LAG+1] <- corr
    pvalues[LAG+1] <- p.value
   }
  data.frame(corr = rs, pvalue = pvalues, lag = 0:max.lag)
})

# as in cor.test in R:
# https://svn.r-project.org/R/trunk/src/library/stats/R/cor.test.R


# only take the lags we are interested in:
# out <- subset(out, lag <= 2)
# and flip the lags to be positive:
# out <- transform(out, lag = lag * -1)
# out <- subset(out, lag >= 0)


# pdf("../fig/individual-predators-vs-lobster-ccf2.pdf", width = 9, height = 9)
# for(region.i in unique(out$region)) {
#   print(region.i)
# print(ggplot(subset(out, region == region.i), aes(lag, r)) + facet_wrap(~predator.variable) + geom_linerange(aes(lag, ymin = 0, ymax = r)) + opts(title = region.i) + geom_hline(aes(yintercept = clim.u), lty = 2) + geom_hline(aes(yintercept = clim.l), lty = 2))
# }
# dev.off()

library(RColorBrewer)
cols <- rev(brewer.pal(11, "RdBu"))
cuts <- seq(-1, 1, length.out = 12)

# x.mids <- seq(0, 1-1/9, length.out = 9) - (1/9)/2 + 1/9
# y.mids <- seq(0, 1-1/9, length.out = 9) - (1/9)/2 + 1/9
# j <- cast(out, region + lag ~ predator.variable, value = "r")
# j <- j[order(j$region, j$lag), ]
# pdf("allspp_correlations.pdf")
# par(mar = c(8, 4, 1, 1))
# image(as.matrix(j[,-c(1:2)]), breaks = cuts, col = cols, xlab = "", ylab = "Species", useRaster = FALSE, axes = FALSE, tck = FALSE)
# abline(v = x.mids, lwd = 1, col = "grey40")
# abline(v = seq(0, 1-(1/9), length.out = 9), lwd = 1, col = "grey40")
# axis(1, at = x.mids, labels = unique(j$region), las = 2, tck = FALSE)
# box(col = "grey50")
# dev.off()


# out <- transform(out, col = cols[findInterval(r, cuts)])
# x <- subset(out, region == "Connecticut" & predator.variable == "longhorn.abund")
# plot(1, 1, xlim = c(0, 9), ylim = c(0, 1), type = "n", axes = TRUE, ann = FALSE, xaxs = "i", yaxs = "i")
# for(i in 0:9) rect(i, 0, i+1, 1, border = FALSE, col = x$col[i+1])


pred.name.link <- read.csv("predator.variables.csv", stringsAsFactors = FALSE)
out <- join(out, pred.name.link)

r.values.wide <- cast(out, region + lag ~ common.predator.variable, value = "corr")
r.values.wide <- r.values.wide[order(r.values.wide$region, r.values.wide$lag), ]

p.values.wide <- cast(out, region + lag ~ common.predator.variable, value = "pvalue")
p.values.wide <- p.values.wide[order(p.values.wide$region, p.values.wide$lag), ]

cor.long <- melt(r.values.wide, id.vars = c("region", "lag"), na.rm = FALSE)
cor.long <- rename(cor.long, c("value" = "corr"))
row.names(cor.long) <- NULL

pvalue.long <- melt(p.values.wide, id.vars = c("region", "lag"), na.rm = FALSE)
pvalue.long <- rename(pvalue.long, c("value" = "pvalue"))
row.names(pvalue.long) <- NULL

cor.long <- join(cor.long, pvalue.long)

cor.long <- transform(cor.long, col = cols[findInterval(corr, cuts)])
cor.long <- cor.long[order(cor.long$region, cor.long$common.predator.variable, cor.long$lag), ]
cor.long$common.predator.variable <- as.character(cor.long$common.predator.variable)
cor.long$col <- as.character(cor.long$col)
cor.long$col[is.na(cor.long$corr)] <- "#FFFFFF"
cor.long$corr[is.na(cor.long$corr)] <- 0

hom.dat.pred <- join(hom.dat.pred, pred.name.link)
total.pred <- ddply(hom.dat.pred, c("region", "common.predator.variable"), summarize, total = mean(predator.value[which(year >= 1980)]), na.rm = TRUE)
total.pred <- ddply(total.pred, c("region"), transform, prop = total/sum(total)*100)
total.pred <- ddply(total.pred, c("region"), transform, prop.scaled = prop/max(prop))

cor.long <- join(cor.long, total.pred)

preds<- unique(cor.long$common.predator.variable)
regions <- unique(cor.long$region)

d.melt.temp <- read.csv("d.melt.TEMP.csv", stringsAsFactors = FALSE)
region.temp <- ddply(subset(d.melt.temp, variable !="prey" & variable == "environment.lag00"), c("region"), summarize, mean.temp = mean(value), sd.temp = sd(value), median.temp = median(value), q0.1.temp = quantile(value, 0.1), q0.9.temp = quantile(value, 0.9))

regions.df <- data.frame(region = regions)
regions.df <- join(regions.df, region.temp[,c("region", "mean.temp")])
regions.df <- regions.df[order(regions.df$mean.temp), ]


col.axis <- "grey40"
odd <- seq(1, 11, 2)
even <- seq(2, 12, 2)
pdf("../fig/ind.predators.corrs.pdf", width = 5.5, height = 3.5)
par(mfcol = c(length(preds), length(regions)))
par(mar = c(0,0,0,0), cex = 0.6, oma = c(3, 8, 4, 4))
par(tck = 0.1, mgp = c(3, 0.05, 0))
for(i in 1:length(regions)){
  for(j in 1:length(preds)){
    x <- subset(cor.long, region == regions.df$region[i] & common.predator.variable == preds[j])
    plot(1, 1, xlim = c(0, 9), ylim = c(0, 1), type = "n", axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
    for(k in 0:8) {

## all same size:
      #rect(k, 0, k+1, 1, border = FALSE, col = x$col[k+1])

## scaled by size:
      #rect(k, 0, k+1, x$prop.scaled[1]^0.25, border = FALSE, col =
        #x$col[k+1])

## scaled by size and centered:
      height <- 1 - x$prop.scaled[1]^0.25
# background:
      rect(0, 0, 10, 1, col = "#00000002", border = NA)
      rect(k, 0+height/2, k+1, 1-height/2, border = FALSE, col =
        x$col[k+1])
      ## add significance stars:

      if(!is.na(x$pvalue[k+1]))
        if(x$pvalue[k+1] <= 0.05) {
          text(k + .5, .5, labels = "*", col = "white", cex = 1)
          #text(k + .5, .2, labels = "*", col = "white", cex = 1)
          #print(x$pvalue)
        }
    }
    # for(k in 0:8) rect(k, 0, k+1, x$prop.scaled[1]^0.25, border = FALSE, col = x$col[k+1])
    # mtext(unique(x$region), cex = 0.3)
    # mtext(unique(x$common.predator.variable), line = -0.3, cex = 0.3)
    box(col = "grey70", lwd = 0.5)
    if(j %in% length(preds)) axis(1, at = c(0.5, 3.5, 6.5), labels = c(0, 3, 6), col = "grey70", col.axis = col.axis)
    if(j %in% 1 & i %in% odd) mtext(unique(x$region), side = 3, cex =0.6, line = .5)
    if(j %in% 1 & i %in% even) mtext(unique(x$region), side = 3, cex =0.6, line = 1.5)
    if(i == 1) mtext(unique(x$common.predator.variable), side = 2,las = 1, cex =0.6, line = .5)
    # if(i == 9 & j < length(preds)) axis(4, col = "grey70", at = c(0.1, 1)^0.25, labels = c(0.1, 1), las = 1, col.axis = col.axis)
    # if(i == 9 & j == length(preds)) axis(4, col = "grey70", at = c(0, 0.1, 1)^0.25, labels = c(0, 0.1, 1), las = 1, col.axis = col.axis)
    if(x$corr[1] == 0) rect(0, 0, 10, 1, col = "#00000010", border = NA)
  }
}
mtext("Lag (years before lobster)", side = 1, outer = TRUE, cex = 0.6,line = 1.6)
# mtext("Proportion of biomass or abundance", side = 4, outer = TRUE, cex = 0.6,line = 2.0)
dev.off()


# time series plot:
library(ggplot2)
hom.dat.pred <- join(hom.dat.pred, region.temp[,c("region", "mean.temp")])
hom.dat.pred <- transform(hom.dat.pred, region.temp.order = reorder(region, mean.temp))
hom.dat.pred <- ddply(hom.dat.pred, "region", transform, pred.value.scaled.one = predator.value / max(predator.value))
# linetypes:
lty.df <- data.frame(common.predator.variable = unique(hom.dat.pred$common.predator.variable), lty = rep(c(1, 2), 100)[1:length(unique(hom.dat.pred$common.predator.variable))])
hom.dat.pred <- join(hom.dat.pred, lty.df)

library(RColorBrewer)
gg_color_hue <- function(n) {
  # hues = seq(15, 375, length=n+1)
  hues = seq(8, 318, length=n+1)
  hcl(h=hues, l=57.5, c=100)[1:n]
}

p <- ggplot(hom.dat.pred, aes(year, pred.value.scaled.one, colour = common.predator.variable, linetype = common.predator.variable)) + facet_wrap(~region.temp.order) + geom_line() + theme_bw() + xlab("Year") + ylab("Scaled predator abundance or biomass") +
# scale_colour_discrete(name = "Predator") + scale_linetype_manual(values = c(rep(c(1, 2), 6), 1))
scale_linetype_manual(values = c(rep(c(1, 2), 6), 1), name = "Predator") +
# scale_color_manual(values = c(brewer.pal(7, "Set3"), brewer.pal(6, "Set3")))
scale_color_manual(values = gg_color_hue(13), name = "Predator")
# p

ggsave("../fig/scaled_pred_abundance.pdf", width = 11, height = 7)

