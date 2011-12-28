
#"nao"

for(i in 4:9) {
pred <- subset(hom.dat, type == "predator")
prey <- subset(hom.dat, type == "prey")
names(prey)[3] <- "prey.value"
names(pred)[3] <- "pred.value"
names(pred)[1] <- "pred.name"
pred$pred.year.original <- pred$year
pred$year <- pred$year + i

d <- merge(pred[,c("year", "region", "pred.value", "pred.name", "pred.year.original")], prey[,c("year", "region", "prey.value")])


d$pred.name <- paste(abbreviate(d$region), d$pred.name, sep = ".")
d <- ddply(d, c("region", "pred.name"), transform, pred.total = sum(pred.value))
d <- transform(d, pred.name = reorder(pred.name, pred.total))

scale.dat.log <- function(x){ 
  x <- log(x + 0.5)
  x <- x - mean(x)
  x <- x / (2 * sd(x))
  x
}
d <- ddply(d, c("region", "pred.name"), transform, scaled.prey.value = scale.dat.log(prey.value), scaled.pred.value = scale.dat.log(pred.value))

d.cor.out <- ddply(d, c("region", "pred.name"), function(x) {
  cor.out <- with(x, cor.test(scaled.pred.value, scaled.prey.value))
  l <- as.numeric(cor.out$conf.int)[1]
  u <- as.numeric(cor.out$conf.int)[2]
  r <- as.numeric(cor.out$estimate)
  data.frame(l = l, u = u, r = r)
})

library(ggplot2)
p <- ggplot(d.cor.out, aes(y = pred.name, x = r, colour = r)) + geom_point() + facet_wrap(~region, scales = "free_y") + geom_segment(aes(l, pred.name, xend = u, yend = pred.name)) + geom_vline(xintercept = 0);ggsave(paste("../fig/predators-cor-lag-", i, ".pdf", sep = ""), width = 16, height = 10)

}

