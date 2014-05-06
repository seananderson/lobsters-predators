#' Make a map

library(maps)
library(plyr)
library(PBSmapping)
data(worldLLhigh)


# Starting with code from the cod crab analysis:
# if you want to re-read all the data - this takes a while:
# insert new file here:
#d <- read.table("../map-data/clim_7971/clim_7971_j.txt", sep = ",", header = TRUE)
#d <- read.table("../map-data/clim_7970/clim_7970_j.txt", sep = ",", header = TRUE)
#d <- read.table("../map-data/clim_8107/clim_8107_j.txt", sep = ",", header = TRUE)
#d <- d[,c("AREA_NAME", "YEAR", "MONTH", "TEMPERATURE")]
d <- readRDS("../map-data/clim_8107/clim_8107_j.rds")

d<-subset(d, TEMPERATURE < 30) # outliers that must be mistakes

#d <- readRDS("../map-data/clim_7970/clim_7970_j.rds")

#d3 <- ddply(d, c("AREA_NAME", "YEAR", "MONTH"), summarize, TEMP = mean(TEMPERATURE))

#d3 <- d3[!is.na(d3$TEMP), ]


#d3 <- ddply(d3, c("AREA_NAME", "YEAR"), transform, months = length(TEMP))

#d3 <- subset(d3, months >= 1)

d2 <- ddply(d, c("AREA_NAME"), function(x) {
  if(nrow(x) >= 1)
    temp <- median(x$TEMPERATURE, na.rm = TRUE)
  else
    temp <- NA
  data.frame(temp = temp)
})

d2 <- d2[!is.na(d2$temp), ]



d2$long <- as.numeric(sub("(-[0-9.]+), ([0-9.]+)", "\\1", d2$AREA_NAME))
d2$lat <- as.numeric(sub("(-[0-9.]+), ([0-9.]+)", "\\2", d2$AREA_NAME))

#d2 <- subset(d2, temp < 30)
#d2$temp[d2$temp >= 16] <- 16

col_range <- c(0, max(d2$temp)+0.01)

# is there bad data to remove?
#if(max(d2$temp) > 100) d2 <- d2[-which(d2$temp > 100), ]

# colours:
#cols <- hsv(exp(seq(log(4.4/6), log(0.06), length.out = 300)))

source("smooth.pal.R")

library(RColorBrewer)
cols <- rev(brewer.pal(9, "Spectral"))
cols <- smooth.pal(cols, 38)[-c(1, 2, 303, 304)]

#opacity <- 98
#cols <- sapply(cols, function(x) paste(x, opacity, sep=""))
cols.temp <- data.frame(temp = c(-999, seq(col_range[1], col_range[2], length.out = 298), 999), col = cols)

d2$col <- sapply(1:nrow(d2), function(x) {
as.character(cols.temp[min((1:300)[d2$temp[x] <= cols.temp$temp]), 2])
})

d2$long <- d2$long + 360


# or load it from before:
save(d2, cols.temp, file = "map-colour-temperature-data.RData")
#load("map-colour-temperature-data.RData")


map.points <- function(long, lat, col, incr = 0.505) {
 rect(long - incr, lat - incr, long + incr, lat + incr, col = col,
   border = NA)
}

pdf("../fig/map-colour.pdf",width = 5, height = 4.2)
par(cex = 0.9, oma = c(0,0,0,0), mar = c(0,0,0,0), tck = 0.015, mgp = c(2, 0.4, 0))
plotMap(worldLLhigh,xlim = c(285,310),ylim = c(39,52.5),tckMinor = 0, xlab = "", lwd=0.75 , type = "n", las = 1, tck = 0.015, col = "white", ylab = "", axes = FALSE, bty = "n")

with(d2, map.points(long, lat, col = col))

par(new = TRUE)
plotMap(worldLLhigh,xlim = c(285,310),ylim = c(39,52.5),tckMinor = 0, xlab = expression(Longitude~(degree~E)), lwd=0.5 , border = "grey35", las = 1, tck = 0.015, col = "grey93", ylab = expression(Latitude~(degree~N)))

# north
# arrows(287, 54.7, 287, 55.6, length = 0.05)
# #
# # text(318.8, 48.3, "N", cex = 0.75)
# text(287, 56.2, "N", cex = 0.8)

plot_region <- function(x, y, label, col = "#ffffff45", cex = 0.9){
    text(x, y, labels = label, cex = cex)
}

l <- structure(list(
  x = c(297.675, 297.303, 292.388, 302.335, 287.29, 288.873, 290.769, 291.287, 293.527),
  y = c(47.102, 43.868, 43.551, 46.814, 40.066, 40.958, 42.532, 40.703, 41.816),
  label = c("SGSL", "NS", "GOM", "NL", "CT", "RI", "MA", "SNE", "GB")),
  .Names = c("x", "y", "label"), row.names = c(NA, -9L), class = "data.frame")

for(i in 1:nrow(l)) {
  plot_region(l[i, "x"], l[i, "y"], l[i, "label"])
}

text(285.5, 51.5, expression(Temperature~(degree~C)), srt = 0, col = "black", cex = 0.8, pos = 4)
#arrows(294, 52, 294, 53, length = 0.05)
#text(294, 53.5, "N", cex = 0.75)


rect(300, 38, 315, 44, col = "grey98")

box()
library(shape)
colorlegend(col = substr(as.character(cols.temp$col), 1, 7),
  col_range, cex = 0.65, lwd = 0.5, posx = c(0.17, 0.20), posy =
  c(0.35, 0.90), zval = seq(0, max(d2$temp), 10))


nao <- read.csv("../map-data/naoi.csv")
nao <- subset(nao, year > 1960)
par(new = TRUE)
par(fig = c(0.69, 0.93, 0.17, 0.38))
par(mar = c(1,1,0,0), cex = 0.7, mgp = c(2, 0.2, 0))
plot(nao$year, nao$nao, type = "l", axes = FALSE)
par(mgp = c(2, 0.6, 0))
axis(1, las = 1, tck = 0.05, padj = -0.4, mgp = c(2, 0.3, 0))
axis(2, las = 1, at = c(-4, 0, 4), tck = 0.05, mgp = c(2, 0.3, 0))
abline(h = 0, lty = 2)
mtext("NAOI", side = 2, cex = 0.7, line = 1.7)



dev.off()

