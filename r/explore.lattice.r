###exploratory plot in Lattice, data for region. Lobster, predators, temperature in the NW Atlantic hom
##Jan 2011 

explore.lattice <- function(data, filename){
 require(lattice)
 #xyplot(value~year|variable,data=hom, scales=list(y=list(relation="free"))) #multipanel plot, scales y-axis is scaled appropriately to the data

pdf(filename, width=10, height=8)

print(xyplot(value~year|variable,
 data = data,
 scales = list(
 y = list(relation = "free", cex = 0.5),
 x = list(cex = 0.5)),
 pch = 20, col = "black", cex = 0.6,
 as.table = TRUE,
type = "o",
 strip = strip.custom(bg = "transparent", par.strip.text = list(cex = 0.58))
 ))
 
dev.off()
}
