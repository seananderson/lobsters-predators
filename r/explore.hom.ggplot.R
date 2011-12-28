explore.hom.ggplot <- function(data){
  require(ggplot2)
  require(plyr)
# d_ply is a function from the plyr package. This means, take the
  # dataframe, split it up by region, and then feed it into the
  # following code with that "chunk" of data called "x".
  d_ply(data, "region", function(x) {
    pdf(paste("../fig/exploratory_plots/", unique(x$region), ".pdf", sep = ""), width=14, height=8)
      # create the main plot
      # take the data (called x within the d_ply function) and plot
      # year against value, plot it as lines and wrap it by "variable" 
      p <- ggplot(x, aes(year, value)) + geom_line() + facet_wrap(~variable, scales = "free_y")
      p <- p + opts(axis.text.x= theme_text(size = 8,col="darkgrey"),axis.text.y= theme_text(size = 8, col = "darkgrey")) # make the axis text a bit smaller and grey
      #p <- p + scale_x_continuous(breaks=seq(1960, 2010, 20))
      p <- p + opts(strip.text.x = theme_text(size = 8)) # make the label text a bit smaller
      # the plot is just an object, so we need to print it to see it:
      print(p)
    dev.off()
  })
}

