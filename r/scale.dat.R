# ====================================================================
# Created:       Nov 25, 2011
# Last modified: Apr 01, 2013
# Purpose:       scale data onto same units so that coefficients are
# roughly comparable - also logs the data
# ====================================================================

scale.dat <- function(x){ 
  #x <- log(x)
  x <- x - mean(x, na.rm = TRUE)
  x <- x / (sd(x, na.rm = TRUE))
  x
}



