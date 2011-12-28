# ====================================================================
# Created:       Nov 25, 2011
# Last modified: Dec 06, 2011
# Purpose:       scale data onto same units so that coefficients are
# roughly comparable - also logs the data
# ====================================================================

scale.dat <- function(x){     #addresses the different units
  #x <- log(x)
  x <- x - mean(x)
  x <- x / (2 * sd(x))
  x
}



