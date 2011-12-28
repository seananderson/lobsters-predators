###############################################################
### Created by:    RAM (originally)
### Contact:       sean.charles.anderson@gmail.com
### Created:       Tue 13 Oct 2009 03:50:37 PM ADT
### Last modified: Tue 13 Oct 2009 05:24:50 PM ADT
### Purpose:       General function to standardize temperature 
###                series from the DFO Oceographic database. 
###                Modified Tue Oct 13 2009 by Sean Anderson.
### Instructions:  Data should be in the format of column 1 = 
###                year, column 2 = month, column 3 = temperature
###############################################################
standardize.temp <- function(dat){
  # General function to standardize temperature series from the DFO Oceographic
  # database.   
  # Data should be in the format of: 
  #      column 1 = year, 
  #      column 2 = month, 
  #      column 3 = temperature

  require(epicalc) # for adjusted means from a glm()

  year<-factor(dat[, 1])                                            
  month<-factor(dat[, 2])
  temp<-dat[, 3]

  junk<-data.frame(year,month, temp)

  par(mfrow=c(2,1))

  temp.aov <- glm(temp ~ year + month, family=gaussian)

  plot(fitted(temp.aov), residuals(temp.aov))
  assign("junk", 0)

  adjmeans <- adjust(month, list(year), model=temp.aov)
  tempser <- adjmeans$mean
  plot(sort(unique(year)), tempser)

  data.frame(year = as.numeric(as.character(sort(unique(year)))), temperature=tempser)
}

