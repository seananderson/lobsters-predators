###correlations  lobsters, predators, temperature in the NW Atlantic   
# By Stephanie Boudreau
# Created February 2011
# Modified Feb 28, 2011
####################

####1) Correlate lobster landings with other lobster indices########

#I have CT currently read in
#try using subset to pull out each variable. e.g. x<-subset(data.df,select=c("variable(s)",logical)

#require(car) #needed for reg.line
#pull out all of the lobster abudance proxies from the dataset
lob.land.ct<-subset(hom_CT, variable=="lob.land", select=c("year", "value"))
lob.abund.fall.ct<- subset(hom_CT, variable=="lob.abund.fall", select=c("year", "value"))
lob.abund.fall.fe.ct<- subset(hom_CT, variable=="lob.abund.fall.f", select=c("year", "value"))
lob.abund.fall.m.ct<-subset(hom_CT, variable=="lob.abund.fall.m", select=c("year", "value")) 
lob.abund.spring.ct<-subset(hom_CT, variable=="lob.abund.spring",select=c("year", "value"))
lob.abund.spring.fe.ct<- subset(hom_CT, variable=="lob.abund.spring.f",select=c("year", "value"))
lob.abund.spring.m.ct<- subset(hom_CT, variable=="lob.abund.spring.m",select=c("year", "value"))
lob.bio.fall.ct <-  subset(hom_CT, variable=="lob.bio.fall",select=c("year", "value"))
lob.bio.spring.ct<- subset(hom_CT, variable=="lob.bio.spring",select=c("year", "value"))
lob.fullrecruit.fall.ct <-  subset(hom_CT, variable=="lob.fullrecruit.fall",select=c("year", "value"))
lob.fullrecruit.spring.ct <-  subset(hom_CT, variable=="lob.fullrecruit.spring",select=c("year", "value"))
lob.lic.ct <-  subset(hom_CT, variable=="lob.lic",select=c("year", "value"))
lob.postrec.fe.ct<-  subset(hom_CT, variable=="lob.postrec.f",select=c("year", "value"))
lob.postrec.m.ct<-  subset(hom_CT, variable=="lob.postrec.m",select=c("year", "value"))
lob.rec.fe.ct<-  subset(hom_CT, variable=="lob.rec.f",select=c("year", "value"))
lob.rec.m.ct<-  subset(hom_CT, variable=="lob.rec.m",select=c("year", "value"))
lob.recruit.fall.ct<-  subset(hom_CT, variable=="lob.recruit.fall",select=c("year", "value"))
lob.recruit.spring.ct<-  subset(hom_CT, variable=="lob.recruit.spring",select=c("year", "value"))
lob.ssa.fall.fe.ct<-  subset(hom_CT, variable=="lob.ssa.fall.f",select=c("year", "value"))
lob.ssa.spring.fe.ct<-  subset(hom_CT, variable=="lob.ssa.spring.f",select=c("year", "value"))
lob.trap.ct<-  subset(hom_CT, variable=="lob.trap",select=c("year", "value"))

#Merge it all into one , keep all NAs to include all years                
#lob.ct<-merge(lob.land.ct, lob.abund.sp.ct, by="year", all=FALSE) #this also works but only inlcudes the years with positive values for both (no NAs)
lob.ct<-merge(lob.land.ct, lob.abund.fall.ct, by="year", all=TRUE)# this works. Merges the columns by year, includes the NAs
lob.ct<-merge(lob.ct, lob.abund.fall.fe.ct, by="year", all=TRUE)    #
lob.ct<-merge(lob.ct, lob.abund.fall.m.ct, by="year", all=TRUE)     #
lob.ct<-merge(lob.ct, lob.abund.spring.ct, by="year", all=TRUE)     #
lob.ct<-merge(lob.ct, lob.abund.spring.fe.ct, by="year", all=TRUE)  #
lob.ct<-merge(lob.ct, lob.abund.spring.m.ct, by="year", all=TRUE)  #
lob.ct<-merge(lob.ct, lob.bio.fall.ct, by="year", all=TRUE)     #
lob.ct<-merge(lob.ct,lob.bio.spring.ct, by="year", all=TRUE)   #
lob.ct<-merge(lob.ct,lob.fullrecruit.fall.ct, by="year", all=TRUE)  #
lob.ct<-merge(lob.ct,lob.fullrecruit.spring.ct, by="year", all=TRUE)    #
lob.ct<-merge(lob.ct,lob.lic.ct, by="year", all=TRUE)    #
lob.ct<-merge(lob.ct,lob.postrec.fe.ct, by="year", all=TRUE)    #
lob.ct<-merge(lob.ct,lob.postrec.m.ct, by="year", all=TRUE)  #
lob.ct<-merge(lob.ct,lob.rec.fe.ct, by="year", all=TRUE)     #
lob.ct<-merge(lob.ct,lob.rec.m.ct, by="year", all=TRUE)    #
lob.ct<-merge(lob.ct,lob.recruit.fall.ct, by="year", all=TRUE)  #
lob.ct<-merge(lob.ct,lob.recruit.spring.ct, by="year", all=TRUE)      #
lob.ct<-merge(lob.ct,lob.ssa.fall.fe.ct, by="year", all=TRUE)    
lob.ct<-merge(lob.ct,lob.ssa.spring.fe.ct, by="year", all=TRUE)  
lob.ct<-merge(lob.ct,lob.trap.ct, by="year", all=TRUE)           

colnames(lob.ct)<-c("year","lob.land.ct","lob.abund.fall.ct","lob.abund.fall.fe.ct","lob.abund.fall.m.ct","lob.abund.spring.ct",
"lob.abund.spring.fe.ct","lob.abund.spring.m.ct","lob.bio.fall.ct","lob.bio.spring.ct","lob.fullrecruit.fall.ct","lob.fullrecruit.spring.ct",
"lob.lic.ct","lob.postrec.fe.ct","lob.postrec.m.ct","lob.rec.fe.ct","lob.rec.m.ct","lob.recruit.fall.ct","lob.recruit.spring.ct","lob.ssa.fall.fe.ct",
"lob.ssa.spring.fe.ct","lob.trap.ct") #renames the columns value.x and value.y etc to lob.land ...

lob.ct

#lob.abund.fall.ct
cor.test(lob.ct$lob.abund.fall.ct,lob.ct$lob.land.ct, na.action=na.omit) # p-value = 5.124e-07,  95% CI:0.6421466 0.9242194,  cor 0.8304291 
ccf(lob.ct$lob.abund.fall.ct,lob.ct$lob.land.ct, na.action=na.omit) #year 0 strongest + cor
#lob.abund.fall.fe.ct
cor.test(lob.ct$lob.abund.fall.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 6.293e-05,  95% CI: 0.4533044 0.8726892,  cor 0.7242615  
ccf(lob.ct$lob.abund.fall.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #year 0 strongest + cor
#lob.abund.fall.m.ct
cor.test(lob.ct$lob.abund.fall.m.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 2.042e-05,  95% CI:  0.5048723 0.8877534,  cor 0.754587 
ccf(lob.ct$lob.abund.fall.m.ct,lob.ct$lob.land.ct, na.action=na.omit) #year 0 strongest + cor
#lob.abund.spring.ct
cor.test(lob.ct$lob.abund.spring.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 0.0009032,  95% CI:  0.3081678 0.8255242,  cor 0.6328878 
ccf(lob.ct$lob.abund.spring.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 1 strongest + cor
#lob.abund.spring.fe.ct
cor.test(lob.ct$lob.abund.spring.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value =  0.0005629, 95% CI:  0.3453381 0.8445013,  cor 0.6631425 
ccf(lob.ct$lob.abund.spring.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 1 strongest + cor
#lob.abund.spring.m.ct
cor.test(lob.ct$lob.abund.spring.m.ct,lob.ct$lob.land.ct, na.action=na.omit) # p-value =  0.0003451,  95% CI: 0.3741130 0.8537216 ,  cor 0.6812582 
ccf(lob.ct$lob.abund.spring.m.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 1 strongest + cor
#lob.bio.fall.ct
cor.test(lob.ct$lob.bio.fall.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 3.041e-06,  95% CI:  0.7151861 0.9629460,  cor 0.8939625  
ccf(lob.ct$lob.bio.fall.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 0 strongest + cor
#lob.bio.spring.ct
cor.test(lob.ct$lob.bio.spring.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 0.001440,  95% CI: 0.3604282 0.8985366 ,  cor 0.726358  
ccf(lob.ct$lob.bio.spring.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 1 strongest + cor
#lob.fullrecruit.fall.ct
cor.test(lob.ct$lob.fullrecruit.fall.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value =  2.818e-05,  95% CI:  0.4906371 0.8836753,  cor 0.746321   
ccf(lob.ct$lob.fullrecruit.fall.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 0 strongest + cor
#lob.fullrecruit.spring.ct
cor.test(lob.ct$lob.fullrecruit.spring.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 9.737e-05,  95% CI:  0.4426563 0.8745368,  cor 0.7229468  
ccf(lob.ct$lob.fullrecruit.spring.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 1 + strongest cor
#lob.lic.ct
cor.test(lob.ct$lob.lic.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value =  0.06162,  95% CI: -0.01806786  0.65390017 ,  cor 0.3644484  
ccf(lob.ct$lob.lic.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 0 not sig. Sig correlations at neg (+ cor, lags -1 to -8) and pos (- cor, 6 to 10) lags 
#lob.postrec.fe.ct
cor.test(lob.ct$lob.postrec.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value =  0.00528,  95% CI: 0.2060576 0.8120389 ,  cor 0.5856495 
ccf(lob.ct$lob.postrec.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag -1 strongest + cor, lag year 0 also + sig
#lob.postrec.m.ct
cor.test(lob.ct$lob.postrec.m.ct,lob.ct$lob.land.ct, na.action=na.omit) #   p-value =0.002312 ,  95% CI: 0.2689948 0.8335679,  cor 0.6277825    
ccf(lob.ct$lob.postrec.m.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 0 strongest + cor
#lob.rec.fe.ct
cor.test(lob.ct$lob.rec.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 0.0003482 ,  95% CI:  0.3946251 0.8719607,  cor 0.7060356   
ccf(lob.ct$lob.rec.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 0 strongest + cor
#lob.rec.m.ct
cor.test(lob.ct$lob.rec.m.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 0.0003482,  95% CI: 0.3946251 0.8719607,  cor 0.7060356  
ccf(lob.ct$lob.rec.m.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 0 strongest + cor
#lob.recruit.fall.ct
cor.test(lob.ct$lob.recruit.fall.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 1.703e-05,  95% CI:0.5127287 0.8899786,  cor 0.7591152  
ccf(lob.ct$lob.recruit.fall.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 0 strongest + cor
#lob.recruit.spring.ct
cor.test(lob.ct$lob.recruit.spring.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 0.0004037,  95% CI:0.3650274 0.8508424,  cor 0.6755786  
ccf(lob.ct$lob.recruit.spring.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 1 strongest + cor
#lob.ssa.fall.fe.ct
cor.test(lob.ct$lob.ssa.fall.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value =3.941e-05,  95% CI:0.4753685 0.8792339,  cor 0.7373663   
ccf(lob.ct$lob.ssa.fall.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 0 strongest + cor
#lob.ssa.spring.fe.ct
cor.test(lob.ct$lob.ssa.spring.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value =0.0001592,  95% CI:0.416982 0.866922,  cor 0.707567  
ccf(lob.ct$lob.ssa.spring.fe.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 1 strongest + cor
#lob.trap.ct
cor.test(lob.ct$lob.trap.ct,lob.ct$lob.land.ct, na.action=na.omit) #  p-value = 0.04002,  95% CI:  0.02293765 0.71603688,  cor 0.4310641  
ccf(lob.ct$lob.trap.ct,lob.ct$lob.land.ct, na.action=na.omit) #year lag 1 strongest + cor   (0 to 4 yrs significant)

##########################################################
pdf(filename, width=10, height=8)
filename = "../fig/CT/hom_CT_cor_ellipse.pdf"
cor.ct<-cor(lob.ct, use="na.or.complete") #correlates everything with everything
require(ellipse) #required for plotcorr, plots ellipses to demo strenth of cor, alternative, numbers=TRUE will plot numbers where 10=1
##plotcorr(cor.ct,numbers = TRUE, type = "lower", diag = TRUE)
#plotcorr(cor.ct)
ord <- order(cor.ct[1,])
xc <- cor.ct[ord, ord]
colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   

#plotcorr(xc, col=colors[5*xc + 6])
#plotcorr(xc, col=colors[5*xc + 6], type = "upper")
plotcorr(xc, col=colors[5*xc + 6], type = "lower", diag = TRUE)
dev.off()

cor.ct<-cor(lob.ct$lob.land, lob.ct, use="na.or.complete")#correlates landings only with the rest of the variables

######################################################################################################################################
######################################################################################################################################
#Merge it all into one , remove all NAs only positive years 1992-2003    I DON'T THINK IT MAKES SENSE TO USE THIS           
#lob.ct<-merge(lob.land.ct, lob.abund.sp.ct, by="year", all=FALSE) #this also works but only inlcudes the years with positive values for both (no NAs)
lob.ct<-merge(lob.land.ct, lob.abund.fall.ct, by="year", all=FALSE)# this works. Merges the columns by year, includes the NAs
lob.ct<-merge(lob.ct, lob.abund.fall.fe.ct, by="year", all=FALSE)    #
lob.ct<-merge(lob.ct, lob.abund.fall.m.ct, by="year", all=FALSE)     #
lob.ct<-merge(lob.ct, lob.abund.spring.ct, by="year", all=FALSE)     #
lob.ct<-merge(lob.ct, lob.abund.spring.fe.ct, by="year", all=FALSE)  #
lob.ct<-merge(lob.ct, lob.abund.spring.m.ct, by="year", all=FALSE)  #
lob.ct<-merge(lob.ct, lob.bio.fall.ct, by="year", all=FALSE)     #
lob.ct<-merge(lob.ct,lob.bio.spring.ct, by="year", all=FALSE)   #
lob.ct<-merge(lob.ct,lob.fullrecruit.fall.ct, by="year", all=FALSE)  #
lob.ct<-merge(lob.ct,lob.fullrecruit.spring.ct, by="year", all=FALSE)    #
lob.ct<-merge(lob.ct,lob.lic.ct, by="year", all=FALSE)    #
lob.ct<-merge(lob.ct,lob.postrec.fe.ct, by="year", all=FALSE)    #
lob.ct<-merge(lob.ct,lob.postrec.m.ct, by="year", all=FALSE)  #
lob.ct<-merge(lob.ct,lob.rec.fe.ct, by="year", all=FALSE)     #
lob.ct<-merge(lob.ct,lob.rec.m.ct, by="year", all=FALSE)    #
lob.ct<-merge(lob.ct,lob.recruit.fall.ct, by="year", all=FALSE)  #
lob.ct<-merge(lob.ct,lob.recruit.spring.ct, by="year", all=FALSE)      #
lob.ct<-merge(lob.ct,lob.ssa.fall.fe.ct, by="year", all=FALSE)    
lob.ct<-merge(lob.ct,lob.ssa.spring.fe.ct, by="year",all=FALSE)  
lob.ct<-merge(lob.ct,lob.trap.ct, by="year", all=FALSE)           

colnames(lob.ct)<-c("year","lob.land.ct","lob.abund.fall.ct","lob.abund.fall.fe.ct","lob.abund.fall.m.ct","lob.abund.spring.ct",
"lob.abund.spring.fe.ct","lob.abund.spring.m.ct","lob.bio.fall.ct","lob.bio.spring.ct","lob.fullrecruit.fall.ct","lob.fullrecruit.spring.ct",
"lob.lic.ct","lob.postrec.fe.ct","lob.postrec.m.ct","lob.rec.fe.ct","lob.rec.m.ct","lob.recruit.fall.ct","lob.recruit.spring.ct","lob.ssa.fall.fe.ct",
"lob.ssa.spring.fe.ct","lob.trap.ct") #renames the columns value.x and value.y etc to lob.land ...

plot(log(lob.ct$lob.abund.spring), log(lob.ct$lob.land), pch=16)##
reg.line(lm(log(lob.ct$lob.land)~(log(lob.ct$lob.abund.spring))), col="black") #
summary(lm(log(lob.ct$lob.land)~(log(lob.ct$lob.abund.spring)))) #Residual SE: 0.4484 on 22 df, Multiple R^2: 0.3812, Adjusted R^2: 0.353, p-value: 0.001309 
#ccf(x, y, lag.max = NULL, type = c("correlation", "covariance"), plot = TRUE, na.action = na.fail, ...)

ccf(log(lob.ct$lob.abund.fall.ct), log(lob.ct$lob.land), ci.col = "black")  #correlation is the default type, y leads x on the positive side of 0, y=landings, x=abundance
ccf(log(lob.ct$lob.abund.fall.fe.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.abund.fall.m.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.abund.spring.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.abund.spring.fe.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.abund.spring.m.ct ), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.bio.fall.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.bio.spring.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.fullrecruit.fall.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.fullrecruit.spring.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.lic.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.postrec.fe.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.postrec.m.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.rec.fe.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.rec.m.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.recruit.fall.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.recruit.spring.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.ssa.fall.fe.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.ssa.spring.fe.ct), log(lob.ct$lob.land), ci.col = "black")
ccf(log(lob.ct$lob.trap.ct), log(lob.ct$lob.land), ci.col = "black")





lob.abund.fall         lob.abund.fall.f      lob.abund.fall.m   lob.abund.spring lob.abund.spring.f     lob.abund.spring.m     lob.bio.fall 
lob.bio.spring         lob.fullrecruit.fall   lob.fullrecruit.spring   lob.lic   lob.postrec.f   lob.postrec.m      lob.rec.f              
lob.rec.m     lob.recruit.fall    lob.recruit.spring     lob.ssa.fall.f    lob.ssa.spring.f       lob.trap       #lob.land                            