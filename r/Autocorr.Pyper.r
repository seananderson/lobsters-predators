Autocorr.Pyper<-function(N,tsx,tsy) 
{
# browser()
lag.max<-min(length(tsx),length(tsy))/4
# Sean July 13 2009 - added na.action = na.pass here to avoid errors
# Is this OK?
rxx<-acf(tsx,lag.max=lag.max,plot=F, na.action = na.pass)$acf[-1,1,1]
ryy<-acf(tsy,lag.max=lag.max,plot=F, na.action = na.pass)$acf[-1,1,1]
#print(rxx)
#print(ryy)
Nstar<-N/(1+2*sum(rxx*ryy))
ifelse(Nstar<N,Nstar,N)
}

