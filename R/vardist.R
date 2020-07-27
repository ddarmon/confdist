#` Confidence Functions for Variance 
#`
#` \code{var.conf} returns plots and functions for the PDF, CDF, and the confidence curve, and only the quantile function 
#`
#` @param x is a vector of the data values
#` @param plot=TRUE turns on the plots when plot=FALSE will only provide the functions
#`
#` @return If all inputs are integer the output will provide list of functions and plots of the confidence distribution, the confidence curve, and the confidence density.
#`
#` @example
#`var.fun<-var.conf(x=c(1,2,3,4,5),plot=FALSE)
#`var.fun$(pconf(0.5))

var.conf<- function(x,plot=TRUE){
  n<- length(x)
  S<-sd(x)
  crit.L<-((n-1)*sd(x)^2)/qchisq(0.975,df=n-1)
  crit.R<-((n-1)*sd(x)^2)/qchisq(0.025,df=n-1)
  if(plot==TRUE){
    pconf.var<- function(var)1-pchisq(((n-1)*S^2)/var,df=n-1)
    curve(pconf.var,xname="var",main="Confidence Distribution", from=crit.L,to=crit.R)
    
    cconf.var<- function(var)abs(2*(1-pchisq(((n-1)*S^2)/var,df=n-1))-1)
    curve(cconf.var,xname="var",main="Confidence Curve",ylab="Confidence level", from=crit.L,to=crit.R)
    
    dconf.var<-function(var)dchisq(((n-1)*S^2)/var,df=n-1)*((n-1)*S^2)/var^2
    curve(dconf.var,xname="var",main="Confidence Density", from=crit.L,to=crit.R)
    
    qconf.var<- function(p)((n-1)*S^2)/qchisq(1-p,df=n-1)
    
    conf.fun<- list(pconf=pconf.var,dconf=dconf.var,cconf=cconf.var,qconf=qconf.var)
    return(conf.fun)
  }else{
    pconf.var<- function(var)1-pchisq(((n-1)*S^2)/var,df=n-1)
    cconf.var<- function(var)abs(2*(1-pchisq(((n-1)*S^2)/var,df=n-1))-1)
    dconf.var<-function(var)dchisq(((n-1)*S^2)/var,df=n-1)*((n-1)*S^2)/var^2
    qconf.var<- function(p)((n-1)*S^2)/qchisq(1-p,df=n-1)
    
    conf.fun<- list(pconf=pconf.var,dconf=dconf.var,cconf=cconf.var,qconf=qconf.var)
    return(conf.fun)}
}