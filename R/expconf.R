
#' Confidence Functions for an Exponential Distribution
#'
#' Computes confidence functions for exponential distributions 
#'
#' @param x a (non-empty) numeric vector of data values
#' @param method a character string specifying the method of creating the functions, must be one of "wilks" or "pivot"
#' @param plot a logical indicating whether you want a paired t-test
#' @return list with distribution curve, confidence curve, density                curve, and quantile function
#' @examples 
#' exp.conf(x=rexp(10), method="pivot")
#' exp.conf(x=rexp(100), method="wilks")

exp.conf <- function(x, method, plot = TRUE) {
  
  xbar <- mean(x)
  n <- length(x)
  
  if(method=="pivot"){
    
    # Confidence Distribution 
    
    exp.dist.function <- function(lambda) pchisq(2*lambda*xbar*n, df = n*2)
    
    # Confidence Density
    
    exp.den.function <- function(lambda) (2*xbar*n)*dchisq((2*lambda*xbar*n), df = n*2) 
    
    # Confidence Curve
    
    exp.confcurve.function <- function(lambda) abs(2*(pchisq(2*lambda*xbar*n, df = n*2))-1)
    
    # Confidence Quantile
    
    exp.quant.function <- function(p) qchisq(p, df=2*n)/(2*n*xbar)
    
    pivot.list <- list(pconf = exp.dist.function, dconf = exp.den.function, cconf = exp.confcurve.function, qconf = exp.quant.function)
    
    
    if(plot == TRUE){
      
      crit.left<- qchisq(0.0005, df=2*n)/(2*xbar*n)
      crit.right <- qchisq(0.9995, df=2*n)/(2*xbar*n)
      
      curve(exp.dist.function(lambda), from= crit.left, to=crit.right, xname="lambda", main = "Confidence Distribution")
      
      curve(exp.den.function(lambda), from= crit.left, to=crit.right, xname = "lambda", main= "Confidence Density") 
      
      curve(exp.confcurve.function(lambda),from=crit.left, to=crit.right, xname="lambda" , main = "Confidence Curve")
      
    }
    
    return(pivot.list)
    
  } else if (method=="wilks"){
    
    xbar <- mean(x)
    theta.hat <- 1/xbar
    
    log.lik.x <- function(theta) (n*log(theta)-theta*n*xbar)
    
    # Confidence Distribution 
    
    wilks.confdist.function <- function(theta) 
      pnorm(sign(theta-theta.hat)*sqrt(2*(log.lik.x(theta.hat)-log.lik.x(theta))))
    
    # Confidence Density
    
    wilks.density.function <- function(theta) dnorm(sign(theta-theta.hat)*sqrt(2*(log.lik.x(theta.hat)-log.lik.x(theta))))*(-n/theta+n*xbar)/sqrt(2*(log.lik.x(theta.hat)-log.lik.x(theta)))*sign(theta-theta.hat)
    
    # Confidence Curve
    
    wilks.confcurve.function <- function(theta) 
      abs(2*(pnorm(sign(theta-theta.hat)*sqrt(2*(log.lik.x(theta.hat)-log.lik.x(theta)))))-1)	
    
    wilks.list <- list(pconf = wilks.confdist.function, dconf = wilks.density.function, cconf =  wilks.confcurve.function)
    
    if(plot == TRUE){
      
      crit.left<- qchisq(0.0005, df=2*n)/(2*xbar*n)
      crit.right <- qchisq(0.9995, df=2*n)/(2*xbar*n)
      
      
      curve(wilks.confdist.function(theta), from= crit.left, to=crit.right, xname= "theta",main = "Confidence Distribution")
      
      curve(wilks.density.function(theta), from= crit.left, to=crit.right, xname = "theta", main = "Confidence Density")
      
      curve(wilks.confcurve.function(theta), from= crit.left, to=crit.right, xname = "theta", main = "Confidence Curve")
      
    }
    return(wilks.list)
    
  }
}