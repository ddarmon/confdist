#' Confidence functions for prop Test
#'
#' Computes confidence functions for proportion tests on vectors of data.
#' 
#' @param x a vector of counts of successes.
#' @param n a vector of counts of trials.
#' @param method a character string specifying the method of creating the functions, must be one of "mid" (default) or "Z"
#' @param plot a logical indicating whether you want the plots to be generated.
#' @param fullrange a character string specifying the range of the plots, must be one of "99.9 CI" (default) or "full"
#' @return A list containing the confidence functions.
#' @examples
#' prop.conf(300, 733)



prop.conf <- function (x, n, method = "mid", plot = TRUE, fullrange = "99.9 CI") {
  
  if(method == "Z"){
    p.hat <- x/n
    
    prop.dist.func <- function(p) 1-pnorm((p.hat - p)/sqrt(p*(1-p)/n))
    
    prop.deriv <- function(p) -1/2*(p - p.hat)*((p - 1)/n + p/n)/(-(p - 1)*p/n)^(3/2) - 1/sqrt(-(p - 1)*p/n)
    
    prop.dens.func <- function(p) -dnorm((p.hat - p)/sqrt(p*(1-p)/n))*prop.deriv(p)
    
    prop.curv.func <- function(p) abs(2*(1-pnorm((p.hat - p)/sqrt(p*(1-p)/n)))-1)
    
    quant.top <- function(p) p.hat + (qnorm(p)^2)/(2*n) + qnorm(p)*sqrt((p.hat*(1-p.hat))/n+((qnorm(p)^2)/(4*n^2)))
    
    quant.bot <- function(p) 1 + (qnorm(p)^2)/n
    
    prop.quant.func <- function(p) quant.top(p)/quant.bot(p)
    
    
    
    x.list <- list(pconf = prop.dist.func, dconf = prop.dens.func, cconf = prop.curv.func, qconf = prop.quant.func)
    
    if (plot){
      if(fullrange == "full"){ 
        curve(prop.dist.func, from = 0, to = 1, n = 2000)
        curve(prop.dens.func, from = 0, to = 1, n = 2000)
        curve(prop.curv.func, from = 0, to = 1, n = 2000)
      }else{
        crit.left <- prop.quant.func(.0005)
        crit.right <- prop.quant.func(.9995)
        curve(prop.dist.func, from = crit.left, to = crit.right, n = 2000)
        curve(prop.dens.func, from = crit.left, to = crit.right, n = 2000)
        curve(prop.curv.func, from = crit.left, to = crit.right, n = 2000)
      }
    }
    
    return(x.list)
    
  }else if(method == "mid"){
    
    mid.dist.func <- function(p) pbinom(q = x, size = n, prob = p, lower.tail = FALSE) + 1/2*dbinom(x = x, size = n, prob = p)
    
    mid.curv.func <- function(p) abs(2*mid.dist.func(p)-1)
    
    pbin.deriv.beta <- function(p) dbeta(x = p, shape1 = x+1, shape2 = n-x)
    
    dbin.deriv <- function(p) choose(n = n, k = x)*(-(n - x)*p^x*(-p + 1)^(n - x - 1) + p^(x - 1)*(-p + 1)^(n - x)*x)
    
    mid.dens.func <- function(p) pbin.deriv.beta(p) + 1/2*dbin.deriv(p)
    
    mid.quant.func <- function(q)
      if(x == 0){
        if(q>=.5 & q<=1){
          uniroot(function(x) mid.dist.func(x)-q, lower = 0, upper = 1)$root
        }else if (q>=0 & q<.5){
          0
        }else{
          stop("Quantile not between 0 and 1")
        }
      }else if(x == n){
        if(q>=0 & q<.5){
          uniroot(function(x) mid.dist.func(x)-q, lower = 0, upper = 1)$root
        }else if (q>=.5 & q<=1){
          1
        }else{
          stop("Quantile not between 0 and 1")
        }
      }else{
        if(q>=0 & q<=1){
          uniroot(function(x) mid.dist.func(x)-q, lower = 0, upper = 1)$root
        }else{
          stop("Quantile not between 0 and 1")
        }
      }
    mid.quant.func <- Vectorize(mid.quant.func)
    
    x.list <- list(pconf = mid.dist.func, dconf = mid.dens.func, cconf = mid.curv.func, qconf = mid.quant.func)
    
    if (plot){
      if(fullrange == "full"){ 
        curve(mid.dist.func, from = 0, to = 1, n = 2000)
        curve(mid.dens.func, from = 0, to = 1, n = 2000)
        curve(mid.curv.func, from = 0, to = 1, n = 2000)
      }else{
        crit.left <- mid.quant.func(.0005)
        crit.right <- mid.quant.func(.9995)
        curve(mid.dist.func, from = crit.left, to = crit.right, n = 2000)
        curve(mid.dens.func, from = crit.left, to = crit.right, n = 2000)
        curve(mid.curv.func, from = crit.left, to = crit.right, n = 2000)
      }
    }
    return(x.list)
  }
}