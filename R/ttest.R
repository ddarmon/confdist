#' Confidence functions for t-Test
#'
#' Computes confidence functions for one and two sample t-tests on vectors of data.
#' 
#' @param x a (non-empty) numeric vector of data values.
#' @param y an optional (non-empty) numeric vector of data values.
#' @param paired a logical indicating whether you want a paired t-test.
#' @param plot a logical indicating whether you want the plots to be generated.
#' @return A list containing the confidence functions.
#' @examples
#' t.confdist(1:10, y = c(7:20))


t.confdist <- function(x, y = NULL, paired = FALSE, plot = TRUE) {
  
  if(!is.null(y)){
    if(paired)
      xok <- yok <- complete.cases(x,y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    y <- y[yok]
  } else {
    if (paired) stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]
  if (paired) {
    x <- x-y
    y <- NULL
  }
  
  xbar <- mean(x)
  sx <- sd(x)
  nx <- length(x)
  vx <- var(x)
  
  if(is.null(y)){
    # Confidence Distribution
    
    x1.dist.func <- function(mu) 1-pt((xbar - mu)/(sx/sqrt(nx)), df = (nx-1))
    
    # Confidence Density
    
    x1.dens.func <- function(mu) (1/(sx/sqrt(nx)))*(dt((xbar-mu)/((sx/sqrt(nx))), df=(nx-1)))
    
    # Confidence Curve
    
    x1.conf.curv.func <- function(mu) abs(2*(1-pt((xbar - mu)/(sx/sqrt(nx)), df = (nx-1)))-1)
    
    # Confidence Quantile
    
    x1.quantile.func <- function(p) xbar + (sx/sqrt(nx))*qt(p, df = (nx-1))
    
    x.list <- list(pconf = x1.dist.func, dconf = x1.dens.func, cconf = x1.conf.curv.func, qconf = x1.quantile.func)
    
    
    if(plot == TRUE){
      
      t.crit <- qt(0.9995, df = nx-1)
      curve(x1.dist.func(mu), from = xbar - t.crit*(sx/sqrt(nx)), to = xbar + t.crit*(sx/sqrt(nx)), xname = "mu", main = "Confidence Distribution")
      curve(x1.dens.func(mu), from = xbar - t.crit*(sx/sqrt(nx)), to = xbar + t.crit*(sx/sqrt(nx)), xname = "mu", main = "Confidence Density")
      curve(x1.conf.curv.func, from = xbar - t.crit*(sx/sqrt(nx)), to = xbar + t.crit*(sx/sqrt(nx)), xname = "mu", main = "Confidence Curve")
      
    }
    
    return(x.list)
    
  } else if (!is.null(y)){
    
    ybar <- mean(y)
    sy <- sd(y)
    ny <- length(y)
    vy <- var(y)
    stderrx <- sqrt(vx/nx)
    stderry <- sqrt(vy/ny)
    stderr <- sqrt(stderrx^2 + stderry^2)
    df.welch <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
    
    # Confidence Distribution
    welch.dist.func <- function(delta) 1-pt((xbar - ybar - delta)/(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), df = df.welch)
    
    # Confidence Density
    welch.dens.func <- function(delta) (1/sqrt((sx^2/(nx))+(sy^2/(ny)))*(dt((xbar - ybar- delta)/sqrt((sx^2/(nx))+(sy^2/(ny))), df=df.welch)))
    
    # Confidence Curve
    welch.conf.curv.func <- function(delta) abs(2*(1-pt((xbar - ybar - delta)/(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), df = df.welch))-1)
    
    # Confidence Quantile
    welch.quantile.func <- function(p) (xbar - ybar) + sqrt((sx^2/(nx))+(sy^2/(ny)))*qt(p, df = df.welch)
    
    welch.list <- list(pconf = welch.dist.func, dconf = welch.dens.func, cconf =  welch.conf.curv.func, qconf = welch.quantile.func)
    
    if(plot == TRUE){
      
      t.crit <- qt(0.9995, df = nx-1)
      
      curve(welch.dist.func, from = (xbar-ybar) - t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), to = (xbar-ybar) + t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), xname = "delta", main = "Confidence Distribution")
      curve(welch.dens.func, from = (xbar-ybar) - t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), to = (xbar-ybar) + t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), xname = "delta", main = "Confidence Density")
      curve(welch.conf.curv.func, from = (xbar-ybar) - t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), to = (xbar-ybar) + t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), xname = "delta", main = "Confidence Curve", ylab = "Level of Confidence")
      
    }
    return(welch.list)
    
  }
}

#' Confidence functions for t-Test with summary data
#'
#' Computes confidence functions for one and two sample t-tests on summary data.
#' 
#' @param mean a (non-empty) numeric vector of data values.
#' @param sd a (non-empty) numeric vector of data values.
#' @param n a (non-empty) numeric vector of data values.
#' @param plot a logical indicating whether you want the plots to be generated.
#' @return A list containing the confidence functions.
#' @examples
#' t.confdist(c(3,5), c(1,.5), c(10, 12))

t.confdist.summary <- function(mean,sd,n, plot = TRUE){
  
  stopifnot(length(mean)==length(sd) && length(mean)==length(n))
  
  if (length(mean)==1){
    xbar <- mean[1]
    sx <- sd[1]
    nx <- n[1]
    vx <- sx^2
    
    # Confidence Distribution
    
    x1.dist.func <- function(mu) 1-pt((xbar - mu)/(sx/sqrt(nx)), df = (nx-1))
    
    # Confidence Density
    
    x1.dens.func <- function(mu) (1/(sx/sqrt(nx)))*(dt((xbar-mu)/((sx/sqrt(nx))), df=(nx-1)))
    
    # Confidence Curve
    
    x1.conf.curv.func <- function(mu) abs(2*(1-pt((xbar - mu)/(sx/sqrt(nx)), df = (nx-1)))-1)
    
    # Confidence Quantile
    
    x1.quantile.func <- function(p) xbar + (sx/sqrt(nx))*qt(p, df = (nx-1))
    
    x.list <- list(pconf = x1.dist.func, dconf = x1.dens.func, cconf = x1.conf.curv.func, qconf = x1.quantile.func)
    
    if(plot == TRUE){
      
      t.crit <- qt(0.9995, df = nx-1)
      curve(x1.dist.func(mu), from = xbar - t.crit*(sx/sqrt(nx)), to = xbar + t.crit*(sx/sqrt(nx)), xname = "mu", main = "Confidence Distribution")
      curve(x1.dens.func(mu), from = xbar - t.crit*(sx/sqrt(nx)), to = xbar + t.crit*(sx/sqrt(nx)), xname = "mu", main = "Confidence Density")
      curve(x1.conf.curv.func, from = xbar - t.crit*(sx/sqrt(nx)), to = xbar + t.crit*(sx/sqrt(nx)), xname = "mu", main = "Confidence Curve")
      
    }
    
    return(x.list)
    
  } else if (length(mean)==2){
    xbar <- mean[1]
    sx <- sd[1]
    nx <- n[1]
    vx <- sx^2
    ybar <- mean[2]
    sy <- sd[2]
    ny <- n[2]
    vy <- sy^2
    stderrx <- sqrt(vx/nx)
    stderry <- sqrt(vy/ny)
    stderr <- sqrt(stderrx^2 + stderry^2)
    df.welch <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
    
    # Confidence Distribution
    welch.dist.func <- function(delta) 1-pt((xbar - ybar - delta)/(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), df = df.welch)
    
    # Confidence Density
    welch.dens.func <- function(delta) (1/sqrt((sx^2/(nx))+(sy^2/(ny)))*(dt((xbar - ybar- delta)/sqrt((sx^2/(nx))+(sy^2/(ny))), df=df.welch)))
    
    # Confidence Curve
    welch.conf.curv.func <- function(delta) abs(2*(1-pt((xbar - ybar - delta)/(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), df = df.welch))-1)
    
    # Confidence Quantile
    welch.quantile.func <- function(p) (xbar - ybar) + sqrt((sx^2/(nx))+(sy^2/(ny)))*qt(p, df = df.welch)
    
    welch.list <- list(pconf = welch.dist.func, dconf = welch.dens.func, cconf =  welch.conf.curv.func, qconf = welch.quantile.func)
    
    if(plot == TRUE){
      
      t.crit <- qt(0.9995, df = df.welch)
      
      curve(welch.dist.func, from = (xbar-ybar) - t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), to = (xbar-ybar) + t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), xname = "delta", main = "Confidence Distribution")
      curve(welch.dens.func, from = (xbar-ybar) - t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), to = (xbar-ybar) + t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), xname = "delta", main = "Confidence Density")
      curve(welch.conf.curv.func, from = (xbar-ybar) - t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), to = (xbar-ybar) + t.crit*(sqrt((((sx)^2)/nx)+((sy)^2)/ny)), xname = "delta", main = "Confidence Curve", ylab = "Level of Confidence")
    }
    return(welch.list)
    
  }else{
    stop("vector lengths must be equal to or less than 2")
  }
  
}