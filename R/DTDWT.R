library(waveslim)
library(zoo)


Faf <- waveslim::FSfarras()$af
Fsf <- waveslim::FSfarras()$sf
af <- waveslim::dualfilt1()$af
sf <- waveslim::dualfilt1()$sf


DTDWT <- function(x,J)
{
  return(waveslim::dualtree(x,J,Faf,af))
}


DTDWT.inverse <- function(wc)
{
  J <- length(wc) - 1
  return(waveslim::idualtree(wc,J,Fsf,sf))
}


suppress_coefficients <- function(wc,predicate)
{
  JJ <- length(wc)
  for (j in 1:JJ) {
    for (c in 1:2) {
      for (t in 1:length(wc[[j]][[c]]))
        if(predicate(j,c,t,wc[[j]][[c]][[t]])) {
          wc[[j]][[c]][[t]] <- 0
        }
    }
  }
  return(wc)  
}

quantile3rd_thresh <- function(wc) {
  return( cbind(sapply(1:length(wc),function(j) { stats::quantile(abs(wc[[j]][[1]]))[[4]]}),sapply(1:length(wc),function(j) { stats::quantile(abs(wc[[j]][[2]]))[[4]]})) )
}

quantiler <- function(wc) { 
  thresholds <- quantile3rd_thresh(wc)
  return( function(j,c,t,x) {abs(x) < thresholds[j,c]} )
}

DTDWT.power <- function(wc)
{
  tmp <- lapply(wc,function(lev) 
  {
    sqrt(lev[[1]]^2 + lev[[2]]^2)
  })
  return(tmp[1:(length(wc)-1)])
}
  

DTDWT.extract <- function(wc,levels,draw=FALSE)
{
  wc1 <- suppress_coefficients(wc,function(j,c,t,x) { !(j %in% levels) })
  x1 <- DTDWT.inverse(wc1)
  if(draw)
    plot(x1,type="l")
  
  invisible(x1)
}


DTDWT.mute <- function(wc,levels,draw=FALSE)
{
  wc1 <- suppress_coefficients(wc,function(j,c,t,x) { j %in% levels })
  x1 <- DTDWT.inverse(wc1)
  if(draw)
    plot(x1,type="l")
  
  invisible(x1)
}


# DTDWT.mute <- function(wc,levels)
# {
#   for (index in levels)
#   {
#     lev <- wc[[index]]
#     lev1 <- lev[[1]]
#     lev2 <- lev[[2]]
#     lev1[TRUE] <- 0
#     lev2[TRUE] <- 0
#   }
#   J <- length(wc) - 1
#   return(DTDWT.inverse(wc))
# }

# lev <- lapply(1:length(ps),function(j) { cbind((1:length(ps[[j]]))*2^(j-1),ps[[j]]) })  
  
  


# v1 <- lapply(w1,function(a) lapply(a, function(b) sapply(b, function(c) {if (abs(c) < 0.5) return(0.0) else return(c)})))