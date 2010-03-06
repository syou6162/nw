nw <- function(Y,X1,X2,h){
  result <- list(Y = Y, X1 = X1, X2 = X2, h = h)
  class(result) <- "nw"
  return(result)
}

choice.optimal.h <- function(x){
  result <- optim(x$h,function(h){
    h11 <- h[1] 
    h12 <- h[2] 
    h21 <- h[3] 
    h22 <- h[4]
    cv(x$Y,x$X1,x$X2,c(h11,h12,h21,h22))
  })
  return(list(h=result$par,convergence=result$convergence))
}

plot.nw <- function(x,...){
  Y <- x$Y
  X1 <- x$X1
  X2 <- x$X2
  h <- x$h
  n <- 30
  s1 <- seq(min(X1),max(X1),length=n)
  s2 <- seq(min(X2),max(X2),length=n)
  h11 <- h[1];h12 <- h[2];h21 <- h[3];h22 <- h[4]
  persp(s1,s2,
        Reduce("+",Map(function(f){
          outer(s1,s2,f)
        },mapply(function(X1,X2,Y){y.exp(X1,X2,Y,h11,h12,h21,h22)},X1,X2,Y))) /
        Reduce("+",Map(function(f){
          outer(s1,s2,f)
        },mapply(function(X1,X2){epanechnikov.radial(X1,X2,h11,h12,h21,h22)},X1,X2))),
        theta=320,phi=20,col=rainbow(50),ticktype="detailed",
        xlab="x1",ylab="x2",zlab="y",...)
}


