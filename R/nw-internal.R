epanechnikov.radial <- function(X1,X2,h11,h12,h21,h22){
  X1 <- X1
  X2 <- X2
  h11 <- h11
  h12 <- h12
  h21 <- h21
  h22 <- h22
  return(function(x1,x2){
    det <- (h11*h22 - h12*h21)
    u1 <- 1/det * (h22 * (x1 - X1) - h12 * (x2 - X2))
    u2 <- 1/det * (-h21 * (x1 - X1) + h11 * (x2 -X2))
    utu <- sqrt(u1^2 + u2^2)
    ((1-utu) * ifelse(utu <= 1,1,0))
  })
}

y.exp <- function(X1,X2,Y,h11,h12,h21,h22){
  X1 <- X1
  X2 <- X2
  Y <- Y
  h11 <- h11
  h12 <- h12
  h21 <- h21
  h22 <- h22
  return(function(x1,x2){
    det <- (h11*h22 - h12*h21)
    u1 <- 1/det * (h22 * (x1 - X1) - h12 * (x2 - X2))
    u2 <- 1/det * (-h21 * (x1 - X1) + h11 * (x2 -X2))
    utu <- sqrt(u1^2 + u2^2)
    ((1-utu) * ifelse(utu <= 1,1,0)) * Y
  })
}

cv <- function(Y,X1,X2,h) {
  .C("cv",
     as.double(Y),
     as.double(X1),
     as.double(X2),
     as.integer(length(Y)),
     as.double(h[1]),
     as.double(h[2]),
     as.double(h[3]),
     as.double(h[4]),
     result=0,
     PACKAGE="nw")$result
}
