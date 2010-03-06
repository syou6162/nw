\name{nw-package}
\alias{nw-package}
\alias{nw}
\docType{package}
\title{
  Nonparametric regression with Nadaraya-Watson estimator
}
\description{
  This package enables you to do the nonparametric regression with
  Nadaraya-Watson estimator especially two dimensions, and to obtain the
  optimal smoothing parameter matrix under the cross-validation criteria.
}
\details{
  \tabular{ll}{
    Package: \tab nw\cr
    Type: \tab Package\cr
    Version: \tab 0.01\cr
    Date: \tab 2009-02-12\cr
    License: \tab What license is it under?\cr
    LazyLoad: \tab yes\cr
  }

}
\author{
  Yasuhisa Yoshida
}
\references{
  Nonparametric and Semiparametric Models
}
\keyword{ package }
\keyword{ nonparametric }
\keyword{ multivariate }
\seealso{
  ~~ Optional links to other man pages, e.g. ~~
  ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
mkr_with_noise<- function(x1,x2){ 
  return(sin(2*pi*x1)+x2+rnorm(1,0,1/4)) 
} 

n <- 20
X1 <- seq(0,1,length=n) 
X2 <- seq(0,1,length=n) 
e <- expand.grid(X1,X2) 
Y <- apply(e,1,function(x){ 
  x1 <- x[1] 
  x2 <- x[2] 
  mkr_with_noise(x1,x2) 
}) 
X1 <- e[,1] 
X2 <- e[,2] 

h <- c(0.05,0,0,0.05)
nw <- nw(Y,X1,X2,h)
plot(nw)

h <- c(0.5,0,0,0.5)
nw <- nw(Y,X1,X2,h)
plot(nw)


# set init for optimize
h <- c(0.3,0.1,0.1,0.3)
nw <- nw(Y,X1,X2,h)
choice.optimal.h(nw)
h <- choice.optimal.h(nw)$h
nw <- nw(Y,X1,X2,h)
plot(nw)
}
