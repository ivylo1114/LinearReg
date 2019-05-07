# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}
X<- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,3,4,5,5,6,8,8,9,9,9,10,11,11,12,-1,3,1,4,6,2,5,7,7,8,8,11,12,8,9,12,9,11,13,10),nrow=20,ncol=3)
Y<- matrix(c(4,7,-4,6,13,5,4,10,16,19,19,17,23,24,14,20,19,25,24,23),nrow=20,ncol=1)
n=20
p=2
beta=solve(t(X)%*%X) %*% (t(X)%*%Y)
sigmasq=sum((Y-X%*%beta)^2)/(n-p-1)
covbeta=sigmasq * solve(t(X)%*%X)

standarderror=sqrt(diag(covbeta))
t.stat=beta*(1/standarderror)

pvalue<-2*pt(-abs(t.stat), n-p-1)

summary(lm(Y~X))
