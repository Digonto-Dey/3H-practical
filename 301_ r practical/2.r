library(mvtnorm)
library(stats4)


n = c(500,1500,6000)
mu = c(5,5)
covmat = matrix(c(8,-3,-3,5), nrow = 2)
set.seed(1200)
mu
covmat

sample = list(
  rmvnorm(n[1], mu, covmat),
  rmvnorm(n[2], mu, covmat),
  rmvnorm(n[3], mu, covmat)
)

#b
neg.ll = function(mean1, mean2, s11, s12, s22){
  mean = c(mean1, mean2)
  S = matrix(c(s11, s12, s12, s22),nrow=2, ncol = 2)
  log.ll = sum(dmvnorm(X, mean, S, log=TRUE))
  return(-log.ll)
}


X = sample[[1]]
xbar = colMeans(X)
S = var(X)

mle1 = mle(minuslogl=neg.ll,start=list(mean1=xbar[1],mean2=xbar[2],
                                       s11=S[1,1],s12=S[1,2],s22=S[2,2]))
mle1


X = sample[[2]]
xbar = colMeans(X)
S = var(X)

mle2 = mle(minuslogl=neg.ll,start=list(mean1=xbar[1],mean2=xbar[2],
                                       s11=S[1,1],s12=S[1,2],s22=S[2,2]))
mle2


X = sample[[3]]
xbar = colMeans(X)
S = var(X)

mle3 = mle(minuslogl=neg.ll,start=list(mean1=xbar[1],mean2=xbar[2],
                                       s11=S[1,1],s12=S[1,2],s22=S[2,2]))
mle3

