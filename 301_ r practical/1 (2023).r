library(mvtnorm)
library(stats4)
n = 1800
mu = c(11, 8, 12, 19, 10)
sigma = matrix(c(6, 4, 3, 2, 1, 4, 5, 4, 3, 2, 3, 4, 7, 4, 3, 2, 3, 4, 9, 4, 1, 2, 3, 4, 4), nrow = 5)

X <- rmvnorm(n, mu, sigma)

neg.ll = function(mean1,mean2,mean3,mean4,mean5, s11,s22,s33,s44,s55,s12,s13,s14,s15,s23,s24,s25,s34,s35,s45) {
  mean = c(mean1,mean2,mean3,mean4,mean5)
  S = matrix(
    c(s11, s12, s13, s14, s15,
      s12, s22, s23, s24, s25,
      s13, s23, s33, s34, s35,
      s14, s24, s34, s44, s45,
      s15, s25, s35, s45, s55
    ), 5, 5)
  log.ll = sum(dmvnorm(X, mean, S, log = TRUE))
  return(-log.ll)
}

xbar = colMeans(X)
S = var(X)


mle1 = mle(minuslogl=neg.ll, start=list(mean1=xbar[1],mean2=xbar[2],mean3=xbar[3],mean4=xbar[4],mean5=xbar[5],
                                        s11=S[1,1],s22=S[2,2],s33=S[3,3],s44=S[4,4],s55=S[5,5],
                                        s12=S[1,2],s13=S[1,3],s14=S[1,4],s15=S[1,5],
                                        s23=S[2,3],s24=S[2,4],s25=S[2,5],
                                        s34=S[3,4],s35=S[3,5],
                                        s45=S[4,5]))
summary(mle1)