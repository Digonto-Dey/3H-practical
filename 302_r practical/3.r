library(stats4)
x=c(105,140,20,113,121,10,44,150,60,30,30,11)
n=length(x)
n

neg.ll=function(a,b){
  term1=-n*log(b/a)
  term2=-(b-1)*sum(log(x/a))
  term3=sum((x/a)^b)
  term1+term2+term3
}
coefs=coef(mle(minuslogl = neg.ll,start = list(a=quantile(x,0.632),b=1.5)))
var=vcov(mle(minuslogl = neg.ll,start = list(a=quantile(x,0.632),b=1.5)))
coefs
var
diag(var)
quantile(x,0.632)

###b

###95% CI for scale parameter
### b
cat("95% confidence intervals for a (scale parameter):\n")
cat("Lower:", coefs[1] - qnorm(0.975) * sqrt(var[1, 1]), "\n")
cat("Upper:", coefs[1] + qnorm(0.975) * sqrt(var[1, 1]), "\n")

cat("95% confidence intervals for b (shape parameter):\n")
cat("Lower:", coefs[2] - qnorm(0.975) * sqrt(var[2, 2]), "\n")
cat("Upper:", coefs[2] + qnorm(0.975) * sqrt(var[2, 2]), "\n")

##c
##estimated median
qweibull(0.5,coefs[2],coefs[1])    # remember qweibull(p,shape=b,scale=a)
##estimated 63.2 percentile
qweibull(0.632,coefs[2],coefs[1])
##estimated 75 percentile
qweibull(0.75,coefs[2],coefs[1])