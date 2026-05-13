a1=0.025
a2=0.025

x=c(86,146,251,623,98,175,176,76,264,15,157,220,42,321,180,198,38,20,61,121,282,224,189,180,325)
n=length(x)
n
s=sum(x)
s

##pivotal M
theta_lower_quantile=qgamma(a1,shape = n,rate = s)
theta_upper_quantile=qgamma(1-a2,shape = n,rate = s)

cat("95% CI in Pivotal",theta_lower_quantile,theta_upper_quantile,"\n")
cat("range of CI",theta_upper_quantile - theta_lower_quantile,"\n")

p_interval=theta_upper_quantile - theta_lower_quantile
p_interval

##Baysian M
#prior
alpha0=1
beta0=1
#posterior
alpha=alpha0+n
beta=beta0+s

btheta_lower=qgamma(a1,shape = alpha ,rate = beta)
btheta_upper=qgamma(1-a2,shape = alpha,rate = beta)

cat("95% CI in Pivotal",btheta_lower,btheta_upper,"\n")
cat("range of CI",btheta_upper - btheta_lower,"\n")

b_interval=btheta_upper - btheta_lower
b_interval