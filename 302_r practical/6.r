x=c(2.1,5.2,2.3,1.4,2.2,2.3,1.6)
n=length(x)
n
theta1=median(x)
d=(median(abs(x-theta1)))/0.6745
k=2

h=(x-theta1)/d
psi=ifelse(h < -k,-k,ifelse(h > k,k,h))
psi_prime=ifelse(h < -k,0,ifelse(h > k, 0,1))
theta2=theta1 + (d*sum(psi))/sum(psi_prime)
theta2

##for 2 Step
h=(x-theta2)/d
psi=ifelse(h < -k,-k,ifelse(h > k,k,h))
psi_prime=ifelse(h < -k,0,ifelse(h > k, 0,1))
theta3=theta2 + (d*sum(psi))/sum(psi_prime)
theta3
##for 3 Step
h=(x-theta3)/d
psi=ifelse(h < -k,-k,ifelse(h > k,k,h))
psi_prime=ifelse(h < -k,0,ifelse(h > k, 0,1))
theta4=theta3 + (d*sum(psi))/sum(psi_prime)
theta4
##robust Var
var_theta=(d^2 * sum((psi)^2))/(sum(psi_prime))^2
var_theta
##normal var
var(x)/n



Comment: From the R-code we obtain the robust location estimate  
θ 
1
​

^
  ​
and  
θ 
2
​

^
  ​
. Both the estimates give similar values i.e 2.150871 and robust variance estimates of  
θ
^
  is 0.01957785. Variance of the sample mean is 0.006911429. This interpretation demonstrates that M-estimates successfully downweighted the outlier and produced a stable, low variance estimate of location than the classical estimate. Huber estimator balances the robustness and efficiency and provides a more reliable central tendency than the mean.

