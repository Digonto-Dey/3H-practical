Y<-c(67.50,75.44,109.7,105.44,109.4,85.83,76.7,129.42,
     104.24,125.83,153.99,152.92,160.03,176.33,174.53)
X<-c(80.09 , 72.57, 112.14 ,121.57 ,125.60 ,131.48, 131.54,
     145.60 ,168.56 ,171.48, 203.54, 222.85, 230.93, 232.99, 261.18)

ols<-lm(Y~X)
summary(ols)
mse<-summary(ols)$sigma**2
mse

data<-data.frame(Y,X)
data


data1<-data[order(data$X),] 
data1  
x<-data1$X;x
y<-matrix(data1$Y);y


##Wald Method####

z<-array(length(x))
for (i in 1:length(x)) {
  if(x[i]<=median(x))
    z[i]=-1
  else
    z[i]=1
}
x.mat<-cbind(1,x)
x.mat
z.mat<-cbind(1,z)
z.mat

###Estimation of parameters#####

t1<-solve(t(z.mat)%*%x.mat);t1
t2<-t(z.mat)%*%y;t2
b.hat<-t1%*%t2;b.hat  			

a.iv.hat<-b.hat[1];a.iv.hat
b.iv.hat<-b.hat[2];b.iv.hat


#####Variance####

###(part-B)
v1<-solve(t(z.mat)%*%x.mat)
v2<-t(z.mat)%*%(z.mat)
v3<-solve(t(x.mat)%*%z.mat)

var.mat<-mse*(v1%*%v2%*%v3);var.mat   							     
var.a.iv<-var.mat[1,1];var.a.iv   
var.b.iv<-var.mat[2,2];var.b.iv 


#Bartlett method######

n1<-round(length(X)/3);n1
n2<-2*n1+1;n2
x1<-c(x[1:n1],x[n2:length(x)]);x1
y1<-matrix(c(y[1:n1],y[n2:length(x)]));y1


z<-array(length(x1))
for (i in 1:length(x1)) {
  if(x1[i]<=median(x1))
    z[i]=-1
  else
    z[i]=1
}
x.mat<-cbind(1,x1);x.mat
z.mat<-cbind(1,z);z.mat

###Estimation of parameters#####

t1<-solve(t(z.mat)%*%x.mat);t1
t2<-t(z.mat)%*%y1;t2
b.hat<-t1%*%t2;b.hat  			

a.iv.hat<-b.hat[1];a.iv.hat
b.iv.hat<-b.hat[2];b.iv.hat


#####Variance####

v1<-solve(t(z.mat)%*%x.mat)
v2<-t(z.mat)%*%(z.mat)
v3<-solve(t(x.mat)%*%z.mat)

var.mat<-mse*(v1%*%v2%*%v3);var.mat   							     
var.a.iv<-var.mat[1,1];var.a.iv   
var.b.iv<-var.mat[2,2];var.b.iv 

#Durbin method##

z<-rank(x)				
x.mat<-cbind(1,x);x.mat
z.mat<-cbind(1,z);z.mat

###Estimation of parameters#####

t1<-solve(t(z.mat)%*%x.mat);t1
t2<-t(z.mat)%*%y;t2
b.hat<-t1%*%t2;b.hat  			

a.iv.hat<-b.hat[1];a.iv.hat
b.iv.hat<-b.hat[2];b.iv.hat


#####Variance####

v1<-solve(t(z.mat)%*%x.mat)
v2<-t(z.mat)%*%(z.mat)
v3<-solve(t(x.mat)%*%z.mat)

var.mat<-mse*(v1%*%v2%*%v3);var.mat   							     
var.a.iv<-var.mat[1,1];var.a.iv   
var.b.iv<-var.mat[2,2];var.b.iv 


