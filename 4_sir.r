y<-c( 16.68, 11.50, 12.03, 14.88, 13.75, 18.11,  8.00, 17.83, 79.24, 21.50, 40.33, 21.00, 13.50, 
      19.75, 24.00 ,29.00, 15.35, 19.00,  9.50, 35.10, 17.90, 52.32, 18.75 ,19.83, 10.75) 
x<-c(560,  220,  340,   80,  150,  330,  110 , 210, 1460,  605,  688,  215,  255 , 462,  448,  776,  
     200,  132 ,  36,  770,  140,  810 , 450,  635,  150) 
##(i) 
model<-lm(y~x);model 
##(ii) 
plot(x,y) 
abline(model) 
#######(iii) 
###High leverage value 
d=(x-mean(x))^2/(sum((x-mean(x))^2));d 
n=length(x);n 
hii=(1/n)+d 
hii 
###### 
h<-hat(x);h 
###Cut-off Point##### 
p=2 
CP=2*(p/n);CP ### Twice the mean rule 
############Identification########### 
CP>hii 
which(hii>CP) 
#####outlier 
r<-model$residuals;r 
msr<-sum(r^2)/(n-p);msr 
ar<-abs(r);ar 
di<-ar/sqrt(msr);di  ##Standardized residuals 
out<-di[di>3];out 
#or 
ri<-ar/sqrt(msr*(1-h));ri  ##Studentized residuals 
out<-ri[ri>3];out

#### Influential Observation##### 
cdi<-(h*ri^2)/(p*(1-h));cdi 
IO<-cdi[cdi>1];IO 
### 
cooks.distance(model) 
#### 
y.hat=4.96116+.04257*x 
y.hat 
ei=y-y.hat;ei 
sl.<-c(1:25) 
data2=data.frame(sl.,y,x,y.hat,ei,hii,di,ri,cdi);data2 
View(data2)


#iv

y<-c( 16.68, 11.50, 12.03, 14.88, 13.75, 18.11,  8.00, 17.83, 79.24, 21.50, 40.33, 21.00, 13.50, 
      19.75, 24.00 ,29.00, 15.35, 19.00,  9.50, 35.10, 17.90, 52.32, 18.75 ,19.83, 10.75) 
x<-c(560,  220,  340,   80,  150,  330,  110 , 210, 1460,  605,  688,  215,  255 , 462,  448,  776,  
     200,  132 ,  36,  770,  140,  810 , 450,  635,  150) 
##(i) 
y[9] = 65.24
y
model<-lm(y~x);model 

plot(x,y) 
abline(model) 


#v
##(i) 
y[9] = 65.24
x[22] = 35.32
x
y
model<-lm(y~x);model 

plot(x,y) 
abline(model) 
