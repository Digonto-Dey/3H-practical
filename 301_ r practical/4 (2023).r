library(Hotelling)

x1=c(57,82,56,77,68,64,58,61,89,40,71,75,86,48)
x2=c(102,131,97,126,147,99,123,110,129,106,98,138,119,107)
x3=c(173,245,213,163,220,138,159,176,212,200,243,182,246,216)
y1=c(51,20,71,17,34,15,45,61,52,23,39,66,74,54)
y2=c(93,118,88,103,96,80,98,130,77,112,84,91,121,110)
y3=c(197,213,131,174,150,178,166,125,181,177,142,191,183,203)

x = data.frame(x1, x2, x3)
y = data.frame(y1, y2, y3)

n1 = nrow(x)
n2 = nrow(y)

mean.x = colMeans(x)
mean.y = colMeans(y)

sigma.x = var(x)
sigma.y = var(y)

x = list(mean = mean.x, cov = sigma.x, n =n1)
y = list(mean = mean.y, cov = sigma.y, n =n2)

fit = hotelling.test(x,y,var.equal = F)
fit

