library(mvtnorm)
n = 1000
mu = c(11,8,12,19,10)
sigma = matrix(c(6, 4, 3, 2, 1, 4, 5, 4, 3, 2, 3, 4, 7, 4, 3, 2, 3, 4, 9, 4, 1, 2, 3, 4, 4), nrow=5)
set.seed(34)

x <- rmvnorm(n, mu, sigma)
head(x)

colnames(x) <- paste0("x", 1:5)
head(x)

par(mfrow=c(1,1))
boxplot(x, col=2:6, main="boxplot of x")

par(mfrow=c(1,5))
for(i in 1:5){
  hist(x[,i], col=i+1, probability = T, main= paste0("x",i))
  lines(density(x[,i]), lwd=2)
}