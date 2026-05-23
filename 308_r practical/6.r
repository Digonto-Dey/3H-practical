### a
mu=2
sigma=2
n=10
nsim=10

xbar = c()
xvar = c()
set.seed(123)

for(i in 1:nsim){
  x <- rnorm(n,mu,sigma)
  xbar[i] <- mean(x)
  xvar[i] <- var(x)
  cat(i, "Mean:", xbar[i], "Var:", xvar[i],"\n")
}


### b
sample_mean <- mean(xbar)
cat("Sample Mean:", sample_mean, "\nPopulation Mean:", mu, "\n")


### c
nsim = 1000

for(i in 1:nsim){
  x <- rnorm(n,mu,sigma)
  xbar[i] <- mean(x)
  xvar[i] <- var(x)
}

par(mfrow = c(1, 2))

hist(xbar, breaks = 10, main = "Sample Means (Hist)", xlab = "Sample Mean", col = "lightblue", border = "black")
qqnorm(xbar, main = "QQ-Plot")
qqline(xbar, col = "red")




### d
mu <- 2; sigma <- 2
n <- c(10, 100, 1000)
nsim <- 1000

xbar1 <- c()
xbar2 <- c()
xbar3 <- c()
xvar1 <- c()
xvar2 <- c()
xvar3 <- c()
set.seed(123)

for(i in 1:nsim){
  x1<-rnorm(n[1], mu, sigma)
  x2<-rnorm(n[2], mu, sigma)
  x3<-rnorm(n[3], mu, sigma)
  
  xbar1[i]<-mean(x1)
  xbar2[i]<-mean(x2)
  xbar3[i]<-mean(x3)
  
  xvar1[i]<-var(x1)
  xvar2[i]<-var(x2)
  xvar3[i]<-var(x3)
}

mean = c(mean(xbar1), mean(xbar2), mean(xbar3))
var  = c(var(xbar1), var(xbar2), var(xbar3))

data.frame(n = n, xbar = mean, variance = var)


### e (part 1)
par(mfrow = c(1, 3))
hist(xbar1, breaks = 10, main = "n=10" , xlab = "Sample Mean", col = "lightblue", border = "black")
hist(xbar2, breaks = 10, main = "n=100", xlab = "Sample Mean", col = "lightgreen", border = "black")
hist(xbar3, breaks = 10, main = "n=1000", xlab = "Sample Mean", col = "lightcoral", border = "black")


### e (part - 2)

par(mfrow = c(1, 3))
qqnorm(xbar1, main = "n=10")
qqline(xbar1, col = "red")
qqnorm(xbar2, main = "n=100")
qqline(xbar2, col = "red")
qqnorm(xbar3, main = "n=1000")
qqline(xbar3, col = "red")


### f
mu <- 2; sigma <- 2
n = 100; nsim = 1000
lower_95 <- c()
upper_95 <- c()
lower_99 <- c()
upper_99 <- c()

set.seed(123)
for(i in 1:nsim){
  x <- rnorm(n, mu, sigma)
  xbar <- mean(x)
  se <- sigma / sqrt(n)
  
  # 95% CI
  lower_95[i] <- xbar - qnorm(0.975) * se
  upper_95[i] <- xbar + qnorm(0.975) * se
  
  # 99% CI
  lower_99[i] <- xbar - qnorm(0.995) * se
  upper_99[i] <- xbar + qnorm(0.995) * se
}

# data.frame(simulation = 1:nsim, lower_95, upper_95, lower_99, upper_99)

ave.in.ci95 = mean((lower_95<=2)&(2<=upper_95))
ave.in.ci99 = mean((lower_99<=2)&(2<=upper_99))

cat("Coverage of 95% CI:", ave.in.ci95, "\n")
cat("Coverage of 99% CI:", ave.in.ci99, "\n")


mu <- 2; sigma <- 2
n <- 100; nsim <- 1000
lower_95 <- c()
upper_95 <- c()
lower_99 <- c()
upper_99 <- c()

set.seed(123)
for(i in 1:nsim){
  x <- rnorm(n, mu, sigma)
  xbar <- mean(x)
  S <- sd(x)  # Sample standard deviation
  
  # 95% CI
  lower_95[i] <- xbar - qt(0.975, df = n - 1) * (S / sqrt(n))
  upper_95[i] <- xbar + qt(0.975, df = n - 1) * (S / sqrt(n))
  
  # 99% CI
  lower_99[i] <- xbar - qt(0.995, df = n - 1) * (S / sqrt(n))
  upper_99[i] <- xbar + qt(0.995, df = n - 1) * (S / sqrt(n))
}

# data.frame(simulation = 1:nsim, lower_95, upper_95, lower_99, upper_99)

ave.in.ci95 = mean((lower_95<=2)&(2<=upper_95))
ave.in.ci99 = mean((lower_99<=2)&(2<=upper_99))

cat("Coverage of 95% CI:", ave.in.ci95, "\n")
cat("Coverage of 99% CI:", ave.in.ci99, "\n")

