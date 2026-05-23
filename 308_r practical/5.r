library(MASS)
### a
invt.Weib <- function(n, beta, eta) {
  U <- runif(n)  # Generate n uniform random numbers
  X <- eta * (-log(1 - U))^(1 / beta)  # Inverse transformation for Weibull distribution
  return(X)
}

### b
set.seed(123)  # For reproducibility
x.weib <- invt.Weib(100, 2.0, 10.0)
mle_weib <- fitdistr(x.weib, "weibull")
mle_weib$estimate

### c
n <- seq(10, 3000, 50)
beta.true <- 2.0
eta.true <- 10.0
beta.hat <- c()
eta.hat <- c()

for(i in 1:length(n)) {
  x.weib <- invt.Weib(n[i], beta.true, eta.true)
  mle_weib <- fitdistr(x.weib, "weibull")
  beta.hat[i] <- mle_weib$estimate[1]
  eta.hat[i] <- mle_weib$estimate[2]
}

plot(n, beta.hat, type = "l", col = "blue", lwd=2,
     xlab = "Sample Size (n)", ylab = "MLE of Beta",
     main = "MLE of Beta vs Sample Size")
abline(h = beta.true, col = "red", lty = 2)
legend("topright", legend = c("MLE of Beta", "True Beta"), col = c("blue", "red"), lty = 1:2, cex = 0.6)

plot(n, eta.hat, type = "l", col = "green", lwd=2,
     xlab = "Sample Size (n)", ylab = "MLE of Eta",
     main = "MLE of Eta vs Sample Size")
abline(h = eta.true, col = "red", lty = 2)
legend("topright", legend = c("MLE of Eta", "True Eta"), col = c("green", "red"), lty = 1:2, cex = 0.6)

