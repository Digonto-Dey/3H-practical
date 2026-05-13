library(stats4)

x <- c(105, 140, 20, 113, 121, 10, 44, 150, 60, 30, 30, 11)
# x <- rgamma(100, shape = 2, scale = 50)  # Example data
mean(x)
n <- length(x)
logL <- function(a, b) {
  # if (a <= 0 || b <= 0) return(Inf)  # ensure valid values
  
  term1 <- n * lgamma(a)             # n * log Gamma(a)
  term2 <- n * a * log(b)            # n * a * log(b)
  term3 <- - (a - 1) * sum(log(x))   # - (a-1) * sum(log(x))
  term4 <- sum(x)/b                  # sum(x) / b
  
  term1 + term2 + term3 + term4      # return negative log-likelihood
}

# Start values (use method of moments as a better guess)
fit <- mle(logL,
           start = list(a=quantile(x,.632),b=1.5),
           method = "L-BFGS-B",
           lower = c(0.01, 0.01))  # Ensure positive a and b

# Extract MLE and variance-covariance matrix
coefs <- coef(fit)
var <- vcov(fit)


cat("MLE estimates: ", coefs, "\n")

cat("Variance are", diag(var),  "\n")

### b
cat("95% confidence intervals for a (shape parameter):\n")
cat("Lower:", coefs[1] - qnorm(0.975) * sqrt(var[1, 1]), "\n")
cat("Upper:", coefs[1] + qnorm(0.975) * sqrt(var[1, 1]), "\n")

cat("95% confidence intervals for b (scale parameter):\n")
cat("Lower:", coefs[2] - qnorm(0.975) * sqrt(var[2, 2]), "\n")
cat("Upper:", coefs[2] + qnorm(0.975) * sqrt(var[2, 2]), "\n")

percentile = function(p) {
  qgamma(p, shape = coefs[1], scale = coefs[2])
}

### c
cat("Estimated median:", percentile(0.5), "\n")
cat("Estimated 63.2th percentile:", percentile(0.632), "\n")
cat("Estimated 75th percentile:", percentile(.75), "\n")
