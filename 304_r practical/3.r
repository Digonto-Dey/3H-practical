# Data
Y <- c(67.50,75.44,109.7,105.44,109.4,85.83,76.7,129.42,104.24,125.83,153.99,152.92,160.03,176.33,174.53)
X <- c(80.09,72.57,112.14,121.57,125.60,131.48,131.54,145.60,168.56,171.48,203.54,222.85,230.93,232.99,261.18)

# OLS and MSE
ols <- lm(Y ~ X)
summary(ols)
mse <- summary(ols)$sigma^2
mse

# Sort data 
data <- data.frame(Y, X)[order(X),]
x <- data$X
y <- data$Y

# Function for IV estimation
iv_est <- function(x, y, z) {
  x.mat <- cbind(1, x)
  z.mat <- cbind(1, z)
  b.hat <- solve(t(z.mat) %*% x.mat) %*% (t(z.mat) %*% y)
  var.mat <- mse * (solve(t(z.mat) %*% x.mat) %*% (t(z.mat) %*% z.mat) %*% solve(t(x.mat) %*% z.mat))
  data.frame(a = b.hat[1], b = b.hat[2], var.a = var.mat[1,1], var.b = var.mat[2,2])
}

### Wald Method ###
z1 <- ifelse(x <= median(x), -1, 1)
wald <- iv_est(x, y, z1)

### Bartlett Method ###
n1 <- round(length(x)/3)
n2 <- 2*n1 + 1
x1 <- c(x[1:n1], x[n2:length(x)])
y1 <- c(y[1:n1], y[n2:length(x)])
z2 <- ifelse(x1 <= median(x1), -1, 1)
bartlett <- iv_est(x1, y1, z2)

### Durbin Method ###
z3 <- 1:length(x)
durbin <- iv_est(x, y, z3)

# Show results
wald
bartlett
durbin
