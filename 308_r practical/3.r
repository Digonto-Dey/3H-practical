### a
set.seed(123)  # For reproducibility
x <- rpois(1000, 5)
hist(x, main = "Histogram of Poisson(5)", xlab = "Value", ylab = "Frequency", col = "lightgreen", border = "black")
boxplot(x, main = "Boxplot of Poisson(5)", ylab = "Value", col = "lightgreen")
summary(x)
mean(x)
var(x)

### b
set.seed(123)  # For reproducibility
y <- rexp(1000, 1/5)
hist(y, main = "Histogram of Exponential(5)", xlab = "Value", ylab = "Frequency", col = "hotpink", border = "black")
boxplot(y, main = "Boxplot of Exponential(5)", ylab = "Value", col = "hotpink")
summary(y)
mean(y)
var(y)