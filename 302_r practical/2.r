ni <- c(4, 10, 24, 12, 6, 3)
n <- sum(ni)


chisquare <- function(a) {
  sapply(a, function(single_a) {
    lower <- c(0, 1, 2, 3, 4, 5)
    upper <- c(1, 2, 3, 4, 5, Inf)
    p <- exp(-lower*single_a) - exp(-upper*single_a)
    
    sum((ni - n * p)^2 / (n * p))
  })
}

est <- optimize(chisquare, c(0, 10))

# Print results
cat("Estimated theta:", est$minimum, "\n")
cat("Minimum chi-square value:", est$objective, "\n")

# Plotting the chi-square function
curve(chisquare, from = 0, to = 1, col = "blue", lwd = 2,
      xlab = expression(theta), ylab = "Chi^2 (theta)",
      ylim = c(20, 70))
abline(h = est$objective, v = est$minimum, lwd=2, lty = c(2,3), col = c(2,3))