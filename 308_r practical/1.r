lcg <- function(n) {
  U <- c()
  m <- 2^32
  a <- 1103515245
  c <- 12345
  
  # Set the seed using the current system time in microseconds
  Z <- as.numeric(Sys.time())*1000
  
  for(i in 1:n) {
    Z <- (a * Z + c) %% m  # Update Z using the LCG formula
    U[i] <- Z / m          # Normalize to get a uniform random number
  }
  return(U)
}

### b
n = c(100, 500, 1000)
sample_1 = lcg(n[1])
sample_2 = lcg(n[2])
sample_3 = lcg(n[3])

### c
par(mfrow = c(1, 3))
hist(sample_1, col = 2, breaks = 10)
hist(sample_2, col = 3, breaks = 10)
hist(sample_3, col = 4, breaks = 10)

### d
ks.test(sample_1, "punif")
ks.test(sample_2, "punif")
ks.test(sample_3, "punif")






# Comment:
# The histograms for n = 100, n = 500, and n = 1000 show that as the sample size increases, 
# the distribution of the generated numbers becomes more evenly spread across the interval 
# [0,1]. For n = 100, there are visible fluctuations and some bins appear higher or lower, 
# indicating sampling variability. For n = 500, the distribution appears more uniform, 
# and by n = 1000, the histogram is closer to a flat uniform distribution, which is 
# expected from a good random number generator.

# Comment:
#   The Kolmogorov–Smirnov (KS) test compares the empirical distribution of the generated 
# numbers with the theoretical Uniform(0,1) distribution. If the p-value > 0.05, we fail 
# to reject the null hypothesis, meaning there is no statistical evidence against uniformity.
# If the p-value < 0.05, it suggests that the sample significantly deviates from a uniform 
# distribution.
# Therefore, based on the KS test results:
# If p-value is large (p > 0.05) → The LCG-generated numbers appear to follow a 
# uniform distribution.
# If p-value is small (p < 0.05) → There may be deficiencies in the generator, 
# and the numbers might not be perfectly uniform.