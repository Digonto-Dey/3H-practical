##a no of poisson
lambda = 3
t = 3
lt = lambda * t
# Poisson process follows poisson dist with parameter lt
# dpois for pdf, pois for cdf
cat("Exactly 4:", dpois(4, lt), "\n")
cat("Higher than 4:", 1 - ppois(4, lt), "\n")
cat("Less than 4:", ppois(3, lt), "\n")


set.seed(22)
N <- 100
n <- 12
sample <- rbinom(N, n, prob = 0.5)

# Frequency table
freq <- table(sample)

# Proper barplot
barplot(freq,
        main = "Binomial Distribution (n=12, p=0.5)",
        xlab = "Number of Successes",
        ylab = "Frequency",
        col = "skyblue")
# histogram
hist(sample,
     main = "Binomial Distribution (n=12, p=0.5)",
     xlab = "Number of Successes",
     ylab = "Frequency",
     col = "skyblue")