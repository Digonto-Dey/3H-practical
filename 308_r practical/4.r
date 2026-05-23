### a
library(MASS)
set.seed(123)  # For reproducibility
n <- 1000

# a) MLEs for Normal Distribution
nor_data <- rnorm(n, mean = 10, sd = 2)
mle_nor <- fitdistr(nor_data, "normal")
print("Normal")
mle_nor$estimate

# b) MLEs for Weibull Distribution
weib_data <- rweibull(n, shape = 2.1, scale = 1.1)
mle_weib <- fitdistr(weib_data, "weibull")
print("Weibull")
mle_weib$estimate

# c) MLEs for Gamma Distribution
gamma_data <- rgamma(n, shape = 3.5, rate = 0.5)
mle_gamma <- fitdistr(gamma_data, "gamma")
print("Gamma")
mle_gamma$estimate