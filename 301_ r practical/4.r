n1 = 30
n2 = 50
mu.x = c(7, 10, 44)
mu.y = c(5, 4, 2)
sigma.x = matrix(c(0.46,1.18,4.49,1.18,7.40,-1.35,4.49,-1.35,4.24), nrow = 3)
sigma.y = matrix(c(0.148,-0.679,0.209,-0.679,4.100,2.200,0.209,2.200,2.1800), nrow = 3)

x = list(mean = mu.x, cov = sigma.x, n = n1)
y = list(mean = mu.y, cov = sigma.y, n = n2)

fit = hotelling.test(x, y, var.equal = F)
fit