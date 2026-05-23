# a
X = seq(-5,10,by=0.1)
pdf.X = dnorm(X,3,2)
#pdf.X

### b
plot(X, pdf.X, type="l", main="PDF of N(3,4)",
     xlab="x", ylab="Density", col="blue", lwd=2)

### c
set.seed(111)
n = 1000
nor.x = rnorm(n,3,2)
#nor.x
summary(nor.x)
sd(nor.x)
hist(nor.x, main="Histogram of N(3,4)", freq = F,
     xlab="x", ylab="Density", col="lightblue", border="black")
lines(X, pdf.X, col="red", lwd=2)

# Adding a legend to the histogram
legend("topright", legend=c("PDF", "Histogram"),
       fill=c("red", "lightblue"))
