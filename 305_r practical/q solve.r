## helper: i-th derivative w.r.t. 'name'
DD <- function(expr, name, order = 1) {
  if (order == 0) return(expr)
  DD(D(expr, name), name, order - 1)
}

## p.g.f. for Binomial(n, p):  G(s) = (q + p*s)^n
pgf <- expression((q + p*s)^n)

## function that returns P(X = 0:n) using G^(i)(0)/i!
binom_probs_via_pgf <- function(n, p) {
  q <- 1 - p
  p_vals <- numeric(n + 1)
  for (i in 0:n) {
    di  <- DD(pgf, "s", i)
    val <- eval(di, envir = list(p = p, q = q, s = 0, n = n))
    p_vals[i + 1] <- as.numeric(val) / factorial(i)
  }
  data.frame(i = 0:n, p = p_vals)
}

## (i) n = 12, p = q = 1/2
res_i <- binom_probs_via_pgf(n = 12, p = 0.5)
res_i
sum(res_i$p)              # = 1

## (ii) n = 12, p = 2/3, q = 1/3
res_ii <- binom_probs_via_pgf(n = 12, p = 2/3)
res_ii
sum(res_ii$p)             # = 1


#(b)
barplot(res_i$p, names.arg = res_i$i, main = "Binomial(12, 0.5)", xlab = "i", ylab = "P(X=i)")
barplot(res_ii$p, names.arg = res_ii$i, main = "Binomial(12, 2/3)", xlab = "i", ylab = "P(X=i)")
##Comment (brief): Larger 𝑝 shifts mass to the right (higher counts) and makes 𝐺(𝑠) grow faster over s∈[0,1].

#(c)
curve(( (1-0.5) + 0.5*x )^12, from = 0, to = 1, n = 400,
      xlab = "s", ylab = "G(s)", main = "PGF for Binomial(n=12, different p)")
curve(( (1-2/3) + (2/3)*x )^12, from = 0, to = 1, n = 400, add = TRUE)
legend("topleft", legend = c("p = 0.5", "p = 2/3"), lty = c(1,1), bty = "n")
##Comment (brief): As λ increases, the PGF becomes steeper on [0,1], reflecting a distribution with higher mean/variance and mass moving to larger counts.
