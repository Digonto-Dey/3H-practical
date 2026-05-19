DD <- function(expr, name, order = 1) {
  if(order == 0) return(expr)
  return(DD(D(expr, name), name, order - 1))
}

pgf = expression((q+p*s)^n)

for(n in 1:6) {
  cat("====n =", n, "=====\n")
  p_vals <- c()
  for(i in 0:n) {
    d <- DD(pgf, "s", i)
    value = eval(d, list(p = 0.5, q = 1 - 0.5, s = 0))
    p_vals[i + 1] <- value / factorial(i)
  }

  
  print(data.frame(i = 0:n, p = p_vals, ))
}

