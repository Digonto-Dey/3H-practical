analyze_2 <- function(y, x, title) {
  model <- lm(y ~ x)
  plot(x, y, pch=19, col="blue", main=title)
  abline(model, col="red", lwd=2)
  
  # --- Define variables first ---
  n = length(x)
  p = 2  # Moved this line up! It defines the number of parameters (intercept + slope).
  
  # High leverage point
  d = (x - mean(x))^2 / sum((x - mean(x))^2)
  h = (1/n) + d
  
  # Outlier
  rs = model$residuals
  msr = sum(rs^2) / (n-p) # Now this line will work correctly
  di = abs(rs) / sqrt(msr) # Standardized residuals
  ri = abs(rs) / sqrt(msr * (1 - h)) # studentized residuals
  
  # Influntial Observation
  cook_dist = (h * ri^2) / (p*(1-h))
  
  # Cut of point for leverage
  cut = 2*(p/n)  # Twice of the mean rule
  
  
  cat("\n----", title, "----\n")
  cat("Model")
  print(model)
  cat("High leverage points:", which(h > cut), "\n")
  cat("Outliers (|r|>3):", which(ri > 3), "\n")
  cat("Outliers (|d|>3):", which(di > 3), "\n")
  cat("Influential (CookD>1):", which(cook_dist > 1), "\n")
  cat("R-squared:", summary(model)$r.squared, "\n")
}

# Now, running the function will work without error
y <- c(16.68,11.50,12.03,14.88,13.75,18.11,8.00,17.83,79.24,21.50,40.33,21.00,13.50,
       19.75,24.00,29.00,15.35,19.00,9.50,35.10,17.90,52.32,18.75,19.83,10.75)
x <- c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,
       200,132,36,770,140,810,450,635,150)

analyze_2(y, x, "Original Data")


# 2️⃣ Change 9th Y
y2 <- y; y2[9] <- 65.24
analyze_2(y2, x, "After changing 9th Y = 65.24")

# 3️⃣ Change 9th Y, 22nd Y, and 22nd X
y3 <- y; x3 <- x
y3[c(9,22)] <- c(65.24, 35.32)
x3[22] <- 610
analyze_2(y3, x3, "After changing 9th Y, 22nd Y, and 22nd X")
