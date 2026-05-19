# Data
years <- 1976:1980
fires <- c(16694, 12271, 12904, 14036, 13874)

# (i) Scatter plot
plot(years, fires, col="red", pch=19, main="Scatter Plot of Fires vs Years")

# (ii) Least Squares (LS) regression
ls_model <- lm(fires ~ years)
abline(ls_model, col="blue", lwd=2)
summary(ls_model)

# (iii) Prepare combinations of 3 points
comb <- combn(1:5, 3)
e <- c()

# (iv) Median of squared errors for each combination
for (i in 1:ncol(comb)) {
  idx <- comb[, i]
  x <- years[idx]
  y <- fires[idx]
  model <- lm(y ~ x)
  pred <- predict(model, data.frame(x = years))
  err <- fires - pred
  e[i] <- median(err^2)
}

# (v) Find smallest median error and best combination
best <- which.min(e)
best_idx <- comb[, best]

cat("\nBest combination (for LMS):", best_idx, "\n")

# (vi) LMS regression using best 3 points
x_best <- years[best_idx]
y_best <- fires[best_idx]
lms_model <- lm(y_best ~ x_best)
summary(lms_model)

# (vii) Plot both lines
abline(lms_model, col="darkgreen", lwd=2, lty=2)
legend("topleft",
       legend=c("Data","LS line","LMS line"),
       col=c("red","blue","darkgreen"),
       pch=c(19, NA, NA),
       lty=c(NA, 1, 2))