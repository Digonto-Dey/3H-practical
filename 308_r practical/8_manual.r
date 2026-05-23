# --------------------------------------
# 1. Generate synthetic swiss-like data
# --------------------------------------
set.seed(123)  # For reproducibility
n <- 1000
sigma <- 1
b <- c(67, -0.17, -0.26, -0.87, 0.10, 1.08)  # Coefficients

# Generate predictors (independent variables)
Agriculture <- runif(n, min = 1, max = 100)
Examination <- runif(n, min = 1, max = 40)
Education <- runif(n, min = 1, max = 60)
Catholic <- runif(n, min = 1, max = 100)
Infant.Mortality <- runif(n, min = 1, max = 30)

# Error term
e <- rnorm(n, mean = 0, sd = sigma)

# Response variable according to linear model
Fertility <- b[1] + b[2]*Agriculture + b[3]*Examination + 
  b[4]*Education + b[5]*Catholic + b[6]*Infant.Mortality + e

# Combine into dataframe
swiss_generated <- data.frame(
  Fertility, Agriculture, Examination,
  Education, Catholic, Infant.Mortality
)

head(swiss_generated, 5)

# --------------------------------------
# 2. LOOCV (Leave-One-Out Cross Validation)
# --------------------------------------
n <- nrow(swiss_generated)
errors <- c()

for (i in 1:n) {
  train_data <- swiss_generated[-i, ]
  test_data  <- swiss_generated[i, ]
  
  model <- lm(Fertility ~ ., data = train_data)
  pred <- predict(model, newdata = test_data)
  
  errors[i] <- (test_data$Fertility - pred)^2
}

LOOCV_RMSE <- sqrt(mean(errors))
LOOCV_RMSE

# --------------------------------------
# 3. 10-Fold Cross Validation
# --------------------------------------
set.seed(123)
k <- 10
folds <- sample(rep(1:k, length.out = nrow(swiss_generated)))
errors <- c()

for (i in 1:k) {
  train_data <- swiss_generated[folds != i, ]
  test_data  <- swiss_generated[folds == i, ]
  
  model <- lm(Fertility ~ ., data = train_data)
  pred <- predict(model, newdata = test_data)
  
  errors <- c(errors, (test_data$Fertility - pred)^2)
}

KFold_RMSE <- sqrt(mean(errors))
KFold_RMSE

# --------------------------------------
# 4. Repeated 10-Fold CV (3 repeats)
# --------------------------------------
set.seed(123)
k <- 10
repeats <- 3
RMSE_values <- c()

for (r in 1:repeats) {
  folds <- sample(rep(1:k, length.out = nrow(swiss_generated)))
  errors <- c()
  
  for (i in 1:k) {
    train_data <- swiss_generated[folds != i, ]
    test_data  <- swiss_generated[folds == i, ]
    
    model <- lm(Fertility ~ ., data = train_data)
    pred <- predict(model, newdata = test_data)
    
    errors <- c(errors, (test_data$Fertility - pred)^2)
  }
  
  RMSE_values[r] <- sqrt(mean(errors))
}

Repeated_KFold_RMSE <- mean(RMSE_values)
Repeated_KFold_RMSE

#The model with the lowest RMSE performs best

# We computed LOOCV, 10-fold CV, and Repeated 10-fold CV error estimates.
# Among these, the lowest RMSE was obtained using [METHOD NAME], indicating that this 
# configuration provides the best generalization performance and lowest prediction 
# error on unseen data.
LOOCV may slightly overfit because it trains on almost the entire dataset each time, while Repeated 10-fold CV gives a more stable & averaged estimate, reducing variance from random splits.