# Load dataset
data("swiss")  # built-in dataset
head(swiss, 3) # Display 3 sample rows

############################################################
# (a) Train-Test Split Manually (80% Train, 20% Test)
############################################################
set.seed(123)
n <- nrow(swiss)
train_index <- sample(1:n, size = 0.8 * n)
train_data <- swiss[train_index, ]
test_data  <- swiss[-train_index, ]

############################################################
# (b) Fit Linear Model and Compute Metrics (No caret)
############################################################
model <- lm(Fertility ~ ., data = train_data)
predictions <- predict(model, newdata = test_data)

# Compute evaluation metrics manually
R2_value <- 1 - sum((test_data$Fertility - predictions)^2) / sum((test_data$Fertility - mean(test_data$Fertility))^2)
RMSE_value <- sqrt(mean((test_data$Fertility - predictions)^2))
MAE_value <- mean(abs(test_data$Fertility - predictions))
PER_value <- RMSE_value / mean(test_data$Fertility)

val_results <- data.frame(
  R2 = R2_value,
  RMSE = RMSE_value,
  MAE = MAE_value,
  PER = PER_value
)
val_results

############################################################
# (c)(i) LOOCV (Leave-One-Out Cross Validation Manual)
############################################################
n <- nrow(swiss)
errors <- c()

for (i in 1:n) {
  train_data <- swiss[-i, ]
  test_data  <- swiss[i, ]
  
  model <- lm(Fertility ~ ., data = train_data)
  pred <- predict(model, newdata = test_data)
  
  errors[i] <- (test_data$Fertility - pred)^2
}

LOOCV_RMSE <- sqrt(mean(errors))
LOOCV_RMSE

############################################################
# (c)(ii) 10-Fold Cross Validation Manual
############################################################
set.seed(123)
k <- 10
folds <- sample(rep(1:k, length.out = nrow(swiss)))
errors <- c()

for (i in 1:k) {
  train_data <- swiss[folds != i, ]
  test_data  <- swiss[folds == i, ]
  
  model <- lm(Fertility ~ ., data = train_data)
  pred <- predict(model, newdata = test_data)
  
  errors <- c(errors, (test_data$Fertility - pred)^2)
}

KFold_RMSE <- sqrt(mean(errors))
KFold_RMSE

############################################################
# (c)(iii) Repeated 10-Fold Cross Validation (3 Repeats)
############################################################
set.seed(123)
k <- 10
repeats <- 3
RMSE_values <- c()

for (r in 1:repeats) {
  folds <- sample(rep(1:k, length.out = nrow(swiss)))
  errors <- c()
  
  for (i in 1:k) {
    train_data <- swiss[folds != i, ]
    test_data  <- swiss[folds == i, ]
    
    model <- lm(Fertility ~ ., data = train_data)
    pred <- predict(model, newdata = test_data)
    
    errors <- c(errors, (test_data$Fertility - pred)^2)
  }
  
  RMSE_values[r] <- sqrt(mean(errors))
}

Repeated_KFold_RMSE <- mean(RMSE_values)
Repeated_KFold_RMSE

