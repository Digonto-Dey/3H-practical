install.packages(c("caret", "recipes", "ggplot2", "dplyr"))


library(caret)      # for computing cross-validation methods
data("swiss")       # Load the data
sample_n(swiss, 3)  # Display first 5 rows of the dataset

# a) Splitting the dataset into training (80%) and test (20%)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(swiss$Fertility, p = 0.8, list = FALSE)
train_data <- swiss[train_index, ]
test_data <- swiss[-train_index, ]


# b) Fit the model on the training data
model <- lm(Fertility ~ . , data = train_data)  # . means every other variable in the dataset
# Predict on the test data
predictions <- predict(model, newdata = test_data)

val_results = data.frame(
  R2 = R2(predictions, test_data$Fertility),
  RMSE = RMSE(predictions, test_data$Fertility),
  MAE = MAE(predictions, test_data$Fertility),
  PER = RMSE(predictions, test_data$Fertility) / mean(test_data$Fertility)
)

# R2:   R-squared
# RMSE: Root Mean Squared Error
# MAE:  Mean Absolute Error
# PER:  Prediction error rate
val_results



### c) i)
# Define training control for LOOCV
train.control <- trainControl(method = "LOOCV")

# Train the model using LOOCV
model.loocv <- train(Fertility ~., data = swiss,
                     method = "lm", trControl= train.control)


model.loocv$results




### c) ii)
# Define training control for k-fold CV
set.seed(123)  # For reproducibility
train.control <- trainControl(method = "cv", number = 10)

# Train the model using k-fold CV
model.kfold <- train(Fertility ~ ., data = swiss,
                     method = "lm", trControl = train.control)

model.kfold$results



### c) iii)

# Define training control for repeated k-fold CV
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# Train the model using repeated k-fold CV
model.repeated <- train(Fertility ~ ., data = swiss,
                        method = "lm", trControl = train.control)


model.repeated$results