data("swiss")       # Load the data

n = nrow(swiss)
set.seed(123)
train_index = sample(1:n, n*0.8)

train_data = swiss[train_index, ]
test_data = swiss[-train_index, ]

# Fit a linear model on the training data
model = lm(Fertility ~ . , data = train_data)

# Make predictions on the test data
yhat = predict(model, newdata = test_data)
y = test_data$Fertility

# Evaluate the model performance using Mean Squared Error (MSE)
e = y - yhat
mse = mean(e^2)
mse



### LOOCV
data("swiss")       # Load the data
n = nrow(swiss)
mse_vec = c()

for(i in 1:n) {
  train_data = swiss[-i, ]
  test_data = swiss[i, ]
  model = lm(Fertility ~ ., data = train_data)
  yhat = predict(model, test_data)
  y = test_data$Fertility
  
  mse_vec[i] = (y - yhat)^2
}

mse = mean(mse_vec)
mse