set.seed(123)  # For reproducibility
n <- 1000
sigma <- 1
b <- c(67, -0.17, -0.26, -0.87, 0.10, 1.08)  # Coefficients

# Generate independent variables from specified distributions
Agriculture <- runif(n, min = 1, max = 100)
Examination <- runif(n, min = 1, max = 40)
Education <- runif(n, min = 1, max = 60)
Catholic <- runif(n, min = 1, max = 100)
Infant.Mortality <- runif(n, min = 1, max = 30)

e <- rnorm(n, mean = 0, sd = sigma)  # Error term

# Generate random variables for the linear regression model
Fertility <- b[1] + b[2] * Agriculture + b[3] * Examination +
  b[4] * Education + b[5] * Catholic +
  b[6] * Infant.Mortality + e

# Create a data frame
swiss_generated <- data.frame(
  Fertility,
  Agriculture,
  Examination,
  Education,
  Catholic,
  Infant.Mortality
)

swiss_generated


# Define training control for LOOCV
train.control <- trainControl(method = "LOOCV")

# Train the model using LOOCV
model.loocv<-train(Fertility ~., data = swiss_generated,
                   method = "lm", trControl= train.control)


model.loocv$results


# Define training control for k-fold CV
set.seed(123)  # For reproducibility
train.control <- trainControl(method = "cv", number = 10)

# Train the model using k-fold CV
model.kfold <- train(Fertility ~ ., data = swiss_generated,
                     method = "lm", trControl = train.control)

model.kfold$results


# Define training control for repeated k-fold CV
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# Train the model using repeated k-fold CV
model.repeated <- train(Fertility ~ ., data = swiss_generated,
                        method = "lm", trControl = train.control)


model.repeated$results

