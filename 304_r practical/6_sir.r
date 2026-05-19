## U.S Air Force Data 
## Cost and Weight
cost <- c(2449, 2248, 3545, 789, 1619, 2079, 918, 1231, 3641, 4314, 2628, 3989, 
          2308, 376, 5428, 2786, 2497, 5551, 5208, 5438)
weight <-c(90, 87, 38, 28, 28, 23, 21, 17, 27, 39, 34, 46, 80, 14, 48, 38, 73, 40, 44, 
           41)


## Graph
plot(weight, cost, main = "U.S Air Force Data ")
abline(lm(cost~weight), col = "red") ## SL fit line

## create a data frame
myData <- data.frame(cost,weight)

## LS model Fit And Take Residuals
LS <- lm(cost ~ weight, data= myData)
errors <- LS$residuals
myData$residual <- errors
sort(errors)

## Graph of Residual
plot(weight, errors, main = "Residual plot")
## trimmed the data 
n = 20
p = 1
alpha = .20      
## 20% data trimmed
h = ((1-alpha)*n)+((p+1)/2)
## 3 large residual omit from data respectively
## trim data collection
t1.data <- myData[myData$residual < 2405, ] 
t1.data

## model fit with trim data
LTS_model_1 <- lm(cost ~ weight,data = t1.data)

## See the model cofficient 
LTS_model_1
##Graph
plot(weight, cost, main = "Frist LTS Model line ",
     col = "red")
abline(LTS_model_1, col = 4)
abline(lm(cost~weight), col = 6)


## again check residual omit h obs.
t1.data$residual <- LTS_model_1$residuals
n=17
alpha = .2
p=1
h = ((1-alpha)*n)+((p+1)/2) ## h =14.6 close to 15
# sort Residuals 
sort(t1.data$residual) ## sort residuals and omit 2 large residauls 

t2.data <- t1.data[t1.data$residual < 1857, ]
LTS_model_2 <- lm(cost ~ weight, data = t2.data ) 
## LTS 2 model
## trimmed three data
t3.data <- t2.data[t2.data$residual < 1655, ]
LTS_model_3 <- lm(cost ~ weight, data = t3.data ) 
## LTS 3 model
t3.data$residual <- LTS_model_3$residuals
sort(t3.data$residual)
## trimmed  four data
t4.data <- t3.data[t3.data$residual < 1406, ]
LTS_model_4 <- lm(cost ~ weight, data = t4.data ) 
## LTS 4 model
t4.data$residual <- LTS_model_4$residuals
sort(t4.data$residual)

## trim five data
t5.data <- t4.data[t4.data$residual < 1071.84893, ]
LTS_model_5 <- lm(cost ~ weight, data = t5.data ) 
## LTS 5 model
t4.data <- t3.data[t3.data$residual < 1406, ]
LTS_model_4 <- lm(cost ~ weight, data = t4.data ) 
## LTS 4 model
t4.data$residual <- LTS_model_4$residuals
sort(t4.data$residual)

## all model graph
plot(weight, cost, main = "All 
Model graph ")
abline(lm(cost~weight))
abline(LTS_model_1, col= 2)
abline(LTS_model_2, col= 3)
abline(LTS_model_3, col= 4)
abline(LTS_model_4, col= 5)
abline(LTS_model_5, col= 6)

