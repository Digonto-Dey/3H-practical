cost <- c(2449, 2248, 3545, 789, 1619, 2079, 918, 1231, 3641, 4314, 2628, 3989,
          2308, 376, 5428, 2786, 2497, 5551, 5208, 5438)
weight <-c(90, 87, 38, 28, 28, 23, 21, 17, 27, 39, 34, 46, 80, 14, 48, 38, 73, 40, 44,
           41)


myData = data.frame(cost, weight)

ls_model = lm(cost ~ weight, data = myData)

myData$residuals = abs(ls_model$residuals)


LTS_step = function(data, alpha = 0.2, p =1) {
  n = nrow(data)
  h = round((1 - alpha)*n + (p+1)/2)
  
  # sort by smallest absolute residuals
  sort_data = data[order(data$residuals), ]
  
  # keep 1st h data
  trimmed_data = sort_data[1:h, ]
  
  
  # Fit new model
  model = lm(cost ~ weight, data = trimmed_data)
  
  trimmed_data$residuals = abs(model$residuals)
  list(model = model, data = trimmed_data)
}

# Iteratively apply LTS trimming steps
step1 = LTS_step(myData)
step2 = LTS_step(step1$data)
step3 = LTS_step(step2$data)
step4 = LTS_step(step3$data)
step5 = LTS_step(step4$data)
step6 = LTS_step(step5$data)

# Final Model
model_ls = ls_model
model_lts1 = step1$model
model_lts2 = step2$model
model_lts3 = step3$model
model_lts4 = step4$model
model_lts5 = step5$model
model_lts6 = step6$model


# Plot models
plot(weight, cost, main = "All model graph")
abline(model_ls, col=1)
abline(model_lts1, col=2)
abline(model_lts2, col=3)
abline(model_lts3, col=4)
abline(model_lts4, col=5)
abline(model_lts5, col=6)
abline(model_lts6, col=7)

legend("topright", c("LS", paste0("LTS", 1:6)), col = 1:7, lty = 1)