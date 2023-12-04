install.packages("tidyverse") 
install.packages("caret")
install.packages("glmnet")
install.packages("ggplot2")

summary(review_data_small)
summary(user_data_small)

library(tidyverse)
library(caret)
library(glmnet)
library(randomForest)


load(file="yelp_review_small.Rda")
load(file="yelp_user_small.Rda")


Combined_Data <- merge(review_data_small, user_data_small, by = "user_id")





# This will create a new column in your data frame called 'dummy_variable'


predictors <- Combined_Data[, c('review_count', 'useful.y','funny.y','cool.y','fans','average_stars','compliment_hot','compliment_more','compliment_more','compliment_profile','compliment_cute','compliment_list','compliment_note','compliment_plain','compliment_cool','compliment_funny','compliment_writer','compliment_photos')]  

scaled_predictors <- scale(predictors)  # Standardize features
scaled_predictors_df <- as.data.frame(scaled_predictors)

predictors_matrix <- model.matrix(~ . - 1, data = scaled_predictors_df)



target <- Combined_Data$stars

set.seed(1)
test_indices <- sample(1:nrow(Combined_Data), 10000)
train_indices <- setdiff(1:nrow(Combined_Data), test_indices)


trainData <- scaled_predictors[train_indices, ]
trainTarget <- target[train_indices]
testData <- scaled_predictors[test_indices, ]
testTarget <- target[test_indices]

rf_model <- randomForest(x = trainData, y = trainTarget, ntree = 100)

rf_predictions <- predict(rf_model, testData)
mse <- mean((predictions - testTarget) ^ 2)
print(paste("Mean Squared Error:", mse))




set.seed(1)
test_indices <- sample(1:nrow(Combined_Data), 10000)
train_indices <- setdiff(1:nrow(Combined_Data), test_indices)


trainData <- predictors_matrix[train_indices, ]
trainTarget <- target[train_indices]
testData <- predictors_matrix[test_indices, ]
testTarget <- target[test_indices]

set.seed(1) # for reproducibility
cv.lasso <- cv.glmnet(predictors_matrix, target, alpha = 0.00002, thresh = 1e-12)
bestLambda <- cv.lasso$lambda.min


final_model <- glmnet(trainData, trainTarget, alpha = 1, lambda = 0.00002, thresh =1e-12)


predictions <- predict(final_model, s = 0.00002, newx = testData)

mse <- mean((predictions - testTarget) ^ 2)
print(paste("Mean Squared Error:", mse))

predictions <- as.vector(predictions)

actual_values <- testTarget

# Calculating Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Calculating R-squared
ss_total <- sum((actual_values - mean(actual_values)) ^ 2)
ss_residuals <- sum((actual_values - predictions) ^ 2)
r_squared <- 1 - (ss_residuals / ss_total)

print(paste("Mean Squared Error (MSE):", mse))
print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("R-squared:", r_squared))

rounded_predictions <- round(predictions)

is_accurate <- abs(rounded_predictions - testTarget) <= 0.5

# Calculate the accuracy as the proportion of accurate predictions
accuracy_score <- sum(is_accurate) / length(testTarget)

# Print the accuracy score
print(paste("Accuracy Score:", accuracy_score))



# Assuming you have your predictors matrix 'x' and target variable 'y'
cv_model <- cv.glmnet(predictors_matrix, target, alpha = 1)  # Alpha 1 for Lasso

# Explore different lambda values
plot(cv_model)
best_lambda <- cv_model$lambda.min
model_at_best_lambda <- glmnet(predictors_matrix, target, alpha = 1, lambda = best_lambda)

# You might also try a lambda that's less penalizing
lambda_less_penalizing <- cv_model$lambda.1se
model_at_lambda_less_penalizing <- glmnet(predictors_matrix, target, alpha = 1, lambda = lambda_less_penalizing)



# Extract model coefficients
coefficients_matrix <- as.matrix(coef(cv.lasso, s = 1))

# Convert to a data frame
coefficients_df <- as.data.frame(coefficients_matrix)

# If you want to remove the intercept and make the data frame more readable
coefficients_df <- coefficients_df[-1, , drop = FALSE]  # Remove intercept
names(coefficients_df) <- c("Coefficient")  # Rename the column

# Add row names as a new column (feature names)
coefficients_df$Feature <- rownames(coefficients_df)
rownames(coefficients_df) <- NULL

# View the coefficients data frame
print(coefficients_df)

print(best_lambda)
print(bestLambda)
print(lambda_less_penalizing)




library(ggplot2)


data_for_plot <- data.frame(Actual = actual_values, Predicted = predictions)


ggplot(data_for_plot, aes(x = Actual, y = Predicted)) +
  geom_point() +  
  geom_abline(color = "red", linetype = "dashed") +  
  ggtitle("Actual vs Predicted Values") +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  theme_minimal()

# Assuming you have a vector of actual values and predicted values
# actual_values <- ...
# predicted_values <- ...

# Calculate residuals
residuals <- actual_values - predictions

# Load necessary library for plotting
library(ggplot2)

# Create a dataframe for plotting
data_for_plot <- data.frame(Actual = actual_values, Residuals = residuals)

# Plot Residuals
ggplot(data_for_plot, aes(x = Actual, y = Residuals)) +
  geom_point() +  # Scatter plot
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +  # Line at y = 0
  ggtitle("Residual Plot") +
  xlab("Actual Values") +
  ylab("Residuals (Actual - Predicted)") +
  theme_minimal()



# Fit the model (example with Lasso regression)
set.seed(1)  # for reproducibility
cv_model <- cv.glmnet(x = trainData, y = trainTarget, alpha = 1)  # alpha = 1 for Lasso; use alpha = 0 for Ridge
train_lambda <- cv_model$lambda.min
model <- glmnet(x = trainData, y = trainTarget, alpha = 1, lambda = train_lambda)

train_predicted_values <- predict(model, s = train_lambda, newx = trainData)

# Calculate MSE
train_mse <- mean((train_predicted_values - trainTarget) ^ 2)

# Calculate R-squared
train_ss_total <- sum((trainTarget - mean(trainTarget)) ^ 2)
train_ss_residuals <- sum((trainTarget - train_predicted_values) ^ 2)
train_r_squared <- 1 - (train_ss_residuals / train_ss_total)

print(paste("MSE:", train_mse))
print(paste("R-squared:", train_r_squared))

train_residuals <- trainTarget - train_predicted_values


break_points <- seq(1, 5, by = 0.5)


train_data_for_plot <- data.frame(Actual = trainTarget, Predicted = as.vector(train_predicted_values))
train_data_for_plot$Train_Binned_Predicted <- cut(train_data_for_plot$Predicted, breaks = break_points)
ggplot(train_data_for_plot, aes(x = Train_Binned_Predicted, y = train_residuals)) +
  geom_boxplot() +
  ggtitle("Binned Residual Plot") +
  xlab("Predicted Value Bins") +
  ylab("Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8))


# Plotting residuals
plot(train_residuals, main = "Residuals Plot", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")



ggplot(train_data_for_plot, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(color = "blue", linetype = "dashed") +
  ggtitle("Actual vs Predicted Plot") +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  theme_minimal()




set.seed(1)  # for reproducibility
cv_model <- cv.glmnet(x = testData, y = testTarget, alpha = 1)  # alpha = 1 for Lasso; use alpha = 0 for Ridge
test_lambda <- cv_model$lambda.min
model <- glmnet(x = testData, y = testTarget, alpha = 1, lambda = test_lambda)

test_predicted_values <- predict(model, s = test_lambda, newx = testData)

# Calculate MSE
test_mse <- mean((test_predicted_values - testTarget) ^ 2)

# Calculate R-squared
test_ss_total <- sum((testTarget - mean(testTarget)) ^ 2)
test_ss_residuals <- sum((testTarget - test_predicted_values) ^ 2)
test_r_squared <- 1 - (test_ss_residuals / test_ss_total)

print(paste("MSE:", test_mse))
print(paste("R-squared:", test_r_squared))

test_residuals <- testTarget - test_predicted_values

break_points <- seq(1, 5, by = 0.5)


test_data_for_plot <- data.frame(Actual = testTarget, Predicted = as.vector(test_predicted_values))
test_data_for_plot$Test_Binned_Predicted <- cut(test_data_for_plot$Predicted, breaks = break_points)
ggplot(test_data_for_plot, aes(x = Test_Binned_Predicted, y = test_residuals)) +
  geom_boxplot() +
  ggtitle("Binned Residual Plot") +
  xlab("Predicted Value Bins") +
  ylab("Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8))

# Plotting residuals
plot(test_residuals, main = "Residuals Plot", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")


test_data_for_plot <- data.frame(Actual =testTarget, Predicted = as.vector(test_predicted_values))
ggplot(test_data_for_plot, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(color = "blue", linetype = "dashed") +
  ggtitle("Actual vs Predicted Plot") +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  theme_minimal()

print(abs(test_mse - train_mse))

print(abs(test_r_squared - train_r_squared))



library(ggplot2)

Plan_residuals <- actual_values - predictions

Plan_data_for_plot <- data.frame(Actual = actual_values, Predicted = as.vector(predictions))
ggplot(Plan_data_for_plot, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  ggtitle("Actual vs Predicted Values") +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  theme_minimal()

break_points <- seq(1, 5, by = 0.5)

Plan_data_for_plot$Binned_Predicted <- cut(Plan_data_for_plot$Predicted, breaks = break_points)
ggplot(Plan_data_for_plot, aes(x = Binned_Predicted, y = Plan_residuals)) +
  geom_boxplot() +
  ggtitle("Binned Residual Plot") +
  xlab("Predicted Value Bins") +
  ylab("Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8))




#Random Forest Model

install.packages("randomForest")


# Build the Random Forest model
rf_train_predictors <- trainData[, !(names(trainData) %in% c('stars'))]
rf_train_target <- trainTarget
rf_test_predictors <- testData[, !(names(testData) %in% c('stars'))]
rf_test_target <- testTarget

rf_model <- randomForest(x = rf_train_predictors, y = rf_train_target, ntree = 500)

rf_predictions <- predict(rf_model, predictors)

rf_mse <- mean((rf_predictions - testTarget) ^ 2)
print(paste("Mean Squared Error:", rf_mse))

# You can also calculate R-squared for the model
rf_ss_total <- sum((testTarget - mean(testTarget)) ^ 2)
rf_ss_residuals <- sum((testTarget - rf_predictions) ^ 2)
rf_r_squared <- 1 - (rf_ss_residuals / rf_ss_total)
print(paste("R-squared:", rf_r_squared))


set.seed(1)
test_indices <- sample(1:nrow(Combined_Data), 10000)
train_indices <- setdiff(1:nrow(Combined_Data), test_indices)


rf_trainData <- predictors_matrix[train_indices, ]
rf_trainTarget <- target[train_indices]
rf_testData <- predictors_matrix[test_indices, ]
rf_testTarget <- target[test_indices]






