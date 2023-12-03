install.packages("tidyverse") 
install.packages("caret")
install.packages("glmnet")

summary(review_data_small)
summary(user_data_small)

library(tidyverse)
library(caret)
library(glmnet)


load(file="yelp_review_small.Rda")
load(file="yelp_user_small.Rda")


Combined_Data <- merge(review_data_small, user_data_small, by = "user_id")



predictors <- Combined_Data[, c('useful.x', 'funny.x', 'cool.x','review_count','useful.y','funny.y','cool.y','fans','average_stars')]  

target <- Combined_Data$stars



predictors_matrix <- model.matrix(~ . - 1, data = predictors)


set.seed(1)
test_indices <- sample(1:nrow(Combined_Data), 10000)
train_indices <- setdiff(1:nrow(Combined_Data), test_indices)

trainData <- predictors_matrix[train_indices, ]
trainTarget <- target[train_indices]
testData <- predictors_matrix[test_indices, ]
testTarget <- target[test_indices]


set.seed(1) # for reproducibility
cv.lasso <- cv.glmnet(trainData, trainTarget, alpha = 1, family = "gaussian")
bestLambda <- cv.lasso$lambda.min


final_model <- glmnet(trainData, trainTarget, alpha = 1, lambda = bestLambda, family = "gaussian")


predictions <- predict(final_model, s = bestLambda, newx = testData)

mse <- mean((predictions - testTarget) ^ 2)
print(paste("Mean Squared Error:", mse))
