#Set Working Directory

setwd("/Users/akhil/Documents/Economics/EC349/EC349 Project")

#Install Relevant Packages

install.packages("tidyverse") 
install.packages("caret")
install.packages("glmnet")
install.packages("ggplot2")
install.packages("randomForest")


#Call on relevant libraries

library(tidyverse)
library(caret)
library(glmnet)
library(ggplot2)
library(randomForest)

#Load Small Data

load(file="yelp_review_small.Rda")
load(file="yelp_user_small.Rda")

#Create a combined dataset, merging by user id

Combined_Data <- merge(review_data_small, user_data_small, by = "user_id")

#Create a vector of features that will be used for the regression; Convert to matrix for regression

Predictors <- Combined_Data[, c('review_count', 'useful.y','funny.y','cool.y','fans','average_stars','compliment_hot','compliment_more','compliment_more','compliment_profile','compliment_cute','compliment_list','compliment_note','compliment_plain','compliment_cool','compliment_funny','compliment_writer','compliment_photos')]  
Predictors_Matrix <- model.matrix(~ . - 1, data = Predictors)

#Define the target the regression is trying to predict

target <- Combined_Data$stars

#Split the Combined dataset into training and test data, doing the same for the target values
set.seed(1)
Test_Indices <- sample(1:nrow(Combined_Data), 10000)
Train_Indices <- setdiff(1:nrow(Combined_Data), Test_Indices)

TrainData <- Predictors_Matrix[Train_Indices, ]
TrainTarget <- target[Train_Indices]

TestData <- Predictors_Matrix[Test_Indices, ]
TestTarget <- target[Test_Indices]

#Determine the value of lambda

set.seed(1)
Lasso_Lambda <- cv.glmnet(Predictors_Matrix, target, alpha = 1, thresh = 1e-12)
Best_Lambda <- Lasso_Lambda$lambda.min

#Use the value of lambda in the LASSO model on the training data

Lasso_Model <- glmnet(TrainData, TrainTarget, alpha = 1, lambda = Best_Lambda, thresh =1e-12)

#Predict the model for the test data

Lasso_Predictions <- predict(Lasso_Model, s = Best_Lambda, newx = TestData)

#Evaluate the model using MSE, R-Squared, and an Accuracy Score

Lasso_MSE <- mean((Lasso_Predictions - TestTarget) ^ 2)

Lasso_Total_SS <- sum((TestTarget - mean(TestTarget))^2)
Lasso_Residual_SS <- sum((TestTarget - Lasso_Predictions) ^ 2)
R_Squared <- 1 - (Lasso_Residual_SS / Lasso_Total_SS)

Accurate <- abs(Lasso_Predictions - TestTarget) <= 0.25
Accuracy_Score <- sum(Accurate) / length(TestTarget)

print(paste("Mean Squared Error (MSE):", Lasso_MSE))
print(paste("R-squared:", R_Squared))
print(paste("Accuracy Score:", Accuracy_Score))


