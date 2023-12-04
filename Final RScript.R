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
Lasso_Lambda <- cv.glmnet(Predictors_Matrix, target, alpha = 0, thresh = 1e-12)
Best_Lambda <- Lasso_Lambda$lambda.min

#Use the value of lambda in the LASSO model on the training data

Lasso_Model <- glmnet(TrainData, TrainTarget, alpha = 0, lambda = Best_Lambda, thresh =1e-12)

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

#Understanding which predictors are most relevant by analysing the regression coefficients

Coefficients_Matrix <- as.matrix(coef(Lasso_Model, s = 1))

# Convert to a data frame
Coefficients_df <- as.data.frame(Coefficients_Matrix)

Coefficients_df <- Coefficients_df[-1, , drop = FALSE]  # Remove intercept
names(Coefficients_df) <- c("Coefficient")

# Add row names as a new column (feature names)
Coefficients_df$Feature <- rownames(Coefficients_df)
rownames(Coefficients_df) <- NULL

# View the coefficients data frame
print(Coefficients_df)

#Scale the predictors to standardise them

Scaled_Predictors <- as.matrix(scale(Predictors))  # Standardize features

ScaledTrainData <- Scaled_Predictors[Train_Indices, ]
ScaledTestData <- Scaled_Predictors[Test_Indices, ]

ScaledTrainTarget <- scale(TrainTarget)
ScaledTestTarget <- scale(TestTarget)

set.seed(1)
Scaled_Lasso_Lambda <- cv.glmnet(Scaled_Predictors, target, alpha = 0, thresh = 1e-12)
Scaled_Best_Lambda <- Lasso_Lambda$lambda.min

Scaled_Lasso_Model <- glmnet(ScaledTrainData, ScaledTrainTarget, alpha = 0, lambda = Best_Lambda, thresh =1e-12)

#Predict the model for the test data

Scaled_Lasso_Predictions <- predict(Lasso_Model, s = Best_Lambda, newx = ScaledTestData)

Scaled_Lasso_MSE <- mean((Scaled_Lasso_Predictions - ScaledTestTarget) ^ 2)

Scaled_Lasso_Total_SS <- sum((ScaledTestTarget - mean(ScaledTestTarget))^2)
Scaled_Lasso_Residual_SS <- sum((ScaledTestTarget - Scaled_Lasso_Predictions) ^ 2)
Scaled_R_Squared <- 1 - (Scaled_Lasso_Residual_SS / Scaled_Lasso_Total_SS)

Scaled_Accurate <- abs(Scaled_Lasso_Predictions - ScaledTestTarget) <= 0.25
Scaled_Accuracy_Score <- sum(Scaled_Accurate) / length(ScaledTestTarget)

print(paste("Mean Squared Error (MSE):", Scaled_Lasso_MSE))
print(paste("R-squared:", Scaled_R_Squared))
print(paste("Accuracy Score:", Scaled_Accuracy_Score))





