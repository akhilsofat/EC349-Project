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

predictors <- Combined_Data[, c('review_count', 'useful.y','funny.y','cool.y','fans','average_stars','compliment_hot','compliment_more','compliment_more','compliment_profile','compliment_cute','compliment_list','compliment_note','compliment_plain','compliment_cool','compliment_funny','compliment_writer','compliment_photos')]  
predictors_matrix <- model.matrix(~ . - 1, data = scaled_predictors_df)

#Define the target the regression is trying to predict

target <- Combined_Data$stars





