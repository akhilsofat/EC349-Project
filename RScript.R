install.packages("tidyverse") 
library(tidyverse)


load(file="yelp_review_small.Rda")
load("yelp_user_small.Rda")

summary(review_data_small)
summary(user_data_small)

install.packages("caret")
library(caret)

set.seed(1)

n <- nrow(user_data_small)
testSize <- 10000

if (n <= testSize) {
  stop("The dataset is too small to extract 10,000 observations for the test set.")
}

testIndex <- sample(1:n, testSize, replace = FALSE)

testData <- user_data_small[testIndex, ]
trainData <- user_data_small[-testIndex, ]

install.packages("glmnet")

library(glmnet)
library(caret)

Combine_data <-merge(review_data_small,user_data_small, by = "user_id")


