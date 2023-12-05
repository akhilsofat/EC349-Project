#Set Working Directory

setwd("/Users/akhil/Documents/Economics/EC349/EC349 Project")

#Install Relevant Packages

install.packages("tidyverse") 
install.packages("caret")
install.packages("ggplot2")
install.packages("randomForest")

#Load relevant libraries

library(tidyverse)
library(caret)
library(ggplot2)
library(randomForest)

#Load Small Data

load(file="yelp_review_small.Rda")
load(file="yelp_user_small.Rda")

#Load positive and negative words for sentiment analysis

Positive_words <- readLines("/Users/akhil/Documents/Economics/EC349/EC349 Project/Positive Words.txt")
Negative_words <- readLines("/Users/akhil/Documents/Economics/EC349/EC349 Project/Negative Words.txt")

#Create a combined dataset, merging by user id

Combined_Data <- merge(review_data_small, user_data_small, by = "user_id")

rm(review_data_small, user_data_small) #remove original datasets for memory functionality