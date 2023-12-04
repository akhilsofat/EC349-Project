#Set Working Directory

setwd("/Users/akhil/Documents/Economics/EC349/EC349 Project")

#Install Relevant Packages

install.packages("tidyverse") 
install.packages("caret")
install.packages("glmnet")
install.packages("ggplot2")

#Call on relevant libraries

library(tidyverse)
library(caret)
library(glmnet)
library(ggplot2)

#Load Small Data

load(file="yelp_review_small.Rda")
load(file="yelp_user_small.Rda")

#Create a combined dataset, merging by user id

Combined_Data <- merge(review_data_small, user_data_small, by = "user_id")





