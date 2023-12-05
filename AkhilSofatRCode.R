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