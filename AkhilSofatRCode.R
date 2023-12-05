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

#Remove punctuation and special characters
Combined_Data$text <- gsub("[[:punct:]]","",Combined_Data$text)
Combined_Data$text <- gsub("[[:digit:]]","",Combined_Data$text)

#Create a function to count the sentiment words, which splits strings into individual words for matching

Sentiment_Count <- function(text, sentiment_words) {
  words <- unlist(strsplit(text, "\\s+"))  
  sum(words %in% sentiment_words)          
}

# Add sentiment count columns to the dataframe
Combined_Data$positive_count <- sapply(Combined_Data$text, Sentiment_Count, sentiment_words = Positive_words)
Combined_Data$negative_count <- sapply(Combined_Data$text, Sentiment_Count, sentiment_words = Negative_words)

factors <- c('positive_count','negative_count','review_count','useful.y','funny.y','cool.y','fans','average_stars','compliment_hot','compliment_more','compliment_more','compliment_profile','compliment_cute','compliment_list','compliment_note','compliment_plain','compliment_cool','compliment_funny','compliment_writer','compliment_photos')


