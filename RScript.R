install.packages("tidyverse") 
library(tidyverse)


review_small_data <-load(file="yelp_review_small.Rda")
user_small_data <-load("yelp_user_small.Rda")

summary(review_data_small)
summary(user_data_small)
